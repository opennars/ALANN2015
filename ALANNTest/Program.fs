open System
open System.Collections.Concurrent
open Akka.Actor                 // AKKA.NET Framework
open Akka.FSharp                // AKKA.NET Framework F# Extensions

open Types                      // ALANN Type System
open TermPrinter                // Pretty Printer for Terms

// Messages that drive concept behaviour
type Message =
    | Spike of single           // Async - Sent to drive spiking behaviour, the spike value should be a system parameter
    | Task of Task              // Async - Sent to drive Task behaviour, the parameter is a task
    | Belief of Belief          // Async - Sent to drive Belief behaviour
    | LastActivated             // Sync  - Requests a concepts last activated time - used to test for active beliefs  

// Record to represent concept state
type ConceptState = 
    {Name : Term;               // The Concept is 'named' by its contained Term
    Activation : single;        // The accumulated level of Activation
    LastActivation : int64;     // The time, in Ticks, when the concept was last activated
    LastUpdate : int64;         // The time, in Ticks, when the Activation level was last decayed
    LastTaskRx : Task option;   // The last Task that was received - can be None, so is option type
    Beliefs : Belief list}      // List of beliefs, contains incoming and outgoing, not efficient at all!!!

// Prototype test frame only
[<EntryPoint>]
let main argv = 
    
    // Setup Akka.net actor system
    let system = AkkaSystem.AkkaSystem.System
    // Create HashTable to enable fast lookup of concept actors from their 'term' name
    let conceptStore = ConcurrentDictionary<Term, IActorRef>(16, 100000)

    // Utility functions
    // CalcRetentionValue is used to identify which belief should be removed
    // it is a function of creation time, syntactic complexity and truth confidence
    let calcRetentionValue belief =
        let created = belief.Stamp.Created
        let sc = belief.Stamp.SC
        let c = belief.J.TV.C

        // calculate retention value & return it
        (1.0f / single(sc)) + (1.0f / single(created)) + c


    // Function to remove element from list
    // Used to make space when belief list reaches capacity
    // not tail recursive
    let rec remove n lst = 
        match lst with
        | h::tl when h = n -> tl
        | h::tl -> h :: (remove n tl)
        | []    -> []

    // Function to retrieve the 'top' level terms form a term
    // it returns a list of terms
    // So a --> b, return [a; b; a --> b]
    let getTerms = 
        function
        | Term(_, [s; p]) as term -> [s; p; term] 
        | Constant s as constant -> [constant]
        | _ -> []

    // Function to retrieve a concept ActorRef from its 'term' name
    // The ActorRef can be used directly for sending messages to the concepts
    let getConceptFromTerm term = conceptStore.[term]

    // Function to test if a belief is outgoing
    // i.e. if the concept name = 'a'
    // and the belief is 'a --> b' then it is outgoing
    // otherwise it is incoming
    let isOutgoingBelief belief state = 
        match getTerms belief with
        | h :: tail when h = state.Name -> true
        | _ -> false

    // Function to test if 'another' concept is active
    // This is a synchronous message send which waits for a response, with a timeout
    // This is a quick and dirty approach to show the concept
    // The function returns wether or not the target concept is active
    // Because activation is a momentary moment it uses a time window
    // as a range. This is not calibrated.
    // The functions returns true if it was active within the time window
    // Timeouts are not currently handled
    let isConceptActive term =
        let ACTIVATION_WINDOW = 1000L // ticks
        let lastActivatedAsync = getConceptFromTerm term <? LastActivated
        let response = int64(Async.RunSynchronously(lastActivatedAsync, 1)) // 1ms timeout
        match DateTime.Now.Ticks - response with
        | time when time < ACTIVATION_WINDOW -> true
        | _ -> false           

    // Function to test if a belief is currently active
    // where an active belief is a belief thats src and dst 
    // concepts are simultaineously active
    // within a small time window
    let isActiveBelief belief state =
        let terms = getTerms belief.J.Term
        match terms with
        | src :: dst :: _ when src = state.Name -> isConceptActive dst
        | src :: dst :: _ when dst = state.Name -> isConceptActive src
        | _ -> failwith "IsActiveBelief: No matching concept"

    // Dummy placeholder function
    let doInference task belief = ()

    // Function called after a concept is activated
    // Sends spikes to outgoing beliefs
    // and calls inference on active beliefs
    let activateConcept state =
        // fire spikes to all outgoing beliefs
        state.Beliefs 
            |> List.filter (fun belief -> isOutgoingBelief belief.J.Term state)
            |> List.iter 
                (fun belief -> 
                    match getTerms belief.J.Term with
                    | _ :: dst :: _ -> getConceptFromTerm dst <! Spike 1.0f
                    | _ -> ()
                )

        // do inference on all 'active beliefs'
        // an active belief is a belief thats src and dst 
        // concepts are simultaineously active
        // within a small time window

        // very rough algorithm
        // iterate each belief
        // check dst or src concept for last activation
        // if within window allowed then do inference
        state.Beliefs 
            |> List.filter (fun belief -> isActiveBelief belief state)
            |> List.iter (fun belief -> doInference state.LastTaskRx belief)            

    // Spike message handling function
    // Responsible for implementing spike behaviour
    // An updated concept state is returned
    let handleSpike spike state = 
        let THRESHOLD = 5.0f
         
        // decay activation
        let activation = state.Activation * 1.0f // exp decay func goes here

        // add spike to activation
        let activation = activation + spike

        // check for firing
        if activation > THRESHOLD then
            activateConcept state

               
        {Name = state.Name; Activation = activation; LastActivation = 0L; LastUpdate = 0L; LastTaskRx = None; Beliefs = []}

    // Task message handling function
    // Responsible for implementing task behaviour
    // An updated concept state is returned
    let handleTask task state = 
        {Name = state.Name; Activation = 0.0f; LastActivation = 0L; LastUpdate = 0L; LastTaskRx = None; Beliefs = []}

    // Belief message handling function
    // Responsible for implementing belief behaviour
    // An updated concept state is returned
    let handleBelief belief state = 
        // if input belief spike

        // update belief links with new belief
        // find lowest retention value
        let created = belief.Stamp.Created
        let sc = belief.Stamp.SC
        let c = belief.J.TV.C

        // if list is beyond capacity remove lowest retention value
        let beliefs = 
            if state.Beliefs |> List.length > 20 then
                let min = state.Beliefs |> List.minBy (fun b -> calcRetentionValue b)
                state.Beliefs |> remove min
            else
                state.Beliefs

        // add belief to list
        let beliefs = belief :: state.Beliefs

        // if input belief then spike at end so it gets included in spike propogation 
        // *** TODO Not tested for yet - currently spikes on all beliefs, this maybe ok
        state.Beliefs |> List.iter 
            (fun belief -> 
                getTerms belief.J.Term |> List.iter
                    (fun term ->
                        getConceptFromTerm term <! Spike 1.0f // send spike
                    )
            )

        {Name = state.Name; Activation = 0.0f; LastActivation = 0L; LastUpdate = 0L; LastTaskRx = None; Beliefs = beliefs}

    // Top level message handling function
    // Responsible for routing specific messages to their respective handlers
    // An updated concept state is returned
    let handleMessage (mailbox : Actor<'a>) msg state =
        match msg with
        | Spike(spike) -> handleSpike spike state
        | Task(task) -> handleTask task state
        | Belief(belief) -> handleBelief belief state
        | LastActivated -> 
            mailbox.Sender() <! state.LastActivation
            state

    // The following code is a test framwork only
    // For Test purposes only

    // Function to generate a concept Actor
    let createConcept term =
        spawn system  (TermPrinter.ToString(term))
            (fun mailbox ->
                let rec loop(state) = actor {
                    let! msg = mailbox.Receive()
                    let state = handleMessage mailbox msg state
                    return! loop(state)
                }
                loop({Name = term; Activation = 0.0f; LastActivation = 0L; LastUpdate = 0L; LastTaskRx = None; Beliefs = []}))

    // Create ActorRef lookup HashTable
    let conceptStore = ConcurrentDictionary<int, IActorRef>(16, 100000)

    // Gnerate N concepts
    // Currently, way too slow!!!!
    // Need different approach
    // But ok for showing principles
    let N = 10000
    printfn "Creating %d concepts..." N

    for i in 1 .. N do
        if i % 1000 = 0 then printfn "Created %d concepts..." i
        conceptStore.TryAdd(i, createConcept (Constant ("Concept" + i.ToString()))) |> ignore

    printfn "Concepts created"
    Console.ReadLine() |> ignore

    0 // return an integer exit code
