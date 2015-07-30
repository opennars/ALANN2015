open System
open System.Collections.Concurrent
open Akka.Actor
open Akka.FSharp
open Types

type Message =
    | Spike of single
    | Task of Task
    | Belief of Belief
    | LastActivated

type ConceptState = {Name : Term; Activation : single; LastActivation : int64; LastUpdate : int64; LastTaskRx : Task option; Beliefs : Belief list}

[<EntryPoint>]
let main argv = 
    
    let system = AkkaSystem.AkkaSystem.System
    let conceptStore = ConcurrentDictionary<Term, IActorRef>(16, 100000)

    let calcRetentionValue belief =
        let created = belief.Stamp.Created
        let sc = belief.Stamp.SC
        let c = belief.J.TV.C

        // calculate retention value & return it
        (1.0f / single(sc)) + (1.0f / single(created)) + c

    // not tail recursive
    let rec remove n lst = 
        match lst with
        | h::tl when h = n -> tl
        | h::tl -> h :: (remove n tl)
        | []    -> []

    let getTerms = 
        function
        | Term(_, [s; p]) as term -> [s; p; term] 
        | Constant s as constant -> [constant]
        | _ -> []

    let getConceptFromTerm term = conceptStore.[term]

    let isOutgoingBelief belief state = 
        match getTerms belief with
        | h :: tail when h = state.Name -> true
        | _ -> false

    let isConceptActive term =
        let ACTIVATION_WINDOW = 1000L // ticks
        let lastActivatedAsync = getConceptFromTerm term <? LastActivated
        let response = int64(Async.RunSynchronously(lastActivatedAsync, 1))
        match DateTime.Now.Ticks - response with
        | time when time < ACTIVATION_WINDOW -> true
        | _ -> false           

    let isActiveBelief belief state =
        let terms = getTerms belief.J.Term
        match terms with
        | src :: dst :: _ when src = state.Name -> isConceptActive dst
        | src :: dst :: _ when dst = state.Name -> isConceptActive src
        | _ -> failwith "IsActiveBelief: No matching concept"

    let doInference task belief = ()

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

    let handleTask task state = 
        {Name = state.Name; Activation = 0.0f; LastActivation = 0L; LastUpdate = 0L; LastTaskRx = None; Beliefs = []}

    let handleBelief belief state = 
        // if input belief spike

        // update belief links with new belief
        // find lowest retention value
        let created = belief.Stamp.Created
        let sc = belief.Stamp.SC
        let c = belief.J.TV.C

        // update retention value
        belief.Stamp.Retention <- calcRetentionValue belief

        // if list is beyond capacity remove lowest retention value
        let beliefs = 
            if state.Beliefs |> List.length > 20 then
                let min = state.Beliefs |> List.minBy (fun b -> b.Stamp.Retention)
                state.Beliefs |> remove min
            else
                state.Beliefs

        // add belief to list
        let beliefs = belief :: state.Beliefs

        // if input belief then spike at end so it gets included in spike propogation *** TODO Not tested for yet
        state.Beliefs |> List.iter 
            (fun belief -> 
                getTerms belief.J.Term |> List.iter
                    (fun term ->
                        getConceptFromTerm term <! Spike 1 // send spike
                    )
            )

        {Name = state.Name; Activation = 0.0f; LastActivation = 0L; LastUpdate = 0L; LastTaskRx = None; Beliefs = beliefs}

    let handleMessage (mailbox : Actor<'a>) msg state =
        match msg with
        | Spike(spike) -> handleSpike spike state
        | Task(task) -> handleTask task state
        | Belief(belief) -> handleBelief belief state
        | LastActivated -> 
            mailbox.Sender() <! state.LastActivation
            state

    let createConcept term =
        spawn system (term.ToString())
            (fun mailbox ->
                let rec loop(state) = actor {
                    let! msg = mailbox.Receive()
                    let state = handleMessage mailbox msg state
                    return! loop(state)
                }
                loop({Name = term; Activation = 0.0f; LastActivation = 0L; LastUpdate = 0L; LastTaskRx = None; Beliefs = []}))

    let conceptStore = ConcurrentDictionary<int, IActorRef>(16, 100000)

    let N = 100000
    printfn "Creating %d concepts..." N

    for i in 1 .. N do
        if i % 10000 = 0 then printfn "Created %d concepts..." i
        conceptStore.TryAdd(i, createConcept (Constant ("Concept" + i.ToString()))) |> ignore

    printfn "Concepts created"
    Console.ReadLine() |> ignore

    0 // return an integer exit code
