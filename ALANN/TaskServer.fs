module TaskServer
    
    open System
    open System.Collections.Generic
    open Parameters
    open Concept
    open Types

    let linkTypeToTermType =
        function
        | LinkType.Inh -> TermType.Inh
        | LinkType.Sim -> TermType.Sim
        | LinkType.Imp -> TermType.Imp
        | LinkType.Equ -> TermType.Equ
        | LinkType.ConImp -> TermType.ConImp
        | LinkType.ConEqu -> TermType.ConEqu
        | LinkType.PreImp -> TermType.PreImp
        | LinkType.PreEqu -> TermType.PreEqu
        | LinkType.RetImp -> TermType.RetImp

    let taskRoutingHandler (request : TaskMessageReader) =
        // Get current time in ticks
        let ticks = DateTime.Now.Ticks

        let task = taskTable.[request.taskId]

        // Load concept that is target of spike
        use concept = Trinity.Global.LocalStorage.UseGraphConcept(request.target)

        // decay potential over time e^-lt
        let delta = float(ticks - concept.LastUpdated)
        let lambda = float(-concept.ActionPotential) * delta
        concept.ActionPotential <- single(float(concept.ActionPotential) * Math.Exp(lambda))
        concept.LastUpdated <- ticks

        // Update potential with task value
        concept.ActionPotential <- concept.ActionPotential + task.P

        if concept.ActionPotential > Parameters.ACTIVATION_THRESHOLD then
            // Concept activated
            // Reset concept
            concept.ActionPotential <- Parameters.ACTION_POTENTIAL_RESET
            concept.LastActivated <- ticks

            // Get links from concept

            // need to extract all relevant info from the concept in use before moving on
            // to target concepts so can dispose current concept

            let links = [for l in concept.Beliefs -> {LinkType = l.LinkType; TV = {F = l.TV.F; C = l.TV.C}; Target = l.Target; Stamp = l.Stamp}]
            
            // Extract beliefs from concept
            // do inference on each belief with task
            concept.Beliefs.ForEach (fun l ->
                // *** TODO Do inference here
                // build belief from concept and link
                let s = termTable.[concept.TermId]
                let p = termTable.[l.Target] // get target concept here *** TO DO
                let term = Term(linkTypeToTermType l.LinkType, [s; p])
                let stamp = {Created = l.Stamp.Created; Occurs = l.Stamp.Occurs; SC = l.Stamp.SC; Origin = l.Stamp.Origin; Trail = [ for t in l.Stamp.Trail -> t]}
                let belief = {J = {Term = term; TV = {F = l.TV.F; C = l.TV.C}}; Stamp = stamp}
                Inference.TaskProcessor.DoInference task (Some belief)
                )

    let create () =
        { new TaskServerBase() with
            member this.TaskRoutingHandler request = taskRoutingHandler request
        }
