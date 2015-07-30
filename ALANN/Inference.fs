module Inference

    open System
    open System.Collections.Concurrent
    open Types
    open TruthFunctions
    open Utilities
    open Parameters
    open Trail

    type TaskBuffer = ConcurrentQueue<Task>
    let TaskBuffer = TaskBuffer()

    type TaskProcessor() =     
        static let getTerm =
            function
            | Question(_ as q)   -> q.Term
            | Judgement (_ as j) -> j.Term
            | Goal(_ as g)       -> g.Term
            | _ -> failwith "TaskProcessor.getTerm()() - Unexpected Sentence type"

        static member private Inference onePremiseInference twoPremiseInference q task (belief : Belief option) =
            let FilterTV (sentence) =
                match sentence with
                | Judgement {TV = tv} when tv.C >= Parameters.MINIMUM_CONFIDENCE -> true
                | Goal {DV = dv} when dv.C >= 0.1f  -> true
                | Question(_) -> true
                | _ -> false

            let rec CreateTasks  (q : ConcurrentQueue<Sentence>) (task : Task) belief result =
               if q.Count = 0 then result
               else
                    match q.TryDequeue() with
                    | true, s when FilterTV s -> 
                        let newTask = 
                            {P = task.P * 0.95f; 
                             S = s; 
                             Stamp = {Created = DateTime.Now.Ticks; 
                             Occurs = Tense.Eternal; 
                             SC = SyntacticComplexity (getTerm s); 
                             Origin = Origin.Derived; 
                             Trail = TrailFuncs.MakeTrail(belief, task)};}

                        newTask::CreateTasks q task belief result
                    | _ -> []

            let rec CreateTasks2  (q : ConcurrentQueue<Sentence>) (task : Task) result =
               if q.Count = 0 then result
               else
                    match q.TryDequeue() with
                    | true, s when FilterTV s -> 
                        let newTask = 
                            {P = task.P * 0.95f; 
                             S = s; 
                             Stamp = {Created = DateTime.Now.Ticks; 
                             Occurs = Tense.Eternal; 
                             SC = SyntacticComplexity (getTerm s); 
                             Origin = Origin.Derived; 
                             Trail = TrailFuncs.MakeTrail(task)}}

                        newTask::CreateTasks2 q task result
                    | _ -> []
                    
            let results1 (task : Task) = 
                onePremiseInference q task 

            let results2 (task : Task) (belief : Belief) = 
                match task, belief with
                | task, belief -> //when DistinctEvidence task.Stamp belief.Stamp ->                        
                    twoPremiseInference q task belief
                
            results1 task
            match belief with 
            | Some belief -> 
                results2 task belief
                let lst = CreateTasks q task belief []
                lst |> List.iter (fun t -> TaskBuffer.Enqueue(t))
            | None -> 
                let lst = CreateTasks2 q task []
                lst |> List.iter (fun t ->  TaskBuffer.Enqueue(t))
                
        static member DoInference task belief =  
            let sentenceQ = new ConcurrentQueue<Sentence>()

            match task, belief with
            | t, b -> 
//                 [TaskProcessor.Inference NALLevel9.OnePremiseInferenceRules NALLevel9.TwoPremiseInferenceRules sentenceQ t b;
//                  TaskProcessor.Inference NALLevel8.OnePremiseInferenceRules NALLevel8.TwoPremiseInferenceRules sentenceQ t b;
//                  TaskProcessor.Inference NALLevel7.OnePremiseInferenceRules NALLevel7.TwoPremiseInferenceRules sentenceQ t b;
//                  TaskProcessor.Inference NALLevel6.OnePremiseInferenceRules NALLevel6.TwoPremiseInferenceRules sentenceQ t b;
//                  TaskProcessor.Inference NALLevel5.OnePremiseInferenceRules NALLevel5.TwoPremiseInferenceRules sentenceQ t b;
//                  TaskProcessor.Inference NALLevel4.OnePremiseInferenceRules NALLevel4.TwoPremiseInferenceRules sentenceQ t b;
//                  TaskProcessor.Inference NALLevel3.OnePremiseInferenceRules NALLevel3.TwoPremiseInferenceRules sentenceQ t b;
//                  TaskProcessor.Inference NALLevel2.OnePremiseInferenceRules NALLevel2.TwoPremiseInferenceRules sentenceQ t b;
                  [TaskProcessor.Inference NALLevel1.OnePremiseInferenceRules NALLevel1.TwoPremiseInferenceRules sentenceQ t b]
                |> List.iter (fun f -> f)

