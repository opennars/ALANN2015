module NALLevel1

    open System
    open System.Collections.Generic
    open System.Collections.Concurrent
    open TruthFunctions
    open Types
    open Reduce
    open Utilities
    open System.Diagnostics.Contracts

    let ImmediateInference (q : ConcurrentQueue<Sentence>) (task : Task) =   
        try
            match task.S with 
            | Judgement {Term = st; TV = tv} -> 
                match st with
                | Term(Inh, [s; p]) 
                    when s <> p -> 
                        q.Enqueue <| Judgement {Term = Term(Inh, [p; s]); TV = f_cnv(tv)}
                | _ -> ()

            | Question {Term = st} -> 
                match st with
                | Term(Inh, [s; p]) 
                    when s <> p -> 
                        q.Enqueue <| Question {Term = Term(Inh, [p; s])}
                | _ -> ()
            | _ -> ()
        with
        | _ as ex -> printfn "Exception in NAL1.ImmediateInference: %s" ex.Message

    let syllogisticInference (q: ConcurrentQueue<Sentence>) (task : Task) (belief : Belief) =
        try
            let firstOrderInference (st1, st2, tv1, tv2) =
                match st1, st2 with
                //
                // NAL1
                //
                // M --> P, S --> M:- S --> P, P --> S
                | Term(Inh, [m1; p]), Term(Inh, [s; m2]) 
                    when m1 = m2 && s <> p -> 
                        [(Term(Inh,[s; p]), f_ded(tv1, tv2)); (Term(Inh, [p; s]), f_exe(tv2, tv1))]

                // M --> P, M --> S :- S --> P, P --> S
                // M --> P, M --> S :- S <-> P   
                | Term(Inh, [m1; p]), Term(Inh, [m2; s]) 
                    when m1 = m2 && s <> p -> 
                        [(Term(Inh, [s; p]), f_ind(tv1, tv2)); 
                         (Term(Inh, [p; s]), f_ind(tv2, tv1));
                         (reduce (Term(Sim, [s; p])), f_com(tv1, tv2))]

                // P --> M, S --> M :- S --> P, P --> S
                // P --> M, S --> M :- S <-> P              
                | Term(Inh, [p; m1]), Term(Inh, [s; m2]) 
                    when m1 = m2 && s <> p -> 
                        [(Term(Inh, [s; p]), f_abd(tv1, tv2)); 
                         (Term(Inh, [p; s]), f_abd(tv2, tv1));
                         (reduce (Term(Sim, [s; p])), f_com(tv2, tv1))]

                // P --> M, M --> S :- S --> P, P --> S
                | Term(Inh, [p; m1]), Term(Inh, [m2; s]) 
                    when m1 = m2 && s <> p -> 
                        [(Term(Inh, [s; p]), f_exe(tv1, tv2)); 
                         (Term(Inh, [p; s]), f_ded(tv2, tv1))]
                | _, _ -> []

            match task.S, belief.J with
            // forward inference
            | Judgement {Term = st1; TV = tv1}, {Term = st2; TV = tv2} ->
                firstOrderInference (st1, st2, tv1, tv2)|> List.iter (fun s -> q.Enqueue(Judgement {Term = fst s; TV = snd s}))
            // backward inference
            | Question {Term = st1}, {Term = st2; TV = tv2} ->
                List.iter (fun s -> q.Enqueue(Question {Term = fst s})) (firstOrderInference (st1, st2, tv2, tv2))
            | _, _ -> ()    
        with
        | _ as ex -> printfn "Exception in NAL1.syllogisticInference: %s" ex.Message

    let OnePremiseInferenceRules q task =
        ImmediateInference q task 

    let TwoPremiseInferenceRules q task1 task2 = 
        syllogisticInference q task1 task2
