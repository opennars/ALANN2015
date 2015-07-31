module FirstOrderSyllogistic

open Types
open TruthFunctions
open InferenceRules
 
let (|Deduction1|Induction1|Abduction1|Exemplification1|Resemblance1|Other|) (t1, t2) =
    match t1, t2 with
    | Term(Inh, [m1; p]), Term(Inh, [s; m2]) when m1 = m2 && s <> p -> Deduction1(s, p)
    | Term(Inh, [m1; p]), Term(Inh, [m2; s]) when m1 = m2 && s <> p -> Induction1(s, p)
    | Term(Inh, [p; m1]), Term(Inh, [s; m2]) when m1 = m2 && s <> p -> Abduction1(s, p)
    | Term(Inh, [p; m1]), Term(Inh, [m2; s]) when m1 = m2 && s <> p -> Exemplification1(s, p)
    | Term(Sim, [m1; p]), Term(Sim, [s; m2]) when m1 = m2 && s <> p -> Resemblance1(s, p)
    | _ -> Other

let (|Analogy1a|Analogy1b|Analogy1c|Analogy1d|Other|) (t1, t2) =
    match t1, t2 with
    | Term(Sim, [m1; p]), Term(Inh, [s; m2]) when m1 = m2 && s <> p -> Analogy1a(s, p)
    | Term(Sim, [m1; p]), Term(Inh, [m2; s]) when m1 = m2 && s <> p -> Analogy1b(s, p)
    | Term(Inh, [m1; p]), Term(Sim, [s; m2]) when m1 = m2 && s <> p -> Analogy1c(s, p)
    | Term(Inh, [p; m1]), Term(Sim, [s; m2]) when m1 = m2 && s <> p -> Analogy1d(s, p)   
    | _ -> Other

let firstOrderSyllogisticInference st1 tv1 st2 tv2 =
    match st1, st2 with
    | Deduction1(s, p) -> [deduction Inh s p tv1 tv2;
                           exemplification Inh s p tv1 tv2]

    | Induction1(s, p) -> [induction Inh s p tv1 tv2;
                           induction Inh p s tv2 tv1;
                           comparison Sim s p tv1 tv2]

    | Abduction1(s, p) -> [abduction Inh s p tv1 tv2;
                           abduction Inh p s tv2 tv1;
                           comparison Sim s p tv2 tv1]
             
    | Resemblance1(s, p) -> [resemblance Sim s p tv1 tv2]    
    
    | Exemplification1(s, p) -> [exemplification Inh s p tv1 tv2;
                                 deduction Inh p s tv2 tv1]

    | Analogy1a(s, p) -> [analogy Inh s p tv2 tv1]

    | Analogy1b(s, p) -> [analogy Inh p s tv2 tv1]

    | Analogy1c(s, p) -> [analogy Inh s p tv1 tv2]

    | Analogy1d(s, p) -> [analogy Inh p s tv1 tv2]

    | _ -> []
