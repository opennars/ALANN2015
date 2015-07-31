module HigherOrderSyllogistic

open Types
open TruthFunctions
open InferenceRules

let (|DeductionH|InductionH|AbductionH|ExemplificationH|ResemblanceH|Other|) (t1, t2) =
    match t1, t2 with
    | Term(Imp, [m1; p]), Term(Imp, [s; m2]) when m1 = m2 && s <> p -> DeductionH(s, p)
    | Term(Imp, [m1; p]), Term(Imp, [m2; s]) when m1 = m2 && s <> p -> InductionH(s, p)
    | Term(Imp, [p; m1]), Term(Imp, [s; m2]) when m1 = m2 && s <> p -> AbductionH(s, p)
    | Term(Imp, [p; m1]), Term(Imp, [m2; s]) when m1 = m2 && s <> p -> ExemplificationH(s, p)
    | Term(Equ, [m1; p]), Term(Equ, [s; m2]) when m1 = m2 && s <> p -> ResemblanceH(s, p)
    | _ -> Other

let (|AnalogyHa|AnalogyHb|AnalogyHc|AnalogyHd|Other|) (t1, t2) =
    match t1, t2 with
    | Term(Equ, [m1; p]), Term(Imp, [s; m2]) when m1 = m2 && s <> p -> AnalogyHa(s, p)
    | Term(Equ, [m1; p]), Term(Imp, [m2; s]) when m1 = m2 && s <> p -> AnalogyHb(s, p)
    | Term(Imp, [m1; p]), Term(Equ, [s; m2]) when m1 = m2 && s <> p -> AnalogyHc(s, p)
    | Term(Imp, [p; m1]), Term(Equ, [s; m2]) when m1 = m2 && s <> p -> AnalogyHd(s, p)   
    | _ -> Other

let higherOrderSyllogisticInference st1 tv1 st2 tv2 =
    match st1, st2 with
    | DeductionH(s, p) -> [deduction Imp s p tv1 tv2;
                           exemplification Imp s p tv1 tv2]

    | InductionH(s, p) -> [induction Imp s p tv1 tv2;
                           induction Imp p s tv2 tv1;
                           comparison Equ s p tv1 tv2]

    | AbductionH(s, p) -> [abduction Imp s p tv1 tv2;
                           abduction Imp p s tv2 tv1;
                           comparison Equ s p tv2 tv1]
             
    | ResemblanceH(s, p) -> [resemblance Equ s p tv1 tv2]    
    
    | ExemplificationH(s, p) -> [exemplification Imp s p tv1 tv2;
                                 deduction Imp p s tv2 tv1]

    | AnalogyHa(s, p) -> [analogy Imp s p tv2 tv1]

    | AnalogyHb(s, p) -> [analogy Imp p s tv2 tv1]

    | AnalogyHc(s, p) -> [analogy Imp s p tv1 tv2]

    | AnalogyHd(s, p) -> [analogy Imp p s tv1 tv2]

    | _ -> []
