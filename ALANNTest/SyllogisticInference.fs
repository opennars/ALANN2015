module SyllogisticInference

open Types
open FirstOrderSyllogistic
open HigherOrderSyllogistic

let (|IsFirstOrder|_|) = 
    function 
    | Term(Inh, _), Term(Inh, _)
    | Term(Inh, _), Term(Sim, _)
    | Term(Sim, _), Term(Inh, _)
    | Term(Sim, _), Term(Sim, _) -> Some() | _ -> None

let (|IsHigherOrder|_|) = 
    function 
    | Term(Imp, _), Term(Imp, _)
    | Term(Imp, _), Term(Equ, _)
    | Term(Equ, _), Term(Imp, _)
    | Term(Equ, _), Term(Equ, _) -> Some() | _ -> None

let SyllogisticInference st1 st2 tv1 tv2 =
    match st1, st2 with
    | IsFirstOrder -> firstOrderSyllogisticInference st1 tv1 st2 tv2
    | IsHigherOrder -> higherOrderSyllogisticInference st1 tv1 st2 tv2
    | _ -> []