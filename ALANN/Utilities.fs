module Utilities
    open Types

    let order lst = lst |> List.sort
    let contains t lst = Set.isProperSubset (Set [t]) (Set lst)
    let remove t lst = lst |> List.filter (fun i -> i <> t)
    let difference l1 l2 = Set.difference (set l1) (set l2) |> Set.toList

    let rec SyntacticComplexity st =
        match st with
        | Term(_, lst) -> 1 + List.fold (fun sum (t) -> sum + SyntacticComplexity(t)) 0 lst
        | Constant(_) -> 1

    let calcRetentionValue created sc c =
        // calculate retention value & return it
        (1.0f / single(sc)) + (1.0f / single(created)) + c