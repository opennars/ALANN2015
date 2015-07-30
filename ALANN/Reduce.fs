module Reduce
    open Types

    let rec reduce (st : Term) =
        match st with
        | Term(ExtInt, [t]) -> t
        | Term(IntInt, [t]) -> t
        | Term(ExtInt, [Term(ExtSet, l1); Term(ExtSet, l2)]) when List.isEmpty (Set.intersect (set l1) (set l2) |> Set.toList) <> true -> 
            Term(ExtSet, Set.intersect (set l1) (set l2) |> Set.toList)
        | Term(ExtInt, [Term(ExtInt, l1); Term(ExtInt, l2)]) -> Term(ExtInt, Set.union (set l1) (set l2) |> Set.toList)
        | Term(ExtInt, [Term(ExtInt, l1); l2]) -> Term(ExtInt, Set.union (set l1) (set [l2]) |> Set.toList)
        | Term(ExtInt, [l1; Term(ExtInt, l2)]) -> Term(ExtInt, Set.union (set [l1]) (set l2) |> Set.toList)
        | Term(ExtInt, [Term(IntSet, l1); Term(IntSet, l2)]) -> Term(IntSet, Set.union (set l1) (set l2) |> Set.toList)
        | Term(IntInt, [Term(IntInt, l1); Term(IntInt, l2)]) -> Term(IntInt, Set.union (set l1) (set l2) |> Set.toList)
        | Term(IntInt, [Term(IntInt, l1); l2]) -> Term(IntInt, Set.union (set l1) ([l2] |> Set.ofList) |> Set.toList)
        | Term(IntInt, [l1; Term(IntInt, l2)]) -> Term(IntInt, Set.union ([l1] |> Set.ofList) (set l2) |> Set.toList)
        | Term(IntInt, [Term(IntSet, l1); Term(IntSet, l2)]) -> Term(IntSet, Set.union (set l1) (set l2) |> Set.toList)
        | Term(IntInt, [Term(ExtSet, l1); Term(ExtSet, l2)]) -> Term(ExtSet, Set.union (set l1) (set l2) |> Set.toList)
        | Term(ExtDif, [Term(ExtSet, l1); Term(ExtSet, l2)]) -> Term(ExtSet, Set.difference (set l1) (set l2) |> Set.toList)
        | Term(IntDif, [Term(IntSet, l1); Term(IntSet, l2)]) -> Term(IntSet, Set.difference (set l1) (set l2) |> Set.toList)
        | Term(Sim, [Term(ExtSet, [s]); Term(ExtSet, [p])]) -> Term(Sim, [s; p])
        | Term(Sim, [Term(IntSet, [s]); Term(IntSet, [p])]) -> Term(Sim, [s; p])
        | Term(Prod, [Term(Prod, lst); t]) -> Term(Prod, lst @ [t])

        | Term(ExtImg, [Term(Prod, [t1; t2]); t3]) when t2 = t3 && t1 <> t2 -> t1
        | Term(IntImg, [Term(Prod, [t1; t2]); t3]) when t2 = t3 && t1 <> t2 -> t1

        | Term(Not, [Term(Not, [s])]) -> s
        | Term(And, [t]) -> t
        | Term(Or, [t]) -> t
        | Term(Or, [t1; t2]) when t1 = t2 -> reduce t2
        | Term(And, [t1; t2]) when t1 = t2 -> reduce t1
        | Term(And, [Term(And, l1); Term(And, l2)]) -> Term(And, Set.union (set l1)(set l2) |> Set.toList)
        | Term(And, [Term(And, l1); l2]) -> Term(And, Set.union (set l1)([l2] |> Set.ofList) |> Set.toList)
        | Term(And, [l1; Term(And, l2)]) -> Term(And, Set.union ([l1] |> Set.ofList)(set l2) |> Set.toList)
        | Term(Or, [Term(Or, l1); Term(Or, l2)]) -> Term(Or, Set.union (set l1)(set l2) |> Set.toList)
        | Term(Or, [Term(Or, l1); l2]) -> Term(Or, Set.union (set l1)([l2] |> Set.ofList) |> Set.toList)
        | Term(Or, [l1; Term(Or, l2)]) -> Term(Or, Set.union ([l1] |> Set.ofList)(set l2) |> Set.toList)
        | _ -> st
