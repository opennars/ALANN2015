module Trail

    open Types
    open Parameters

    type Id() =
        static let id = ref 0UL
        static member NextID = id:= !id + 1UL; !id 
        static member Reset = id := 0UL  

    type TrailFuncs() =
        static let rec merge p q =
             match p, q with
             | [], [] -> []
             | a, [] -> a
             | [], a -> a
             | x::xs, y::ys when x>=y -> x::merge xs q
             | x::xs, y::ys -> y::merge p ys

        static let removeDuplicates(lst : Trail) = 
            let f item acc =
                match acc with 
                | [] -> [item]
                | _ ->
                    match List.exists(fun x -> x = item) acc with
                    | false -> item :: acc
                    | true -> acc
            lst 
            |> List.rev
            |> fun x -> List.foldBack f x []
            |> List.rev

        static let capTrail(trail : Trail) = 
            let length = min Parameters.TRAIL_LENGTH (Seq.length trail)
            trail |> List.toSeq |> Seq.take length |> List.ofSeq
            
        static member MakeTrail(belief : Belief, task : Task) =
            let trail = Id.NextID::removeDuplicates(merge belief.Stamp.Trail  task.Stamp.Trail) 
            capTrail trail

        static member MakeTrail(task : Task) =
            let trail = Id.NextID::task.Stamp.Trail  
            capTrail trail

        static member MakeTrail() = [Id.NextID]