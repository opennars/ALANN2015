namespace Graph

open SymbolTable.SymbolTable

module Graph = 
    type Graph() =
        static member Save() =
            Global.LocalStorage.SaveStorage() |> ignore
        static member Reset() = 
            Global.LocalStorage.ResetStorage() |> ignore
            SymbolTable.get().Clear()
        static member Restore() =
            Global.LocalStorage.LoadStorage() |> ignore
            SymbolTable.restore()
        static member DisplayStats() =
            printfn "CellCount: \t\t%d" Global.LocalStorage.CellCount
            printfn "TotalCommittedMemory: \t%d" Global.LocalStorage.TotalCommittedMemory
            printfn "TotalCellSize: \t\t%d" Global.LocalStorage.TotalCellSize