namespace SymbolTable

open System.Collections.Concurrent
open Types

module SymbolTable = 
    type SymbolTable() =
        static let symbolTable = ConcurrentDictionary<Term, int64>(16, 1000000)
        static member contains term = symbolTable.ContainsKey term
        static member getCellId term = symbolTable.[term]
        static member AddCellId term cellid = symbolTable.TryAdd(term, cellid)
        static member get() = symbolTable
        static member restore() = 
//            symbolTable.Clear()
//            for n in Global.LocalStorage. do
//                symbolTable.TryAdd(n.Name, n.CellID) |> ignore
            ()
