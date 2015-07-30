module TermPrinter

open Types

type TermPrinter(term) =
    static member ToString(term) =
        let rec tp term =             

            let separateList lst sep =
                match lst with
                | [] -> sep + sep   // *** TO DO *** find cause of empty list
                | _ -> List.reduce (fun x y -> x + sep + y) (List.map (fun x -> tp x) lst )

            match term with
            | Term(Not, lst) -> "--" + separateList lst " "
            | Term(And, lst) -> "(" + separateList lst " && " + ")"
            | Term(Or, lst) -> "(" + separateList lst " || " + ")"
            | Term(Imp, [s; p]) -> "(" + tp s + " ==> " + tp p + ")"
            | Term(PreImp, [s; p]) -> "(" + tp s + " =/> " + tp p + ")"
            | Term(ConImp, [s; p]) -> "(" + tp s + " =|> "+ tp p + ")"
            | Term(RetImp, [s; p]) -> "(" + tp s + " =|> " + tp p + ")"
            | Term(Equ, [s; p]) -> "(" + tp s + " <=> " + tp p + ")"
            | Term(ConEqu, [s; p]) -> "(" + tp s + " <|> "+ tp p + ")"
            | Term(PreEqu, [s; p]) -> "(" + tp s + " </> " + tp p + ")"
            | Term(Inh, [s; p]) -> "(" + tp s + " --> " + tp p + ")"
            | Term(Sim, [s; p]) -> "(" + tp s + " <-> " + tp p + ")"
            | Term(Operator, lst) -> "^(, " + separateList lst " " + ")"
            | Term(ExtSet, lst) -> "{" + separateList lst " " + "}"
            | Term(IntSet, lst) -> "[" + separateList lst " " + "]"
            | Term(ExtInt, lst) -> "(" + separateList lst " & " + ")"
            | Term(IntInt, lst) -> "(" + separateList lst " | " + ")"
            | Term(ExtDif, [a; b]) -> "(" + tp a + " - " + tp b + ")"
            | Term(IntDif, [a; b]) -> "(" + tp a + " ~ " + tp b + ")"
            | Term(Prod, lst) -> "(" + separateList lst " * " + ")"
            | Term(Par, lst) -> "(" + separateList lst "; " + ")"
            | Term(Seq, lst) -> "(" + separateList lst ", " + ")"
            | Term(ExtImg, lst) -> "/(" + separateList lst " " + ")"
            | Term(IntImg, lst) -> "\\(" + separateList lst " " + ")"
            | Constant(c) -> c.ToString()
            | Term(IVar, [c]) -> "$" + c.ToString()
            | Term(DVar, [c]) -> "#" + c.ToString()
            | Term(QVar, [c]) -> "?" + c.ToString()
            | _ -> "Unknown type Error"

        tp term