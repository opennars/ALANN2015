module  Parser
    open FParsec
    open Types
    open Reduce
    open Utilities
    open System.Collections.Generic

    let vMap = new Dictionary<Term, int>()
    let mutable vNum = 0
    let pterm, ptermRef = createParserForwardedToRef<Term, unit>() 
    let pstatement, pstatementRef = createParserForwardedToRef<Term, unit>()   

    let str s = pstring s
    let pcomment = str "**" >>. skipRestOfLine true
    let ws = skipSepBy spaces pcomment

    let str_ws s = pstring s .>> ws

    let listBetweenStrings sOpen sClose pElement f =
        between (str sOpen) (str sClose)
            (ws >>. many1 (pElement .>> ws) |>> (fun lst -> Term(f, lst)))

    let setBetweenStrings sOpen sClose pElement f =
        between (str sOpen) (str sClose)
            (ws >>. many1 (pElement .>> ws)  |>> List.sort |>> (fun lst -> Term(f, lst)))

    let pfloat_ws = pfloat .>> ws
    let pstringliteral =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2 isIdentifierFirstChar isIdentifierChar 
        |>> fun i -> Constant(i)            

    let pstringliteral_ws = pstringliteral .>> ws 
    let pconstant  = pstringliteral_ws
    let pConstant = pconstant 
    let renameVar v = if vMap.ContainsKey(v) then (vMap.[v]).ToString() else vNum <- vNum + 1; vMap.Add(v, vNum); vNum.ToString()
    let pivar = str_ws "$" >>. pconstant |>> fun v -> Term(IVar, [Constant (renameVar v)])
    let pdvar = str_ws "#" >>. pconstant |>> fun v -> Term(DVar, [Constant (renameVar v)])
    let pqvar = str_ws "?" >>. opt pconstant |>> fun v -> Term(QVar, [(if v.IsSome then Constant (renameVar v.Value) else Constant (""))])
    let pvariable = choice [pivar; pdvar; pqvar]
    let pextset = setBetweenStrings "{" "}" pterm ExtSet
    let pextset_ws = pextset .>> ws
    let pintset = setBetweenStrings "[" "]" pterm IntSet
    let pintset_ws = pintset .>> ws
    let pset = pextset_ws <|> pintset_ws
    let parenthesised_term = between (str_ws "(") (str_ws ")") pstatement .>> ws
    let pint_image = str_ws "\\" >>. listBetweenStrings "(" ")" pterm IntImg
    let pint_image_ws = pint_image .>> ws
    let pext_image = str_ws "/" >>. listBetweenStrings "(" ")" pterm ExtImg
    let pext_image_ws = pext_image .>> ws
    let poperator = str_ws "^" >>. listBetweenStrings "(" ")" pterm Operator
    let poperator_ws = poperator .>> ws
    let pprefix_term = pint_image_ws <|> pext_image_ws <|> poperator_ws
    let opp = new OperatorPrecedenceParser<Term, unit, unit>()
    let statement = opp.ExpressionParser
    opp.TermParser <- choice [pConstant; pvariable; pset; parenthesised_term; pprefix_term]
    opp.AddOperator(InfixOperator("&&", ws, 3, Associativity.Left, fun x y -> reduce(Term(And, order [x;y]))))
    opp.AddOperator(InfixOperator("||", ws, 3, Associativity.Left, fun x y -> reduce(Term(Or, order [x;y]))))
    opp.AddOperator(InfixOperator(",", ws, 3, Associativity.Left, fun x y -> Term(Seq, [x;y])))
    opp.AddOperator(InfixOperator(";", ws, 3, Associativity.Left, fun x y -> Term(Par, order[x;y])))
    opp.AddOperator(InfixOperator("&", ws,2, Associativity.Left, fun a b -> reduce(Term(ExtInt, order [a; b]))))
    opp.AddOperator(InfixOperator("|", ws,2, Associativity.Left, fun a b -> reduce(Term(IntInt, order [a; b]))))
    opp.AddOperator(InfixOperator("*", ws,2, Associativity.Left, fun a b -> Term(Prod, [a; b])))
    opp.AddOperator(InfixOperator("^", ws,2, Associativity.Left, fun a b -> Term(Operator, [a; b])))
    opp.AddOperator(InfixOperator("-", ws,2, Associativity.Left, fun a b -> reduce(Term(ExtDif, [a; b]))))
    opp.AddOperator(InfixOperator("~", ws,2, Associativity.Left, fun a b -> reduce(Term(IntDif, [a; b]))))
    opp.AddOperator(PrefixOperator("--", ws,2, false, fun x -> Term(Not, [x])))
    opp.AddOperator(InfixOperator("-->", ws,4, Associativity.Right, fun a b -> Term(Inh, [a; b])))
    opp.AddOperator(InfixOperator("<->", ws,4, Associativity.Right, fun a b -> reduce(Term(Sim, [a; b]))))
    opp.AddOperator(InfixOperator("{--", ws,4, Associativity.Right, fun a b -> Term(Inh, [Term(ExtSet, [a]); b])))
    opp.AddOperator(InfixOperator("--]", ws,4, Associativity.Right, fun a b -> Term(Inh, [a; Term(IntSet, [b])])))
    opp.AddOperator(InfixOperator("{-]", ws,4, Associativity.Right, fun a b -> Term(Inh, [Term(ExtSet, [a]); Term(IntSet, [b])])))
    opp.AddOperator(InfixOperator("==>", ws,5, Associativity.Right, fun a b -> Term(Imp, [a; b])))
    opp.AddOperator(InfixOperator("<=>", ws,5, Associativity.Right, fun a b -> Term(Equ, [a; b])))
    opp.AddOperator(InfixOperator("<|>", ws,5, Associativity.Right, fun a b -> Term(ConEqu, [a; b])))
    opp.AddOperator(InfixOperator("</>", ws,5, Associativity.Right, fun a b -> Term(PreEqu, [a; b])))
    opp.AddOperator(InfixOperator("=/>", ws,5, Associativity.Right, fun a b -> Term(PreImp, [a; b])))
    opp.AddOperator(InfixOperator("=|>", ws,5, Associativity.Right, fun a b -> Term(ConImp, [a; b])))
    opp.AddOperator(InfixOperator("=\\>", ws,5, Associativity.Right, fun a b -> Term(RetImp, [a; b])))

    do ptermRef := choice [pConstant; pvariable; pset; parenthesised_term]
    do pstatementRef := choice [statement]
    let ptruth = between (str_ws "{") (str_ws "}") (tuple2 pfloat_ws pfloat_ws) |>> fun a -> {F = (float32)(fst a); C = (float32)(snd a)}
    let pdesire = between (str_ws "{") (str_ws "}") (tuple2 pfloat_ws pfloat_ws) |>> fun a -> {F = (float32)(fst a); C = (float32)(snd a)}
    let ptense = (str_ws ":|:" |>> fun t -> Present) <|> (str_ws ":\:" |>> fun t -> Past) <|> (str_ws ":/:" |>> fun t -> Future)
    let pjudgement = pipe3 (opt ptense) statement (opt ptruth)  (fun t s tv -> Judgement({Term = s; TV = if tv.IsSome then tv.Value else {F = 1.0f; C = 0.9f}}))
    let pquestion = pipe3 (str_ws "?") (opt ptense) statement (fun _ t s -> Question({Term = s}))
    let pgoal = pipe3 (str_ws "!") statement (opt pdesire) (fun _ s d -> Goal({Term = s; DV = if d.IsSome then d.Value else {F = 1.0f; C = 0.9f}}))
    let pquestionDesire = pipe3 (str_ws "??") (opt ptense) statement (fun _ t s -> Question({Term = s}))
    let psentence = ws >>. opt (choice [pquestion; pgoal; pjudgement]) .>> eof

    exception ParseError of string

    let Parser(program:string) =  
        vNum <- 0; vMap.Clear(); 
        match run psentence program with
        | Success(result, _, _)   -> result
        | Failure(errorMsg, e, s) -> failwith "Parser Error: %s" s
