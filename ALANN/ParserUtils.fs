module ParserUtils
    open System
    open System.IO;
    open Types
    open Parser
    open Utilities
    open Trail

    let lines_of_file filename =
        seq { use stream = File.OpenRead filename
              use reader = new StreamReader(stream)
              while not reader.EndOfStream do
                  yield reader.ReadLine() }
    
    let ParseLine line =
        let makeTask sentence term =
            {P = Parameters.Parameters.USERSTI; 
             S = sentence; 
             Stamp = {Created = DateTime.Now.Ticks; 
             Occurs = Tense.Eternal; // convert tense to occurrs here
             SC = SyntacticComplexity term; 
             Origin = Origin.User; 
             Trail = TrailFuncs.MakeTrail()}}
        try
            let sentence = Parser(line)            
            let lst = 
                match sentence with
                | Some(Question({Term = t}) as q)  -> [makeTask q t]
                | Some(Judgement({Term = t}) as j) -> [makeTask j t]
                | Some(Goal({Term = t}) as g)      -> [makeTask g t]
                | None | _ -> []

            lst

        with
            ParseError(str) -> 
                printfn "%s" str
                []
    
    let ParseText(text : string) =
        let lines = text.Split('\n')
        lines 
        |> Array.map ParseLine
        |> List.concat
    
    let ParseFile(filename) =
        lines_of_file filename 
        |> Seq.map ParseLine
        |> List.concat

