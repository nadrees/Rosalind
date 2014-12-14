open System

type Symbol = A | G | T | C

type SymbolCount = {
    ACount : int;
    CCount : int;
    GCount : int;
    TCount : int;
}

let characterToSymbol c =
    match c with
    | 'A' -> A
    | 'G' -> G
    | 'T' -> T
    | 'C' -> C
    | _ -> raise (System.ArgumentException("Invalid character found: " + c.ToString()))

let countSymbol state sym =
    match sym with
    | A -> { state with ACount = state.ACount + 1 }
    | C -> { state with CCount = state.CCount + 1 }
    | G -> { state with GCount = state.GCount + 1 }
    | T -> { state with TCount = state.TCount + 1 }

[<EntryPoint>]
let main argv = 
    let inputString = argv.[0]
    let symbols = Seq.map (fun c -> characterToSymbol c) inputString
    let result = Seq.fold countSymbol { ACount = 0; CCount = 0; GCount = 0; TCount = 0; } symbols
    printfn "%i %i %i %i" result.ACount result.CCount result.GCount result.TCount
    Console.ReadKey() |> ignore
    0 // return an integer exit code
