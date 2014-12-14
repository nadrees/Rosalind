open Utilities.Seq
open System

[<EntryPoint>]
let main argv = 
    let fibs = fibonacci true
    let result = Seq.nth 23 fibs
    printfn "%i" result
    Console.ReadKey() |> ignore
    0