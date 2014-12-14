open Utilities

[<EntryPoint>]
let main argv = 
    let fileLines = System.IO.File.ReadAllLines("input.txt")
    let hammingDistance = Seq.HammingDistance (Seq.nth 0 fileLines) (Seq.nth 1 fileLines)
    printfn "%i" hammingDistance
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
