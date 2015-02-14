open MathNet.Numerics

let binomialExperiment n k =
    let p = 0.25
    SpecialFunctions.Binomial(k, n) * (pown p n) * (pown 0.75 (k - n))

[<EntryPoint>]
let main argv = 
    let k = int(argv.[0])
    let N = int(argv.[1])
    let probabilities = Seq.init N (fun n -> binomialExperiment n (pown 2 k))
    let answer = 1.0 - Seq.sum probabilities
    printfn "%f" answer
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
