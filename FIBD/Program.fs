open System
open System.Numerics

let calcMortalFib numGenerations lifeSpan =
    let rec calcNextGeneration populationStack generationNumber = 
        match generationNumber with
        | x when x = numGenerations -> List.sum populationStack
        | _ ->
            let offspringThisGeneration = List.tail populationStack |> List.sum // only members alive for more than 1 generation can breed
            let nextGenerationStack = Seq.truncate lifeSpan (offspringThisGeneration :: populationStack) |> Seq.toList
            calcNextGeneration nextGenerationStack (generationNumber + 1)
    calcNextGeneration [new BigInteger(1)] 1

[<EntryPoint>]
let main argv = 
    let n = Int32.Parse argv.[0]
    let m = Int32.Parse argv.[1]
    let answer = calcMortalFib n m
    printfn "%A" answer
    Console.ReadKey() |> ignore
    0 // return an integer exit code
