open System

let E'offspringDominantPhenotype p'offspringDominantPhenotype numChildren numPopulation =
    p'offspringDominantPhenotype * numChildren * numPopulation

let calcOffspring infoTuple =
    match infoTuple with
    | numPopulation, p'offSpringDominantPhenotype -> E'offspringDominantPhenotype p'offSpringDominantPhenotype 2.0 numPopulation

[<EntryPoint>]
let main argv = 
    let numAA_AA = Double.Parse(argv.[0])
    let numAA_Aa = Double.Parse(argv.[1])
    let numAA_aa = Double.Parse(argv.[2])
    let numAa_Aa = Double.Parse(argv.[3])
    let numAa_aa = Double.Parse(argv.[4])
    let numaa_aa = Double.Parse(argv.[5])

    let answer = [(numAA_AA, 1.0); (numAA_Aa, 1.0); (numAA_aa, 1.0); (numAa_Aa, 0.75); (numAa_aa, 0.50)]
                 |> List.map calcOffspring
                 |> List.sum

    Console.WriteLine(answer);
    Console.ReadKey() |> ignore
    0 // return an integer exit code
