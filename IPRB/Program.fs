// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let k = System.Double.Parse(argv.[0])
    let m = System.Double.Parse(argv.[1])
    let n = System.Double.Parse(argv.[2])

    let population = k + m + n

    // odds when we pick a dominant first
    let pK = k / population

    // odds when we choose a heterozygous first
    let pMK = (m / population) * (k / (population - 1.0))
    let pMM = (m / population) * ((m - 1.0) / (population - 1.0)) * 0.75
    let pMN = (m / population) * (n / (population - 1.0)) * 0.5

    // odds when we choose a recessive first
    let pNK = (n / population) * (k / (population - 1.0))
    let pNM = (n / population) * (m / (population - 1.0)) * 0.5

    let result = pK + pMK + pMN + pMM + pNK + pNM

    printfn "%f" result
    System.Console.ReadKey() |> ignore

    0 // return an integer exit code
