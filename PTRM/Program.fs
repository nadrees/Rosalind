open Bio.AminoAcid

[<EntryPoint>]
let main argv = 
    let aminoStr = Seq.map ParseAminoAcid argv.[0]
    let sum = GetMonoisotopicMassOfString aminoStr
    printfn "%f" sum
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
