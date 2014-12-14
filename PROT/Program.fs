open Bio.Symbols

[<EntryPoint>]
let main argv = 
    let fileLine = System.IO.File.ReadAllText("input.txt")
    let aminoString = Seq.map ParseRNACharacter fileLine |> RNASeqToRNACodons |> Seq.map RNAtoAminoAcid |> Seq.takeWhile (fun s -> s <> AminoAcid.Stop)
    System.IO.File.WriteAllText("output.txt", (PrintString aminoString))
    0 // return an integer exit code
