open Bio.RNA
open Bio.AminoAcid
open Utilities

[<EntryPoint>]
let main argv = 
    let fileLine = System.IO.File.ReadAllText("input.txt")
    let aminoString = Seq.map ParseRNACharacter fileLine |> RNASeqToRNACodons |> Seq.map RNAtoAminoAcid |> Seq.takeWhile (fun s -> s <> AminoAcid.Stop)
    System.IO.File.WriteAllText("output.txt", (Seq.PrintString aminoString))
    0 // return an integer exit code
