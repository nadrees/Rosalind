open ImperativeUtilities
open Bio.DNA
open Bio.RNA
open Bio.Alphabets
open StringExtensions
open Utilities

[<EntryPoint>]
let main argv = 
    let dnaStrings = Utilities.ParseLines(System.IO.File.ReadAllLines("input.txt")) |> Seq.map (fun r -> r.Str)

    let dnaString = Seq.nth 0 dnaStrings
    let introns = Seq.skip 1 dnaStrings |> Seq.toList

    let proteinString = dnaString.Splice(introns)
                        |> Seq.map (fun dna -> ParseDNACharacter dna |> DNAtoRNA)
                        |> RNASeqToAminoAcids
                        |> Seq.takeWhile (fun aa -> aa <> AminoAcid.Stop)
                        |> Seq.PrintString
    printfn "%A" proteinString
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
