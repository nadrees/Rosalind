open ImperativeUtilities
open Bio.DNA
open Utilities

[<EntryPoint>]
let main argv = 
    let dnaString = Seq.nth 0 (Utilities.ParseLinesToDNARecords(System.IO.File.ReadAllLines("input.txt")))
    let inverseString = ReverseCompliment dnaString.DNAString
    let allStrings = [dnaString.DNAString; 
                      Seq.skip 1 dnaString.DNAString; 
                      Seq.skip 2 dnaString.DNAString;
                      inverseString;
                      Seq.skip 1 inverseString;
                      Seq.skip 2 inverseString;]
    let results = Seq.collect DNASeqToProteinStrings allStrings |> Seq.map Seq.PrintString |> Seq.distinct
    System.IO.File.WriteAllLines("output.txt", results)
    printfn "Done"
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
