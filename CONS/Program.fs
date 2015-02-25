open ImperativeUtilities
open Bio.DNA

[<EntryPoint>]
let main argv = 
    let dnaRecords = Utilities.ParseLinesToDNARecords(System.IO.File.ReadAllLines("input.txt"))
    let profile = DNASeqsToProfileMatrix (Seq.map (fun r -> r.DNAString) dnaRecords)
    let consensus = GetConsensusString profile

    let matrix = PrintDNAProfile profile
    let consensusStr = Utilities.Seq.PrintString consensus

    System.IO.File.WriteAllLines("output.txt", consensusStr::matrix)

    0 // return an integer exit code
