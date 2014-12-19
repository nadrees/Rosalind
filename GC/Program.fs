open Bio.DNA
open ImperativeUtilities

type GCContentRecord = {
    dna : DNARecord
    GCContent : float
}

let foldState state elem =
    match state with
    | (currentTotal, currentCount) ->    
        match elem with
        | (true, count) ->
            (currentTotal + count, currentCount + count)
        | (false, count) ->
            (currentTotal + count, currentCount)

let computeGCContent dnaRecord = 
    let symbolCounts = Seq.countBy (fun x -> x = DNA.G || x = DNA.C) dnaRecord.DNAString
    let totalCount = Seq.fold foldState (0, 0) symbolCounts
    match totalCount with
    | (finalTotal, finalCount) ->
        { dna = dnaRecord; GCContent = (float(finalCount) / float(finalTotal)) }

[<EntryPoint>]
let main argv = 
    let fileLines = System.IO.File.ReadAllLines("input.txt")
    let records = Seq.map computeGCContent (Utilities.ParseLinesToRecords(fileLines))
    let maxRecord = Seq.maxBy (fun r -> r.GCContent) records
    printfn "%s" maxRecord.dna.Name
    printfn "%f" (maxRecord.GCContent * 100.0)
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
