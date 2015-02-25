open System.Net.Http
open System.Text.RegularExpressions
open ImperativeUtilities

let webClient = new HttpClient()
webClient.BaseAddress <- new System.Uri("http://www.uniprot.org/uniprot/")

let GetRecord recordId = 
    let body = Async.AwaitTask (webClient.GetStringAsync(recordId + ".fasta"))
               |> Async.RunSynchronously
    let records = ImperativeUtilities.Utilities.ParseLines (body.Split('\n'))
    Seq.map (fun (r : ImperativeUtilities.Record) -> (recordId, r.Str)) records

let regex = new Regex("N[^P][ST][^P]")
let FindMotifLocations (str : string) =
    let TranslateTo1Based = List.rev >> List.map (fun l -> l + 1)

    let rec FindMotifLocationsHelper currentLocations =
        let startIndex = if currentLocations = List.Empty then 0 else (List.head currentLocations) + 1
        if startIndex > str.Length then 
            TranslateTo1Based currentLocations
        else
            let m = regex.Match(str, startIndex)
            match m.Success with
            | false -> TranslateTo1Based currentLocations
            | true ->
                let index = m.Index
                FindMotifLocationsHelper (index :: currentLocations)
    
    FindMotifLocationsHelper []

let PrintResults (recordId, (locations : seq<int>)) =
    printfn "%s" recordId
    printfn "%s" (System.String.Join(" ", locations))

[<EntryPoint>]
let main argv = 
    System.IO.File.ReadAllLines("input.txt")
    // download each file and map it to tuples of the protein string and name
    |> Seq.map (fun line -> GetRecord line) 
    |> Seq.concat
    // look for locations of the motif
    |> Seq.map (fun (recordId, str) -> (recordId, FindMotifLocations str))
    // filter any without the motif
    |> Seq.filter (fun (recordId, locations) -> locations.Length > 0)
    // output results
    |> Seq.iter PrintResults
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
