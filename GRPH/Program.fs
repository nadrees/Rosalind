open Bio.Alphabets
open Bio.DNA
open ImperativeUtilities
open Utilities.Seq

type Node = {
    DNARecord: DNARecord;
    Prefix: seq<DNA>;
    Suffix: seq<DNA>
}

let FilterPairs pair =
    match pair with
    | (x1, x2) -> SeqsAreEqual x1.Suffix x2.Prefix

[<EntryPoint>]
let main argv = 
    let answer = Utilities.ParseLinesToRecords(System.IO.File.ReadAllLines("input.txt")) 
                 // map each record to a node with the prefix and suffix memoized
                 |> Seq.map (fun dna -> { DNARecord = dna; 
                                          Prefix = PrefixOfLength 3 dna.DNAString; 
                                          Suffix = SuffixOfLength 3 dna.DNAString })
                 |> Seq.toList
                 // generate all pairs of nodes
                 |> AllPairs
                 // look for node pairs where the suffix = prefix
                 |> Seq.filter FilterPairs
                 // translate found pairs to name name string
                 |> Seq.map (fun pair -> match pair with | (x1, x2) -> sprintf "%s %s" x1.DNARecord.Name x2.DNARecord.Name)
                 // combine strings to one, using newline to separate
                 |> Seq.reduce (fun s1 s2 -> sprintf "%s\r\n%s" s1 s2)
    printfn "%s" answer
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
