open Bio.Alphabets
open Bio.AminoAcid

[<EntryPoint>]
let main argv = 
    let proteinString = argv.[0]
    let rnaCodons = Seq.map ParseAminoAcid proteinString 
                    // append the stop codon since our amino acid stopped
                    |> Seq.append (seq { yield AminoAcid.Stop })
                    |> Seq.map AminoAcidToRNACodons
    let moduloAnswer = Seq.fold (fun currentCount codons -> (currentCount * (Seq.length codons)) % 1000000) 1 rnaCodons
    printfn "%i" moduloAnswer
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
