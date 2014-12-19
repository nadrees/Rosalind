open Bio.DNA

let parseCharacter c =
    match TryParseDNACharacter c with
    | (true, Some s) -> s
    | _ -> raise (new System.ArgumentException("Invalid character " + c.ToString()))

[<EntryPoint>]
let main argv = 
    let inputString = argv.[0]
    let dnaCharacters = Seq.map parseCharacter inputString
    let rnaCharacters = Seq.map DNAtoRNA dnaCharacters
    let str = Seq.fold (fun s rna -> sprintf "%s%A" s rna) "" rnaCharacters
    printfn "%A" str
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
