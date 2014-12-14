namespace Bio

module Symbols =
    type DNA = A | G | T | C
    type RNA = A | C | G | U

    let DNAtoRNA dnaSymbol = 
        match dnaSymbol with
        | DNA.A -> RNA.A
        | DNA.C -> RNA.C
        | DNA.G -> RNA.G
        | DNA.T -> RNA.U

    let TryParseDNACharacter c =
        match c with
        | 'A' -> (true, Some(DNA.A))
        | 'C' -> (true, Some(DNA.C))
        | 'G' -> (true, Some(DNA.G))
        | 'T' -> (true, Some(DNA.T))
        | _ -> (false, None)

    let TryParseRNACharacter c =
        match c with
        | 'A' -> (true, Some(RNA.A))
        | 'C' -> (true, Some(RNA.C))
        | 'G' -> (true, Some(RNA.G))
        | 'U' -> (true, Some(RNA.U))
        | _ -> (false, None)

    let ParseDNACharacter c =
        match TryParseDNACharacter c with
        | (true, Some s) -> s
        | _ -> raise (new System.ArgumentException("Invalid character " + c.ToString()))

    let DNAtoComplement d =
        match d with 
        | DNA.A -> DNA.T
        | DNA.T -> DNA.A
        | DNA.C -> DNA.G
        | DNA.G -> DNA.C

    let PrintString symbolSeq =
        Seq.fold (fun str s -> sprintf "%s%A" str s) "" symbolSeq

    let ReverseCompliment dnaSeq = 
        Seq.toArray dnaSeq
        |> Array.rev
        |> Array.toSeq
        |> Seq.map DNAtoComplement