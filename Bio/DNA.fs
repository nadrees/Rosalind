namespace Bio

module DNA = 
    type DNA = A | G | T | C

    type DNARecord = {
        Name : string
        DNAString : seq<DNA>
    }

    let TryParseDNACharacter c =
        match c with
        | 'A' -> (true, Some(DNA.A))
        | 'C' -> (true, Some(DNA.C))
        | 'G' -> (true, Some(DNA.G))
        | 'T' -> (true, Some(DNA.T))
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

    let DNAtoRNA dnaSymbol = 
        match dnaSymbol with
        | DNA.A -> RNA.A
        | DNA.C -> RNA.C
        | DNA.G -> RNA.G
        | DNA.T -> RNA.U

    let ReverseCompliment dnaSeq = 
        Seq.toArray dnaSeq
        |> Array.rev
        |> Array.toSeq
        |> Seq.map DNAtoComplement

    let DNASeqToProteinStrings : (seq<DNA> -> seq<Bio.AminoAcid.AminoAcid> list) =
        Seq.map DNAtoRNA >> Bio.RNA.RNASeqToProteinStrings