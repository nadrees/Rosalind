namespace Bio

open Alphabets

module DNA = 
    type DNARecord = {
        Name : string
        DNAString : seq<DNA>
    }

    type ProfileColumn = {
        ACount : int
        CCount: int
        GCount : int
        TCount : int
    }
    let private DefaultProfileColumn = {
        ACount = 0;
        CCount = 0;
        GCount = 0;
        TCount = 0;
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

    let DNASeqToProteinStrings : (seq<DNA> -> seq<AminoAcid> list) =
        Seq.map DNAtoRNA >> Bio.RNA.RNASeqToProteinStrings

    let DNASeqsToProfileMatrix dnaSeqs = 
        let addSeqs seq1 seq2 =    
            let addColumn col1 col2 = 
                { ACount = col1.ACount + col2.ACount;
                  CCount = col1.CCount + col2.CCount;
                  GCount = col1.GCount + col2.GCount;
                  TCount = col1.TCount + col2.TCount; }
            Seq.map2 addColumn seq1 seq2
        let DNASeqToProfileMatrix dnaSeq = 
            let DNAToProfileColumn d =
                match d with
                | DNA.A -> { DefaultProfileColumn with ACount = 1 }
                | DNA.C -> { DefaultProfileColumn with CCount = 1 }
                | DNA.G -> { DefaultProfileColumn with GCount = 1 }
                | DNA.T -> { DefaultProfileColumn with TCount = 1 }
            Seq.map DNAToProfileColumn dnaSeq
        let profiles = Seq.map DNASeqToProfileMatrix dnaSeqs
        Seq.reduce addSeqs profiles |> Seq.toList

    let GetConsensusString profile = 
        let GetConsensusForColumn column =
            match column with 
            | _ when column.ACount >= column.CCount &&
                     column.ACount >= column.GCount &&
                     column.ACount >= column.TCount -> DNA.A
            | _ when column.CCount >= column.GCount &&
                     column.CCount >= column.TCount -> DNA.C
            | _ when column.GCount >= column.TCount -> DNA.G
            | _ -> DNA.T
        Seq.map GetConsensusForColumn profile

    let PrintDNAProfile profile = 
        let printLine f = 
            Seq.fold (fun state column -> sprintf "%s %i" state (f column)) "" profile
        let aStr = printLine (fun column -> column.ACount)
        let cStr = printLine (fun column -> column.CCount)
        let gStr = printLine (fun column -> column.GCount)
        let tStr = printLine (fun column -> column.TCount)
        [ sprintf "A:%s" aStr;
          sprintf "C:%s" cStr;
          sprintf "G:%s" gStr;
          sprintf "T:%s" tStr; ]