namespace Bio

module Symbols =
    type DNA = A | G | T | C
    type RNA = A | C | G | U
    type AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | U | V | W | Y | Stop

    let DNAtoRNA dnaSymbol = 
        match dnaSymbol with
        | DNA.A -> RNA.A
        | DNA.C -> RNA.C
        | DNA.G -> RNA.G
        | DNA.T -> RNA.U

    let RNAtoAminoAcid rnaCodon =
        match rnaCodon with
        | (RNA.G, RNA.C, RNA.U) | (RNA.G, RNA.C, RNA.C) | (RNA.G, RNA.C, RNA.A) | (RNA.G, RNA.C, RNA.G) -> AminoAcid.A
        | (RNA.U, RNA.G, RNA.U) | (RNA.U, RNA.G, RNA.C) -> AminoAcid.C
        | (RNA.G, RNA.A, RNA.U) | (RNA.G, RNA.A, RNA.C) -> AminoAcid.D
        | (RNA.G, RNA.A, RNA.A) | (RNA.G, RNA.A, RNA.G) -> AminoAcid.E
        | (RNA.U, RNA.U, RNA.U) | (RNA.U, RNA.U, RNA.C) -> AminoAcid.F
        | (RNA.G, RNA.G, RNA.U) | (RNA.G, RNA.G, RNA.C) | (RNA.G, RNA.G, RNA.A) | (RNA.G, RNA.G, RNA.G) -> AminoAcid.G
        | (RNA.C, RNA.A, RNA.U) | (RNA.C, RNA.A, RNA.C) -> AminoAcid.H
        | (RNA.A, RNA.U, RNA.U) | (RNA.A, RNA.U, RNA.C) | (RNA.A, RNA.U, RNA.A) -> AminoAcid.I
        | (RNA.A, RNA.A, RNA.A) | (RNA.A, RNA.A, RNA.G) -> AminoAcid.K
        | (RNA.U, RNA.U, RNA.A) | (RNA.U, RNA.U, RNA.G) | (RNA.C, RNA.U, RNA.U) | (RNA.C, RNA.U, RNA.C) | (RNA.C, RNA.U, RNA.A) | (RNA.C, RNA.U, RNA.G) -> AminoAcid.L
        | (RNA.A, RNA.U, RNA.G) -> AminoAcid.M // start codon
        | (RNA.A, RNA.A, RNA.U) | (RNA.A, RNA.A, RNA.C) -> AminoAcid.N
        | (RNA.C, RNA.C, RNA.U) | (RNA.C, RNA.C, RNA.C) | (RNA.C, RNA.C, RNA.A) | (RNA.C, RNA.C, RNA.G) -> AminoAcid.P
        | (RNA.C, RNA.A, RNA.A) | (RNA.C, RNA.A, RNA.G) -> AminoAcid.Q
        | (RNA.C, RNA.G, RNA.U) | (RNA.C, RNA.G, RNA.C) | (RNA.C, RNA.G, RNA.A) | (RNA.C, RNA.G, RNA.G) | (RNA.A, RNA.G, RNA.A) | (RNA.A, RNA.G, RNA.G) -> AminoAcid.R
        | (RNA.U, RNA.C, RNA.U) | (RNA.U, RNA.C, RNA.C) | (RNA.U, RNA.C, RNA.A) | (RNA.U, RNA.C, RNA.G) | (RNA.A, RNA.G, RNA.U) | (RNA.A, RNA.G, RNA.C) -> AminoAcid.S
        | (RNA.A, RNA.C, RNA.U) | (RNA.A, RNA.C, RNA.C) | (RNA.A, RNA.C, RNA.A) | (RNA.A, RNA.C, RNA.G) -> AminoAcid.T
        | (RNA.G, RNA.U, RNA.U) | (RNA.G, RNA.U, RNA.C) | (RNA.G, RNA.U, RNA.A) | (RNA.G, RNA.U, RNA.G) -> AminoAcid.V
        | (RNA.U, RNA.G, RNA.G) -> AminoAcid.W
        | (RNA.U, RNA.A, RNA.U) | (RNA.U, RNA.A, RNA.C) -> AminoAcid.Y
        | (RNA.U, RNA.A, RNA.A) | (RNA.U, RNA.A, RNA.G) | (RNA.U, RNA.G, RNA.A) -> AminoAcid.Stop

    let rec RNASeqToRNACodons (rnaSeq : seq<RNA>) : seq<(RNA * RNA * RNA)> =
        let tupleToCodon (tuple : (int * seq<int * RNA>)) =
            let unpackRNA rnaTuple = 
                match rnaTuple with
                | (_, rna) -> rna

            let groupedRNA = snd (tuple)
            let firstRNA = Seq.nth 0 groupedRNA
            let secondRNA = Seq.nth 1 groupedRNA
            let thirdRNA = Seq.nth 2 groupedRNA
            (unpackRNA firstRNA, unpackRNA secondRNA, unpackRNA thirdRNA)

        let length = (Seq.toList rnaSeq).Length
        match (length % 3) with
        | 0 ->
            let indexedRNA = Seq.mapi (fun index rna -> (index, rna)) rnaSeq
            let groupedRNA = Seq.groupBy (fun tuple -> (fst (tuple)) / 3) indexedRNA
            let rnaCodons = Seq.map tupleToCodon groupedRNA
            rnaCodons
        | x ->
            RNASeqToRNACodons (Seq.take (length - x) rnaSeq)

    let AcidSeqToProteinStrings acidSeq = 
        let rec ReadProteinStrings acidSeq currentStrings = 
            match Seq.tryFindIndex (fun a -> a = AminoAcid.M) acidSeq with
            | None -> currentStrings
            | Some startIndex ->
                let startChoppedSeq = Seq.skip startIndex acidSeq
                match Seq.tryFindIndex (fun a -> a = AminoAcid.Stop) startChoppedSeq with
                | None -> currentStrings
                | Some stopIndex ->
                    let choppedSeq = Seq.take stopIndex startChoppedSeq
                    let remainder = Seq.skip 1 startChoppedSeq
                    ReadProteinStrings remainder (choppedSeq::currentStrings)
        ReadProteinStrings acidSeq []

    let RNASeqToProteinStrings : (seq<RNA> -> seq<AminoAcid> list) = 
        RNASeqToRNACodons >> Seq.map RNAtoAminoAcid >> AcidSeqToProteinStrings

    let DNASeqToProteinStrings : (seq<DNA> -> seq<AminoAcid> list) =
        Seq.map DNAtoRNA >> RNASeqToProteinStrings

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

    let ParseRNACharacter c = 
        match TryParseRNACharacter c with
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