namespace Bio

open Alphabets

module RNA =
    let TryParseRNACharacter c =
        match c with
        | 'A' -> (true, Some(RNA.A))
        | 'C' -> (true, Some(RNA.C))
        | 'G' -> (true, Some(RNA.G))
        | 'U' -> (true, Some(RNA.U))
        | _ -> (false, None)

    let ParseRNACharacter c = 
        match TryParseRNACharacter c with
        | (true, Some s) -> s
        | _ -> raise (new System.ArgumentException("Invalid character " + c.ToString()))

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

    let RNASeqToProteinStrings : (seq<RNA> -> seq<AminoAcid> list) = 
        RNASeqToRNACodons >> Seq.map RNAtoAminoAcid >> Bio.AminoAcid.AcidSeqToProteinStrings