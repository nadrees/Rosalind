namespace Bio

open Alphabets

module AminoAcid =
    let TryParseAminoAcid c =
        match c with
        | 'A' -> Some(AminoAcid.A)
        | 'C' -> Some(AminoAcid.C)
        | 'D' -> Some(AminoAcid.D)
        | 'E' -> Some(AminoAcid.E)
        | 'F' -> Some(AminoAcid.F)
        | 'G' -> Some(AminoAcid.G)
        | 'H' -> Some(AminoAcid.H)
        | 'I' -> Some(AminoAcid.I)
        | 'K' -> Some(AminoAcid.K)
        | 'L' -> Some(AminoAcid.L)
        | 'M' -> Some(AminoAcid.M)
        | 'N' -> Some(AminoAcid.N)
        | 'P' -> Some(AminoAcid.P)
        | 'Q' -> Some(AminoAcid.Q)
        | 'R' -> Some(AminoAcid.R)
        | 'S' -> Some(AminoAcid.S)
        | 'T' -> Some(AminoAcid.T)
        | 'U' -> Some(AminoAcid.U)
        | 'V' -> Some(AminoAcid.V)
        | 'W' -> Some(AminoAcid.W)
        | 'Y' -> Some(AminoAcid.Y)
        | _ -> None

    let ParseAminoAcid c =
        match TryParseAminoAcid c with
        | Some s -> s
        | None -> raise(new System.ArgumentException(sprintf "Unable to parse amino acid %s" (c.ToString())))

    let GetMonoisotopicMass acid = 
        match acid with
        | AminoAcid.A -> 71.03711
        | AminoAcid.C -> 103.00919
        | AminoAcid.D -> 115.02694
        | AminoAcid.E -> 129.04259
        | AminoAcid.F -> 147.06841
        | AminoAcid.G -> 57.02146
        | AminoAcid.H -> 137.05891
        | AminoAcid.I -> 113.08406
        | AminoAcid.K -> 128.09496
        | AminoAcid.L -> 113.08406
        | AminoAcid.M -> 131.04049
        | AminoAcid.N -> 114.04293
        | AminoAcid.P -> 97.05276
        | AminoAcid.Q -> 128.05858
        | AminoAcid.R -> 156.10111
        | AminoAcid.S -> 87.03203
        | AminoAcid.T -> 101.04768
        | AminoAcid.V -> 99.06841
        | AminoAcid.W -> 186.07931
        | AminoAcid.Y -> 163.06333
        | _ -> raise(new System.ArgumentException(sprintf "Unknown acid %A" acid))

    let GetMonoisotopicMassOfString : (seq<AminoAcid> -> float) =
        Seq.map GetMonoisotopicMass >> Seq.sum

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
