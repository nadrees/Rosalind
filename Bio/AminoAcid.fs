namespace Bio

module AminoAcid =
    type AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | U | V | W | Y | Stop

    let TryParseAminoAcid c =
        match c with
        | 'A' -> Some(A)
        | 'C' -> Some(C)
        | 'D' -> Some(D)
        | 'E' -> Some(E)
        | 'F' -> Some(F)
        | 'G' -> Some(G)
        | 'H' -> Some(H)
        | 'I' -> Some(I)
        | 'K' -> Some(K)
        | 'L' -> Some(L)
        | 'M' -> Some(M)
        | 'N' -> Some(N)
        | 'P' -> Some(P)
        | 'Q' -> Some(Q)
        | 'R' -> Some(R)
        | 'S' -> Some(S)
        | 'T' -> Some(T)
        | 'U' -> Some(U)
        | 'V' -> Some(V)
        | 'W' -> Some(W)
        | 'Y' -> Some(Y)
        | _ -> None

    let ParseAminoAcid c =
        match TryParseAminoAcid c with
        | Some s -> s
        | None -> raise(new System.ArgumentException(sprintf "Unable to parse amino acid %s" (c.ToString())))

    let GetMonoisotopicMass acid = 
        match acid with
        | A -> 71.03711
        | C -> 103.00919
        | D -> 115.02694
        | E -> 129.04259
        | F -> 147.06841
        | G -> 57.02146
        | H -> 137.05891
        | I -> 113.08406
        | K -> 128.09496
        | L -> 113.08406
        | M -> 131.04049
        | N -> 114.04293
        | P -> 97.05276
        | Q -> 128.05858
        | R -> 156.10111
        | S -> 87.03203
        | T -> 101.04768
        | V -> 99.06841
        | W -> 186.07931
        | Y -> 163.06333
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
