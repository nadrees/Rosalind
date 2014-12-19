namespace Bio

module AminoAcid =
    type AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | U | V | W | Y | Stop

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
