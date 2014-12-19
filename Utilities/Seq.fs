namespace Utilities

module Seq =
    let fibonacci startWith0 = 
        let rec fibHelper n0 n1 =
            seq {
                let n2 = n0 + n1
                yield n2
                yield! fibHelper n1 n2
            }
        let first = if startWith0 then 0 else 1
        seq { yield first; yield 1; yield! fibHelper first 1 }

    let HammingDistance seq1 seq2 = 
        Seq.zip seq1 seq2
        |> Seq.map (fun (x1, x2) -> if x1 <> x2 then 1 else 0)
        |> Seq.sum

    let PrintString symbolSeq =
        Seq.fold (fun str s -> sprintf "%s%A" str s) "" symbolSeq