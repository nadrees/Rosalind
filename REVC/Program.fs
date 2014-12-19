open Utilities
open Bio.DNA

[<EntryPoint>]
let main argv = 
    let line = IO.readFile "input.txt"
    let dna = Seq.map ParseDNACharacter line
    let reversedDna = Seq.toArray dna |> Array.rev |> Array.toSeq
    let compliment = Seq.map DNAtoComplement reversedDna
    let str = Seq.PrintString compliment
    IO.writeFile "output.txt" str
    0 // return an integer exit code
