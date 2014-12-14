open ImperativeUtilities
open Bio.Symbols

type ReversePalindrome = {
    StartPosition : int;
    Length : int;
}

let getReversePalindromes (str : string) =
    let getReversePalindromesOfLength length = 
        printfn "Searching for reverse palindroms of length %i" length
        let rec getReversePalindromesOfLengthHelper currentSubstrings pos =
            if pos > str.Length - length then
                currentSubstrings
            else
                let substr = str.Substring(pos, length)
                let reversedSubstr = Seq.map ParseDNACharacter substr
                                     |> ReverseCompliment
                                     |> PrintString
                if (substr = reversedSubstr) then
                    let newRecord = { StartPosition = pos; Length = substr.Length; }
                    getReversePalindromesOfLengthHelper (newRecord::currentSubstrings) (pos + 1)
                else
                    getReversePalindromesOfLengthHelper currentSubstrings (pos + 1)
        getReversePalindromesOfLengthHelper [] 0
    List.collect getReversePalindromesOfLength [4..12]

[<EntryPoint>]
let main argv = 
    let fileLines = System.IO.File.ReadAllLines("input.txt")
    let dnaRecord = Seq.nth 0 (Utilities.ParseLinesToRecords(fileLines))
    let reversePalindromes = getReversePalindromes (PrintString dnaRecord.DNAString)
    System.IO.File.WriteAllLines("output.txt", List.map (fun x -> sprintf "%i %i" (x.StartPosition + 1) x.Length) reversePalindromes)
    0 // return an integer exit code
