open Utilities.IO

let findSubstrPositions (str1 : string) (str2 : string) =
    let rec findSubstrPositionsHelper currentPositions = 
        match currentPositions with
        | [] ->
            match str1.IndexOf(str2) with
            | -1 -> currentPositions
            | x -> findSubstrPositionsHelper (x::currentPositions)
        | _ ->
            let startIndex = (List.head currentPositions) + 1
            if (startIndex = str1.Length) then
                currentPositions
            else
                match str1.IndexOf(str2, startIndex) with
                | -1 -> currentPositions
                | x -> findSubstrPositionsHelper (x::currentPositions)
    List.rev (findSubstrPositionsHelper List.Empty)

[<EntryPoint>]
let main argv = 
    let fileLines = System.IO.File.ReadAllLines("input.txt")
    let substrPositions = List.map (fun x -> x + 1) (findSubstrPositions (Seq.nth 0 fileLines) (Seq.nth 1 fileLines))
    writeLineToFile "output.txt" (List.toArray substrPositions)
    0 // return an integer exit code
