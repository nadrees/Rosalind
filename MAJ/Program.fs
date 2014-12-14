open System
open System.Collections
open Utilities

let countInteger currentCount num =
    match Map.tryFind num currentCount with
    | Some x ->
        Map.add num (x + 1) currentCount
    | None ->
        Map.add num 1 currentCount

let countNumsInLine lineArr =
    Array.fold countInteger Map.empty lineArr

let findMajorityElement lineArr =    
    let numCounts = countNumsInLine lineArr
    let key = Map.tryFindKey (fun k v -> v > (lineArr.Length / 2)) numCounts
    match key with
    | Some v -> v
    | None -> -1

[<EntryPoint>]
let main argv = 
    let fileLines = Seq.skip 1 (IO.parseFile "input.txt")
    let majorityElements = Seq.map (fun line -> findMajorityElement line) fileLines
    IO.writeLineToFile "output.txt" (Seq.toArray majorityElements)
    0 // return an integer exit code
