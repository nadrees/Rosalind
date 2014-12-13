module Utilities

open System
open System.Collections.Generic

let memoize f =
    let cache = Dictionary<_,_>()
    // return a function that takes an argument and applies f to it 
    // if the result is not already known
    fun x ->
        match cache.TryGetValue(x) with
        | (true, res) -> 
            res
        | _ ->
            let calcResult = f x
            cache.[x] <- calcResult
            calcResult

let parseFile fileName =
    let parseLine (line : string) =
        line.Split [|' '|] |> Seq.map (fun x -> Int32.Parse(x)) |> Seq.toArray

    let fileLines = System.IO.File.ReadLines(fileName)
    Seq.map (fun x -> parseLine x) fileLines

let writeLineToFile fileName elements = 
    let lineToWrite = Array.fold (fun str elem -> str + " " + elem.ToString()) "" elements
    System.IO.File.WriteAllText(fileName, lineToWrite.Trim())