namespace Utilities

open System

module IO =
    let parseFile fileName =
        let parseLine (line : string) =
            line.Split [|' '|] |> Seq.map (fun x -> Int32.Parse(x)) |> Seq.toArray

        let fileLines = System.IO.File.ReadLines(fileName)
        Seq.map (fun x -> parseLine x) fileLines

    let writeLineToFile fileName elements = 
        let lineToWrite = Array.fold (fun str elem -> str + " " + elem.ToString()) "" elements
        System.IO.File.WriteAllText(fileName, lineToWrite.Trim())