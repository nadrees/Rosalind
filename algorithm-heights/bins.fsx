open System

let parseFile = 
    let fileLines = System.IO.File.ReadLines("rosalind_bins.txt")

    let parseLine lineNum =
        let line = Seq.nth lineNum fileLines
        line.Split [|' '|] |> Seq.map (fun x -> Int32.Parse(x)) |> Seq.toArray
    
    (parseLine 2, parseLine 3)

let binarySearch value (sortedArray:_[]) =
    let rec binarySearchHelper startIndex stopIndex =
        let index = (startIndex + stopIndex) / 2
        if index = startIndex || index = stopIndex then
            -1 // index didn't move and we didn't find the number last time
        else
            let num = sortedArray.[index]
            if num > value then
                binarySearchHelper startIndex index
            else if num < value then
                binarySearchHelper index stopIndex
            else
                index + 1
    binarySearchHelper -1 (Array.length sortedArray)

let sortedArray, numsToFind = parseFile

let indexes = Array.map (fun num -> binarySearch num sortedArray) numsToFind

let result = Array.fold (fun str (num : Int32) -> str + " " + num.ToString()) "" indexes

System.IO.File.WriteAllText("rosalind_bind_resultset.txt", result.Trim())