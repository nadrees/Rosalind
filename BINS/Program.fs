open Utilities.IO

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

[<EntryPoint>]
let main argv = 
    let fileLines = parseFile "rosalind_bins.txt"
    let sortedArray = Seq.nth 2 fileLines
    let numsToFind = Seq.nth 3 fileLines
    let indexes = Array.map (fun num -> binarySearch num sortedArray) numsToFind
    writeLineToFile "rosalind_bins_resultset.txt" indexes
    0
