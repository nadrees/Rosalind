open System
open Utilities

let InsertionSort (arr: _[]) =
    let Swap index1 index2 =
        let value1 = arr.[index1]
        let value2 = arr.[index2]
        arr.[index1] <- value2
        arr.[index2] <- value1
    let SortIndex index = 
        let rec SortIndexHelper currentIndex swapCount =
            if currentIndex = 0 || arr.[currentIndex] >= arr.[currentIndex - 1] then
                swapCount
            else
                Swap currentIndex (currentIndex - 1)
                SortIndexHelper (currentIndex - 1) (swapCount + 1)
        SortIndexHelper index 0
    let indexes = Seq.init arr.Length (fun i -> i)
    let indexcCounts = Seq.map (fun i -> SortIndex i) indexes
    Seq.reduce (fun sum value -> sum + value) indexcCounts

[<EntryPoint>]
let main argv = 
    let fileLines = Utilities.IO.parseFile "input.txt"
    let lineToSort = Seq.nth 1 fileLines
    let count = InsertionSort lineToSort
    printfn "%i" count
    Console.ReadLine() |> ignore
    0 // return an integer exit code
