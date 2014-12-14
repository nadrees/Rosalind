open System

let fibSeq k startNum =
    let rec fibSeqHelper n0 n1 =
        seq {
            let n2 = (k * n0) + n1
            yield n2
            yield! fibSeqHelper n1 n2
        }
    let longStartNum = int64(startNum)
    seq { yield longStartNum; yield longStartNum; yield! fibSeqHelper longStartNum longStartNum }

[<EntryPoint>]
let main argv = 
    let n = Int32.Parse(argv.[0])
    let k = Int64.Parse(argv.[1])
    let count = Seq.nth (n - 1) (fibSeq k 1)
    printfn "%i" count
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
