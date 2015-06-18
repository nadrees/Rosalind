module StringExtensions

open System

type System.String with 
    member this.Splice substrs =
        let rec spliceMember (str : String) substrs =
            match substrs with
            | [] -> str
            | currentSubStr :: remainingSubStrs ->
                let replacedStr = str.Replace(currentSubStr, String.Empty)
                spliceMember replacedStr remainingSubStrs
        spliceMember this substrs