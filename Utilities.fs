module Utilities

// the F# map type is immutable, and recreating them often is very slow
// use the .NET Dictionary as our backing cache instead
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