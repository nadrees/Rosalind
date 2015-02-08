namespace Bio

module Alphabets =
    [<RequireQualifiedAccess>]
    type DNA = A | G | T | C
    [<RequireQualifiedAccess>]
    type RNA = A | C | G | U
    [<RequireQualifiedAccess>]
    type AminoAcid = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y | Stop