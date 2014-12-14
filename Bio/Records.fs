namespace Bio

module Records =
    type DNARecord = {
        Name : string
        DNAString : seq<Symbols.DNA>
    }
