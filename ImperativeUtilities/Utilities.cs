using System;
using System.Collections.Generic;
using System.Linq;

namespace ImperativeUtilities
{
    public class Utilities
    {
        public static IEnumerable<Bio.DNA.DNARecord> ParseLinesToRecords(IEnumerable<String> lines)
        {
            var records = new List<Bio.DNA.DNARecord>();

            String name = null, dnaSequence = String.Empty;

            foreach (var line in lines)
            {
                if (line.Contains('>'))
                {
                    if (name != null)
                        records.Add(CreateDNARecord(name, dnaSequence));

                    name = line.Substring(1);
                    dnaSequence = String.Empty;
                }
                else
                    dnaSequence += line;
            }
            records.Add(CreateDNARecord(name, dnaSequence));

            return records;
        }

        private static Bio.DNA.DNARecord CreateDNARecord(String name, String dnaSequence)
        {
            var dna = dnaSequence.Select(c => Bio.DNA.ParseDNACharacter(c));
            return new Bio.DNA.DNARecord(name, dna);
        }
    }
}
