using System;
using System.Collections.Generic;
using System.Linq;

namespace ImperativeUtilities
{
    public class Utilities
    {
        public static IEnumerable<Record> ParseLines(IEnumerable<String> lines)
        {
            lines = lines.Select(l => l.Trim());

            var records = new List<Record>();

            String name = null, sequence = String.Empty;
            foreach (var line in lines)
            {
                if (line.Contains('>'))
                {
                    if (name != null)
                        records.Add(new Record { Name = name, Str = sequence });

                    name = line.Substring(1);
                    sequence = String.Empty;
                }
                else
                    sequence += line;
            }
            records.Add(new Record { Name = name, Str = sequence });

            return records;
        }

        public static IEnumerable<Bio.DNA.DNARecord> ParseLinesToDNARecords(IEnumerable<String> lines)
        {
            return ParseLines(lines).Select(r => CreateDNARecord(r.Name, r.Str));
        }

        private static Bio.DNA.DNARecord CreateDNARecord(String name, String dnaSequence)
        {
            var dna = dnaSequence.Select(c => Bio.DNA.ParseDNACharacter(c));
            return new Bio.DNA.DNARecord(name, dna);
        }
    }

    public class Record
    {
        public String Name { get; set; }
        public String Str { get; set; }
    }
}
