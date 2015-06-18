module AminoAcidTests

open NUnit.Framework
open Bio.AminoAcid

let GenerateTryParseAminoAcidCases () =
    seq {
        yield TestCaseData('A').Returns(Some(Bio.Alphabets.AminoAcid.A))
        yield TestCaseData('B').Returns(None)
        yield TestCaseData('C').Returns(Some(Bio.Alphabets.AminoAcid.C))
        yield TestCaseData('D').Returns(Some(Bio.Alphabets.AminoAcid.D))
        yield TestCaseData('E').Returns(Some(Bio.Alphabets.AminoAcid.E))
        yield TestCaseData('F').Returns(Some(Bio.Alphabets.AminoAcid.F))
        yield TestCaseData('G').Returns(Some(Bio.Alphabets.AminoAcid.G))
        yield TestCaseData('H').Returns(Some(Bio.Alphabets.AminoAcid.H))
        yield TestCaseData('I').Returns(Some(Bio.Alphabets.AminoAcid.I))
        yield TestCaseData('J').Returns(None)
        yield TestCaseData('K').Returns(Some(Bio.Alphabets.AminoAcid.K))
        yield TestCaseData('L').Returns(Some(Bio.Alphabets.AminoAcid.L))
        yield TestCaseData('M').Returns(Some(Bio.Alphabets.AminoAcid.M))
        yield TestCaseData('N').Returns(Some(Bio.Alphabets.AminoAcid.N))
        yield TestCaseData('P').Returns(Some(Bio.Alphabets.AminoAcid.P))
        yield TestCaseData('Q').Returns(Some(Bio.Alphabets.AminoAcid.Q))
        yield TestCaseData('R').Returns(Some(Bio.Alphabets.AminoAcid.R))
        yield TestCaseData('S').Returns(Some(Bio.Alphabets.AminoAcid.S))
        yield TestCaseData('T').Returns(Some(Bio.Alphabets.AminoAcid.T))
        yield TestCaseData('U').Returns(None)
        yield TestCaseData('V').Returns(Some(Bio.Alphabets.AminoAcid.V))
        yield TestCaseData('W').Returns(Some(Bio.Alphabets.AminoAcid.W))
        yield TestCaseData('X').Returns(None)
        yield TestCaseData('Y').Returns(Some(Bio.Alphabets.AminoAcid.Y))
        yield TestCaseData('Z').Returns(None)
    }
let GenerateParseAminoAcidCases () =
    let MapTestCase (testCase : TestCaseData) =
        match testCase.Result :?> Option<Bio.Alphabets.AminoAcid> with
        | Some x -> TestCaseData(testCase.Arguments).Returns(x)
        | None -> TestCaseData(testCase.Arguments).Throws(typedefof<System.ArgumentException>)
    GenerateTryParseAminoAcidCases() |> Seq.map MapTestCase

[<Test; TestCaseSource("GenerateTryParseAminoAcidCases")>]
let ``Test Bio.AminoAcid.TryParseAminoAcid`` letter =
    TryParseAminoAcid letter

[<Test; TestCaseSource("GenerateParseAminoAcidCases")>]
let ``Test Bio.AminoAcid.ParseAminoAcid`` letter =
    ParseAminoAcid letter