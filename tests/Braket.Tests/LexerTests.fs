module Lexer

open System
open Xunit
open FsUnit.Xunit
open Lexer

[<Fact>]
let ``Lex simple symbol`` () = 
    let actual = tokenise "A"
    actual |> should equal ([Symb "A"])

[<Fact>]
let ``Lex simple bra`` () = 
    let actual = tokenise "<A|"
    actual |> should equal ([OpenBra; Symb "A"; Bar])

[<Fact>]
let ``Lex simple ket`` () = 
    let actual = tokenise "|A>"
    actual |> should equal ([Bar; Symb "A"; CloseKet])

[<Fact>]
let ``Lex simple braket`` () = 
    let actual = tokenise "<A|B>"
    actual |> should equal ([OpenBra; Symb "A"; Bar; Symb "B"; CloseKet])

[<Fact>]
let ``Lex ket add ket`` () = 
    let actual = tokenise "|A> + |B>"
    actual |> should equal [Bar; Symb "A"; CloseKet; Symb "+"; Bar; Symb "B"; CloseKet]

[<Fact>]
let ``Lex ket add constant`` () = 
    let actual = tokenise "|A> + z"
    actual |> should equal [Bar; Symb "A"; CloseKet; Symb "+"; Symb "z"]

[<Fact>]
let ``Lex symbol in brackets`` () = 
    let actual = tokenise "(z)"
    actual |> should equal [OpenB; Symb "z"; CloseB]