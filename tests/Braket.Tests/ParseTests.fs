module Parse

open Xunit
open FsUnit.Xunit
open Lexer

type Statement = 
    | Nil
    | Bra of string
    | Ket of string
    | Scalar of string
    | Add of Statement * Statement

/// A simple, non-reducable value.
let (|Value|_|) = function
    | [Symb(s)] -> Scalar s |> Some
    | [Bar; Symb(s); CloseKet] -> Ket s |> Some
    | [OpenBra; Symb(s); Bar] -> Bra s |> Some

// let (|Binary|_|) (t: Token list) =
//     match t with
//     | Value(a)

let parse: Token list -> Statement = function
    | [] -> Nil
    | Value(v) -> v

[<Fact>]
let ``Can parse simple variable symbol`` () = 
    let actual = parse [Symb("A")]
    actual |> should equal <| Scalar("A")

[<Fact>]
let ``Can parse nill`` () = 
    parse [] |> should equal <| Nil

[<Fact>]
let ``Can parse ket`` () = 
    parse [Bar; Symb "A"; CloseKet] |> should equal <| Ket("A")

[<Fact>]
let ``Can parse bra`` () = 
    parse [OpenBra; Symb "A"; Bar] |> should equal <| Bra("A")

// [<Fact>]
let ``Can parse bra plus bra`` () = 
        // <A| + <B|     
    parse [OpenBra; Symb "A"; Bar; Plus; OpenBra; Symb "B"; Bar] |> should equal <| Add (Bra "A", Bra "B")