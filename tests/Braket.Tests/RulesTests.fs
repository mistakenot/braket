module Rules

open Xunit
open FsUnit.Xunit
open Parse
open Rules
open Vars

let addKets = Addition(Ket "A", Ket "B")
let lookupFail: VarLookup = fun s -> failwith "Called lookupFail"

[<Fact>]
let ``Ket addition creates new ket`` () =
    let lookup: VarLookup = function
    | Addition(Ket "A", Ket "B") -> "C"
    | _ -> failwith "Incorrect lookup."
    let actual = sumOfTwoKetsEqualsKet lookup addKets
    actual |> should equal <| Some(Ket "C")

[<Fact>]
let ``Sum of two keys commute`` () = 
    let actual = sumOfTwoKetCommutes lookupFail addKets
    let expected = Addition(Ket "B", Ket"A") |> Some
    actual |> should equal expected
