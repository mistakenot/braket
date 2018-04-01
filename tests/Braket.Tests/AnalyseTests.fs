module Analyse

open Xunit
open FsUnit.Xunit
open Parse

[<Fact>]
let ``Analyse two equal statements`` () = 
    let l, r = Bra("A"), Bra("A")
    Analyse.run Rules.empty l r |> should equal <| true

[<Fact>]
let ``Analyse two equivilent addition statements with no rules`` () = 
    let l, r = Addition(Bra "A", Bra "B"), Addition(Bra "B", Bra "A")
    Analyse.run Rules.allRules l r |> should equal <| true