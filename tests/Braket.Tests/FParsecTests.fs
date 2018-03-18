module FParsecTests

open FParsec
open Xunit
open FsUnit.Xunit

let test p v = 
    match run p v with
    | Success(v, _, _) -> v
    | Failure(msg, _, _) -> failwith msg

[<Fact>]
let ``Parse simple float`` () = 
    test pfloat "1.0" |> should equal <| 1.0

open Parse

[<Fact>]
let ``Parse simple bra`` () = 
    test pbra "<A|" |> should equal <| Bra "A"

[<Fact>]
let ``Parse simple ket`` () = 
    test pket "|A>" |> should equal <| Ket "A"

[<Fact>]
let ``Parse braket product`` () = 
    test pbraketProduct "<A|B>" |> should equal <| Product(Bra "A", Ket "B")