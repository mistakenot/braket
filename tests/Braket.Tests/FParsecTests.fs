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

let mapSuccess: Parser<'a, 'u> -> ('a -> 'b) -> Parser<'b, 'u> = fun parser mapper -> parse {
    let! value = parser
    return value |> mapper }

let (</>) = mapSuccess

let pbra = pstring "<" >>. asciiLetter .>> pstring "|" </> (fun c -> c.ToString() |> Bra)

[<Fact>]
let ``Parse simple bra`` () = 
    test pbra "<A|" |> should equal <| Bra "A"

let pket = pstring "|" >>. asciiLetter .>> pstring ">" </> (fun c -> c.ToString() |> Ket)

[<Fact>]
let ``Parse simple ket`` () = 
    test pket "|A>" |> should equal <| Ket "A"

let pproductKet = asciiLetter .>> pstring ">" </> (fun c -> c.ToString() |> Ket)

let pbraketProduct = pipe2 pbra pproductKet (fun bra ket -> Product(bra, ket))

[<Fact>]
let ``Parse braket product`` () = 
    test pbraketProduct "<A|B>" |> should equal <| Product(Bra "A", Ket "B")