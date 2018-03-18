module Parse

open FParsec

type Statement = 
    | Nil
    | Bra of string
    | Ket of string
    | Scalar of string
    | Addition of Statement * Statement
    | Product of Statement * Statement
    | Transpose of Statement

let mapSuccess: Parser<'a, 'u> -> ('a -> 'b) -> Parser<'b, 'u> = fun parser mapper -> parse {
    let! value = parser
    return value |> mapper }

let (</>) = mapSuccess

let ws = spaces

let pbra: Parser<Statement, unit> = pstring "<" >>. asciiLetter .>> pstring "|"  </> (fun c -> c.ToString() |> Bra)
let pbra_ws = pbra .>> ws

let pket: Parser<Statement, unit> = pstring "|" >>. asciiLetter .>> pstring ">" </> (fun c -> c.ToString() |> Ket)
let pket_ws = pket .>> ws

let pproductKet: Parser<Statement, unit> = asciiLetter .>> pstring ">" </> (fun c -> c.ToString() |> Ket)
let pproductKet_ws = pproductKet .>> ws

let pbraketProduct = pipe2 pbra pproductKet (fun bra ket -> Product(bra, ket))
let pbraketProduct_ws = pbraketProduct .>> ws

let padd: Parser<unit, unit> = pstring "+" >>. unicodeSpaces 

let pbraAddition = pipe3 pbra_ws padd pbra (fun ba _ bb -> Addition(ba, bb))

let pketAddition = pipe3 pket_ws padd pket (fun ba _ bb -> Addition(ba, bb))
