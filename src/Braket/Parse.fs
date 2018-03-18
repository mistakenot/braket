module Parse

open FParsec

type Statement = 
    | Nil
    | Bra of string
    | Ket of string
    | Scalar of string
    | Add of Statement * Statement
    | Product of Statement * Statement

let mapSuccess: Parser<'a, 'u> -> ('a -> 'b) -> Parser<'b, 'u> = fun parser mapper -> parse {
    let! value = parser
    return value |> mapper }

let (</>) = mapSuccess

let pbra: Parser<Statement, unit> = pstring "<" >>. asciiLetter .>> pstring "|"  </> (fun c -> c.ToString() |> Bra)

let pket: Parser<Statement, unit> = pstring "|" >>. asciiLetter .>> pstring ">" </> (fun c -> c.ToString() |> Ket)

let pproductKet: Parser<Statement, unit> = asciiLetter .>> pstring ">" </> (fun c -> c.ToString() |> Ket)

let pbraketProduct = pipe2 pbra pproductKet (fun bra ket -> Product(bra, ket))