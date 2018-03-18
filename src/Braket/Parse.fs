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

// let (</>) = mapSuccess

let pbra = pstring "<" >>. asciiLetter .>> pstring "|" 

let pket = pstring "|" >>. asciiLetter .>> pstring ">" </> (fun c -> c.ToString() |> Ket)

let pproductKet = asciiLetter .>> pstring ">" </> (fun c -> c.ToString() |> Ket)

// let pbraketProduct = pipe2 pbra pproductKet (fun bra ket -> Product(bra, ket))