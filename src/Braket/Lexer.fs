module Lexer

type Token =
    | Symb of string
    | OpenBra | CloseKet
    | OpenB | CloseB
    | Bar

let tokenise: string -> Token list = 
    let rec loop accum = function
        | [] -> List.rev accum
        | ' '::tail -> loop accum tail
        | '('::tail -> loop (OpenB :: accum) tail
        | ')'::tail -> loop (CloseB :: accum) tail
        | '<'::tail -> loop (OpenBra :: accum) tail
        | '>'::tail -> loop (CloseKet :: accum) tail
        | '|'::tail -> loop (Bar :: accum) tail
        | c::tail -> loop (Symb(c.ToString()) :: accum) tail
    Seq.toList >> loop []

let Plus = Symb "+"