module Rules

open Parse
open Vars

let sumOfTwoKetsEqualsKet (varOf: VarLookup) (statement: Statement) = 
    match statement with
    | Addition(Ket a, Ket b) -> 
        varOf statement |> Ket |> Some
    | _ -> None

let sumOfTwoKetCommutes (varOf: VarLookup) (statement: Statement) = 
    match statement with
    | Addition(Ket a, Ket b) -> Addition(Ket b, Ket a) |> Some
    | _ -> None