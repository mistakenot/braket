module Rules

open Parse
open Vars

type Rule = VarLookup -> Statement -> Statement option

let sumOfTwoKetsEqualsKet (varOf: VarLookup) (statement: Statement) = 
    match statement with
    | Addition(Ket a, Ket b) -> 
        varOf statement |> Ket |> Some
    | _ -> None

let sumOfTwoKetCommutes (varOf: VarLookup) (statement: Statement) = 
    match statement with
    | Addition(Ket a, Ket b) -> Addition(Ket b, Ket a) |> Some
    | _ -> None

type RuleList = (string * Rule) list

let ruleExists: string -> RuleList -> bool = fun name -> List.exists (fun (n, _) -> n = name)

let addRule name rule list = 
    if ruleExists name list then list
    else (name, rule) :: list

let getTransforms: RuleList -> VarLookup -> Statement -> Statement list = fun rules vars statement ->
    List.map (fun (_,rule) -> rule vars statement) rules 
    |> List.collect (fun o -> if o.IsSome then [o.Value] else [])

let allRules = 
    addRule "Sum of two kets equals ket" sumOfTwoKetsEqualsKet >>
    addRule "Sum of two kets commutes" sumOfTwoKetCommutes 
    <| []