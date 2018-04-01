module Analyse

open Rules
open Vars
open Parse

let run: RuleList -> Statement -> Statement -> bool = fun rules left right ->
    if left = right then true
    else 
        let vars = Vars.empty
        let lTransforms = Rules.getTransforms rules vars left
        let rTransforms = Rules.getTransforms rules vars left
        List.allPairs lTransforms rTransforms |> List.exists (fun (l,r) -> l = r)