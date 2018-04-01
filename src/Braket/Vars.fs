module Vars

open Parse

type Var = string * Statement

type VarCollection = Map<string, Statement>

/// Get a symbol to represent a statement
type VarLookup = Statement -> string

let empty: VarLookup = fun _ -> ""
