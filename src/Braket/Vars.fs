module Vars

open Parse

/// Get a symbol to represent a statement
type VarLookup = Statement -> string

type Var = string * Statement

let empty: VarLookup = fun _ -> "A"

let getOrAdd: Statement -> VarLookup -> (string * VarLookup) = fun statement lookup ->