module Vars

open Parse

/// Get a symbol to represent a statement
type VarLookup = Statement -> string

type Var = string * Statement
