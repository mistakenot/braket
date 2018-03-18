module Parse

type Statement = 
    | Nil
    | Bra of string
    | Ket of string
    | Scalar of string
    | Add of Statement * Statement
    | Product of Statement * Statement