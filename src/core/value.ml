type t =
    | VInt of int
    | VBool of bool
    | VList of t list
    | VString of string
    | VError
    | VDontCare

type vtype =
    | TInt
    | TBool
    | TList
    | TString

let rec to_string = function
    | VInt x -> string_of_int x
    | VBool true -> "true"
    | VBool false -> "false"
    | VList x -> "[" ^ (CCString.concat "," (CCList.map to_string x)) ^ "]"
    | VString x -> "\"" ^ x ^ "\""
    | VError -> "_|_"
    | VDontCare -> "???"

let compare = Pervasives.compare
