type expr =
  | Name of string
  | Application of string * expr list
  | Int of int
  | String of string
  | Float of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Eq of expr * expr

type statement =
  | Var of string * expr
  | Return of expr option
  | Func of
    { name : string;
      params: string list;
      body: statement list;
    }

type prog = statement list
