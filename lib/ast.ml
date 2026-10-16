type expr =
  | Name of string
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
  | Func of
    { name : string;
      params: string list;
      body: statement list * expr option;
    }

type prog = statement list
