{
open Parser
open Printf
}

let int = ['0'-'9']+
let float = ['0'-'9']* ['.' ','] ['0'-'9']+
let name = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule read =
  parse
  | "-#" _* "#-" { read lexbuf }
  | [' ' '\t']+ { read lexbuf }
  | "\r\n"+ | '\n'+ { NLine }
  | "=" { Eq }
  | "-" { Minus }
  | "+" { Plus }
  | "*" { Mult }
  | "/" { Div }
  | ":" { Colon }
  | "(" { LParen }
  | ")" { RParen }
  | "[" { LSqb }
  | "]" { RSqb }
  | "," { Comma }
  | "\"" (_* as s) "\"" { String s }
  | "func" { Func }
  | "var" { Var }
  | "return" { Return }
  | int as i { Int (int_of_string i) }
  | float as f { (* let replaced_comma_f = String.replace_first ~sub:"," ~by:"." f in *) Float (float_of_string f) }
  | name as n { Name n }
  | eof { Eof }
