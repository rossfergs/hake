{
let log = Futil.Logging.log ~tag:__MODULE__ ~min_level:`info

open Indent
open Parser
open Printf

(** queue of tokens to process before continuing lexing *)
let tok_queue = ref []

(** Add a token to the queue *)
let add_tok_to_queue tok =
  tok_queue := tok :: !tok_queue

(** Unindent and add necessary tokens to the queue, a NLine is always added to the queue *)
let handle_unindent size =
  add_tok_to_queue NLine;
  unindent ();
  while size < get_level () do
    log `debug (sprintf "handle_unindent -> current levels: %s, new level: %d\n" (string_of_levels ()) size);
    add_tok_to_queue Dedent;
    unindent ();
  done

(**
  Emit a Dedent or NLine depending on what size of indentation is given, 
  will also add necesarry toks to queue
 *)
let emit_dent x_size =
  match x_size with 
    | size when size < get_level () -> handle_unindent size; Dedent
    | size when size > get_level () -> indent size; add_tok_to_queue Indent; NLine
    | size when size = get_level () -> NLine
    | _ ->
      raise (BadIndentError (Printf.sprintf "Expected X: %d, Actual X: %d" (get_level ()) x_size))
}

let int = ['0'-'9']+
let float = ['0'-'9']* ['.' ','] ['0'-'9']+
let name = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let nl = "\r\n"|'\n'

rule read =
  parse
    | ">+" _* "+>" { read lexbuf }
    | nl+ ([' ' '\t']* as ws) {
          Lexing.new_line lexbuf;
          emit_dent (String.length ws)
        }
    | [' ' '\t']+ { read lexbuf }
    | "=" { Eq }
    | "-" { Minus }
    | "+" { Plus }
    | "*" { Mult }
    | "/" { Div }
    | ":" { Colon }
    | "(" { LParen }
    | ")" { RParen }
    | "," { Comma }
    | "\"" (_* as s) "\"" { String s }
    | "func" { Func }
    | "var" { Var }
    | "return" { Return }
    | int as i { Int (int_of_string i) }
    | float as f { Float (float_of_string f) }
    | name as n { Name n }
    | eof { Eof }
    | _ as c {
        failwith (Printf.sprintf "Unexpected character: %c" c)
      }

{
(** Generate a string of the input token *)
let string_of_token = function
  | Int i -> Printf.sprintf "Int %d" i
  | Float f -> Printf.sprintf "Float %f" f
  | String s -> Printf.sprintf "String %s" s
  | Name n -> Printf.sprintf "Name %s" n
  | Eq -> "Eq"
  | Func -> "Func"
  | Var -> "Var"
  | Return -> "Return"
  | Mult -> "Mult"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Div -> "Div"
  | Colon -> "Colon"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Comma -> "Comma"
  | NLine -> "NLine"
  | Indent -> "Indent"
  | Dedent -> "Dedent"
  | Eof -> "Eof"

(** Output a token from either the given lexbuf or any awaiting from the queue *)
let read_wrap lexbuf =
  let t = (match !tok_queue with
    | [] -> read lexbuf
    | t :: ts ->
      tok_queue := ts; t)
  in
  log `debug (string_of_token t);
  t
}
