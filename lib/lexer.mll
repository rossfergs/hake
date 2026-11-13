{
let log = Futil.Logging.log ~tag:__MODULE__ ~min_level:`debug

open Indent
open Parser
open Printf

(** queue of tokens to process before continuing lexing *)
let tok_queue = ref []

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

let show_queue () =
  let rec impl = function
    | [] -> ""
    | t :: [] -> string_of_token t
    | t :: ts -> (string_of_token t) ^ "; " ^ impl ts
  in
  Printf.printf "[%s]" (impl !tok_queue)

(** Add a token to the queue *)
let add_tok_to_queue tok =
  tok_queue := tok :: !tok_queue

(** Checks the current indentation layer is valid *)

let check_indentation size =
  if size > get_level () then 
    raise (BadIndentError (Printf.sprintf "Unexpected indentation level! Current level: %d, Inbetween blocks with level %d or level %d" size (get_level ()) (get_lower_level ()))) 
  else 
  ();

(** Unindent and add necessary tokens to the queue, a NLine is always added to the queue *)
let handle_unindent size =
  add_tok_to_queue NLine;
  unindent ();
  while size < get_level () do
    log `debug (sprintf "handle_unindent -> current levels: %s, new level: %d\n" (string_of_levels ()) size);
    add_tok_to_queue Dedent;
    unindent ();
  done

let is_token_queued () = !tok_queue != []

let get_queued_token () =
  match !tok_queue with
    | [] -> failwith "empty queue"
    | tok :: ts -> tok_queue := ts; tok

let handle_indent size =
  indent size;
  add_tok_to_queue Indent

(**
  Emit a Dedent or NLine depending on what size of indentation is given, 
  will also add necesarry toks to queue
 *)
let emit_dent x_size =
  match x_size with 
    | size when size < get_level () -> handle_unindent size; Dedent
    | size when size > get_level () -> handle_indent size; NLine
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
    | eof
    | nl+ eof { if is_token_queued() then get_queued_token() else Eof }
    | nl+ ([' ' '\t']* as ws) {
          Lexing.new_line lexbuf;
          emit_dent (String.length ws)
        }
    | ">+" _* "+>"
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
    | _ as c {
        failwith (Printf.sprintf "Unexpected character: %c" c)
      }

{

(** Output a token from either the given lexbuf or any awaiting from the queue *)
let read_wrap lexbuf =
  let tok = (match !tok_queue with
    | [] -> read lexbuf
    | t :: ts ->
      tok_queue := ts; t)
  in
  show_queue ();
  log `debug (string_of_token tok);
  tok
}
