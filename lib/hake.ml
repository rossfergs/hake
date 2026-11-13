open Lexing

module Test_utils = struct let read_file filename = Futil.read_lines_concat filename end
exception ParserFail of string

let log = Futil.Logging.log ~min_level:`info ~tag:"HAKE!"

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c

let is_empty input_string = String.equal (String.trim input_string) String.empty

(** Create an AST for the input string if possible *)
let parse_program input_string =
  (* return an empty list if the input is empty *)
  if is_empty input_string then let () = log `warning "Empty input!" in [] else
  (* process string to remove \n\s*\n and replace wiht \n\n *)
  let re_remove_empty_space = Str.regexp {|^\s*$|} in
  let re_dup_newlines = Str.regexp {|\n\n+|} in
  let processed_input = Str.global_replace re_remove_empty_space "\n\n" input_string
  |> Str.global_replace re_dup_newlines "\n\n" in
  (* create token buffer from input *)
  let lexbuf = Lexing.from_string processed_input in
  try
    let sl = Parser.prog (Lexer.read_wrap) lexbuf in
    Indent.reset ();
    sl
  with Parser.Error ->
    Indent.reset ();
    raise (ParserFail ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))
