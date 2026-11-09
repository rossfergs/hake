(*
  Module for handling indentation level in lexer
 *)
exception BadIndentError of string

(** list of x positions for indentation *)
let pos_xs = ref []

(** Generate string of current levels *)
let string_of_levels () = Futil.string_of_int_list !pos_xs

(**
  Get the current indentation level
 *)
let get_level () =
  match !pos_xs with
    | [] -> 0
    | x :: _ -> x

(**
  Add a new indentation layer to the list 
 *)
let indent size =
  pos_xs := size :: !pos_xs

(** 
  Remove the top level indentation layer
 *)
let unindent () =
  match !pos_xs with
    | x :: xs -> pos_xs := xs
    | _ -> pos_xs := []

(** Reset the current state of the indentation tracker *)
let reset () =
  pos_xs := []

