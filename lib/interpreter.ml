(**
  Tree walking interpreter for HAKE
 *)

let log = Futil.Logging.log ~min_level:`info ~tag:"HAKE"

(** Represents scope in the tree walking interpreter *)
type scope = {
  current_scope : Ast.expr Futil.String_map.t;
  global_scope : Ast.expr Futil.String_map.t;
}

(** adds an expr to the global scope, accessible with a name, and returns the new scope *)
let add_to_global_scope scope name expr : scope =
  let new_global_scope = Futil.String_map.add name expr scope.global_scope in
  { global_scope = new_global_scope; current_scope = scope.current_scope }

(** adds an expr to the current scope, accessible with a name, and returns the new scope *)
let add_to_current_scope scope name expr : scope =
  let new_current_scope = Futil.String_map.add name expr scope.current_scope in
  { global_scope = scope.global_scope; current_scope = new_current_scope }

(**
  Interprets the meaning of a statement at the program level 
  (this means that things interpreted here are added to the global scope)
let rec interpret_program scope = let open Ast in function
  | Var (name, expr) :: ss ->
      interpret_program (add_to_global_scope scope name expr) ss
  | Func { name = name; params = params; body = body } :: ss ->
      interpret_program (add_to_global_scope scope name expr) ss
  | Return _ -> 
      log `fatal "`return` statement not permitted at top-level!"
      
 *)
