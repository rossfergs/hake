let fails = ref 0
let incr_fail () = fails := !fails + 1
let failed_tests = ref []
let add_failed_test name error = failed_tests := (name, error) :: !failed_tests

let reset () =
  fails := 0;
  failed_tests := []

(** Confirm if current file is expected to fail *)
let is_expected_fail filename = String.starts_with ~prefix:"failing" filename

let display_fails () =
  Printf.eprintf "Some tests failed:\n";
  let names, errors = List.split !failed_tests in
  for idx = 0 to (List.length names) - 1 do
    Printf.eprintf "failed test: %s\n  Cause: %s\n" (List.nth names idx) (List.nth errors idx);
  done;
  Printf.eprintf "Number of fails: %d.\nSee above for failing test.\n" !fails

let test_file dir_name filename =
  let full_filename = dir_name ^ "/" ^ filename in
  let file_contents = Hake.Test_utils.read_file full_filename in
  if not (is_expected_fail filename) then
    try
      ignore (Hake.parse_program file_contents);
      Printf.printf "PASS: %s\n" filename
    with Hake.ParserFail err ->
      Printf.printf "FAIL: %s\n" filename;
      add_failed_test filename err;
      incr_fail ()
  else
    try
      ignore (Hake.parse_program file_contents);
      Printf.printf "FAIL: %s\n" filename;
      add_failed_test filename "unexpected pass?!";
      incr_fail ()
    with Hake.ParserFail _ ->
      Printf.printf "PASS: %s\n" filename

(** Test all files *)
let () =
  let dir_name = "parser_test_files" in
  let dir = Unix.opendir dir_name in
  let rec get_filenames dir acc =
    try
      match Unix.readdir dir with
        | "." | ".." -> get_filenames dir acc
        | filename -> get_filenames dir (filename :: acc)
    with End_of_file ->
      List.rev acc
  in
  let filenames = get_filenames dir [] in
  List.iter (test_file dir_name) filenames;
  if !fails > 0 then
    let () = display_fails () in
    reset ();
    exit 1
  else
    print_endline "all tests pass!"
