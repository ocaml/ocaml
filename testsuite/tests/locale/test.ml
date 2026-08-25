(* TEST
 modules = "stubs.c";
 run_can_skip = "true";
*)

external setlocale : string -> string option = "ml_setlocale"

let show f =
  try
    string_of_float @@ f ()
  with exn -> Printf.sprintf "exn %s" (Printexc.to_string exn)
let pr fmt = Printf.ksprintf print_endline fmt

let setlocale locale_aliases =
  match List.find_opt (fun loc -> setlocale loc <> None) locale_aliases with
  | Some _ -> ()
  | None ->
      exit (int_of_string (Sys.getenv "TEST_SKIP"))

let () =
  let s = "12345.6789" in
  let f = 1.23 in
  let test () =
    pr "  print 1.23 : %s" (show @@ fun () -> f);
    pr "  parse %S : %s" s (show @@ fun () -> float_of_string s);
    pr "  roundtrip 1.23 : %s"
      (show @@ fun () -> float_of_string @@ string_of_float f);
  in
  pr "locale from environment";
  setlocale [""];
  test ();
  pr "locale nl_NL";
  setlocale ["nl_NL"; "nl_NL.utf8"; "Dutch_Netherlands.1252"];
  test ();
  pr "locale POSIX";
  setlocale ["C"];
  test ();
  ()
