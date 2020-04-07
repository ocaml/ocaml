(* TEST
modules = "stubs.c"
*)

external setlocale : string -> unit = "ml_setlocale"

let show f =
  try
    string_of_float @@ f ()
  with exn -> Printf.sprintf "exn %s" (Printexc.to_string exn)
let pr fmt = Printf.ksprintf print_endline fmt

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
  setlocale "";
  test ();
  pr "locale nl_NL";
  setlocale "nl_NL";
  test ();
  pr "locale POSIX";
  setlocale "C";
  test ();
  ()
