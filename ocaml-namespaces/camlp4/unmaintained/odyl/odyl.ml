(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)



value apply_load () =
  let i = ref 1 in
  let stop = ref False in
  while not stop.val && i.val < Array.length Sys.argv do {
    let s = Sys.argv.(i.val) in
    if s = "-I" && i.val + 1 < Array.length Sys.argv then do {
      Odyl_main.directory Sys.argv.(i.val + 1);
      i.val := i.val + 2
    }
    else if s = "-nolib" then do { Odyl_main.nolib.val := True; incr i }
    else if s = "-where" then do {
      print_string Odyl_config.standard_library;
      print_newline ();
      flush stdout;
      exit 0
    }
    else if s = "-version" then do {
      print_string Sys.ocaml_version;
      print_newline ();
      flush stdout;
      exit 0
    }
    else if s = "--" then do { incr i; stop.val := True; () }
    else if String.length s > 0 && s.[0] == '-' then stop.val := True
    else if Filename.check_suffix s ".cmo" || Filename.check_suffix s ".cma"
    then do { Odyl_main.loadfile s; incr i }
    else stop.val := True
  }
;

value main () =
  try do { apply_load () ; Odyl_main.go.val () } with
  [ Odyl_main.Error fname str ->
      do {
        flush stdout;
        Printf.eprintf "Error while loading \"%s\": " fname;
        Printf.eprintf "%s.\n" str;
        flush stderr;
        exit 2
      } ]
;

Printexc.catch main ();
