(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let compargs = ref ([] : string list)
let profargs = ref ([] : string list)
let toremove = ref ([] : string list)

let option opt () = compargs := opt :: !compargs
let option_with_arg opt arg = compargs := arg :: opt :: !compargs
let process_file filename = compargs := filename :: !compargs

let _ =
  Arg.parse
    (* Same options as the compiler ocamlc *)
      ["-I", Arg.String(option_with_arg "-I");
       "-c", Arg.Unit(option "-c");
       "-o", Arg.String(option_with_arg "-o");
       "-i", Arg.Unit(option "-i");
       "-a", Arg.Unit(option "-a");
       "-pp", Arg.Unit(fun () -> prerr_endline "ocamlcp: profiling is incompatible with the -pp option"; exit 2);
       "-unsafe", Arg.Unit(option "-unsafe");
       "-nopervasives", Arg.Unit(option "-nopervasives");
       "-custom", Arg.Unit(option "-custom");
       "-ccopt", Arg.String(option_with_arg "-ccopt");
       "-cclib", Arg.String(option_with_arg "-cclib");
       "-linkall", Arg.Unit(option "-linkall");
       "-drawlambda", Arg.Unit(option "-drawlambda");
       "-dlambda", Arg.Unit(option "-dlambda");
       "-dinstr", Arg.Unit(option "-dinstr");
       "-v", Arg.Unit(option "-v");
       "-", Arg.String process_file;
    (* Options specific to the profiler *)
       "-p", Arg.String(fun s -> profargs := s :: "-m" :: !profargs)]
      process_file;
  let status =
    Sys.command
      (Printf.sprintf "ocamlc -pp \"ocamlprof -instrument %s\" %s"
          (String.concat " " (List.rev !profargs))
          (String.concat " " (List.rev !compargs))) in
  exit status
