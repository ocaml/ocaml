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

let usage = "Usage: ocamlcp <options> <files>\noptions are:"

let _ =
  Arg.parse [
    (* Same options as the compiler ocamlc *)
       "-a", Arg.Unit(option "-a"), " Build a library";
       "-c", Arg.Unit(option "-c"), " Compile only (do not link)";
       "-cclib", Arg.String(option_with_arg "-cclib"),
             "<opt>  Pass option <opt> to the C linker";
       "-ccopt", Arg.String(option_with_arg "-ccopt"),
             "<opt>  Pass option <opt> to the C compiler and linker";
       "-custom", Arg.Unit(option "-custom"), " Link in custom mode";
       "-i", Arg.Unit(option "-i"), " Print the types";
       "-I", Arg.String(option_with_arg "-I"),
             "<d>  Add <d> to the list of include directories";
       (* -impl et -intf ont disparu ?? PWZ *)
       "-linkall", Arg.Unit(option "-linkall"), " Don't remove unused modules";
       "-o", Arg.String(option_with_arg "-o"),
             "<name>  Set output file name to <name> (default a.out)";
       "-v", Arg.Unit(option "-v"), " Print compiler version number";
       "-unsafe", Arg.Unit(option "-unsafe"),
             " No bound checking on array and string access";

       "-nopervasives", Arg.Unit(option "-nopervasives"), " (undocumented)";
       "-drawlambda", Arg.Unit(option "-drawlambda"), " (undocumented)";
       "-dlambda", Arg.Unit(option "-dlambda"), " (undocumented)";
       "-dinstr", Arg.Unit(option "-dinstr"), " (undocumented)";

       "-", Arg.String process_file,
             "<f>  Take <f> as a file name (even if it starts with `-')";
    (* Option specific to the profiler *)
       "-p", Arg.String(fun s -> profargs := s :: "-m" :: !profargs),
             "[afilmt]  Profile constructs specified by argument:\n\
       a  Everything\n\
       f  Function calls\n\
       i  if ... then ... else\n\
       l  while, for\n\
       m  match ... with\n\
       t  try ... with"
      ] process_file usage;
  let status =
    Sys.command
      (Printf.sprintf "ocamlc -pp \"ocamlprof -instrument %s\" %s"
          (String.concat " " (List.rev !profargs))
          (String.concat " " (List.rev !compargs))) in
  exit status
