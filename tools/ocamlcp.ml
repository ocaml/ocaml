(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Printf

let compargs = ref ([] : string list)
let profargs = ref ([] : string list)
let toremove = ref ([] : string list)

let option opt () = compargs := opt :: !compargs
let option_with_arg opt arg = compargs := arg :: opt :: !compargs
let process_file filename = compargs := filename :: !compargs

let make_archive = ref false

let usage = "Usage: ocamlcp <options> <files>\noptions are:"

let incompatible o =
  fprintf stderr "ocamlcp: profiling is incompatible with the %s option\n" o;
  exit 2

module Options = Main_args.Make_options (struct
  let _a () = make_archive := true; option "-a" ()
  let _c = option "-c"
  let _cclib s = option_with_arg "-cclib" s
  let _ccopt s = option_with_arg "-ccopt" s
  let _custom = option "-custom"
  let _g = option "-g"
  let _i = option "-i"
  let _I s = option_with_arg "-I" s
  let _impl s = option_with_arg "-impl" s
  let _intf s = option_with_arg "-intf" s
  let _linkall = option "-linkall"
  let _make_runtime = option "-make_runtime"
  let _noassert = option "-noassert"
  let _o s = option_with_arg "-o" s
  let _output_obj = option "-output-obj"
  let _pp s = incompatible "-pp"
  let _thread () = incompatible "-thread"
  let _unsafe = option "-unsafe"
  let _use_runtime s = option_with_arg "-use_runtime" s
  let _v = option "-v"
  let _verbose = option "-verbose"
  let _nopervasives = option "-nopervasives"
  let _drawlambda = option "-drawlambda"
  let _dlambda = option "-dlambda"
  let _dinstr = option "-dinstr"
  let anonymous = process_file
end)

let _ =
  let optlist = Options.list @ [
       "-p", Arg.String(fun s -> profargs := s :: "-m" :: !profargs),
             "[afilmt]  Profile constructs specified by argument:\n\
       a  Everything\n\
       f  Function calls\n\
       i  if ... then ... else\n\
       l  while, for\n\
       m  match ... with\n\
       t  try ... with"
    ]
  in
  Arg.parse optlist process_file usage;
  let status =
    Sys.command
      (Printf.sprintf "ocamlc -pp \"ocamlprof -instrument %s\" %s %s"
          (String.concat " " (List.rev !profargs))
          (if !make_archive then "profiling.cmo" else "")
          (String.concat " " (List.rev !compargs))) in
  exit status
