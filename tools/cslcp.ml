(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let cslargs = ref ([] : string list)
let profargs = ref ([] : string list)
let toremove = ref ([] : string list)

let remove_file name =
  try Sys.remove name with Sys_error _ -> ()

let option opt () = cslargs := opt :: !cslargs
let option_with_arg opt arg = cslargs := arg :: opt :: !cslargs

let process_file filename =
  if Filename.check_suffix filename ".ml" then begin
    let instrname = filename ^ "t" in
    toremove := instrname :: !toremove;
    let status =
      Sys.command
        (Printf.sprintf "cslprof -instrument %s %s > %s"
                        (String.concat " " (List.rev !profargs))
                        filename instrname) in
    if status <> 0 then begin
      List.iter remove_file !toremove;
      exit 2
    end;
    cslargs := instrname :: !cslargs
  end else begin
    cslargs := filename :: !cslargs
  end

let _ =
  Arg.parse
    (* Same options as the compiler cslc *)
      ["-I", Arg.String(option_with_arg "-I");
       "-c", Arg.Unit(option "-c");
       "-o", Arg.String(option_with_arg "-o");
       "-i", Arg.Unit(option "-i");
       "-a", Arg.Unit(option "-a");
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
    Sys.command ("cslc " ^ String.concat " " (List.rev !cslargs)) in
  List.iter remove_file !toremove;
  exit status
