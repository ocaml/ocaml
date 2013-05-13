(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let debug = Printf.eprintf;;

let main () =
  debug "starting\n%!";
  let (rd, wr) = Unix.pipe() in
  debug "after pipe\n%!";
  let _ = Thread.create
    (fun () ->
      debug "sub: starting\n%!";
      ignore (Unix.write wr "0123456789" 0 10);
      debug "sub: after write\n%!";
      Thread.delay 3.0;
      debug "sub: after delay\n%!";
      print_endline "closing fd...";
      Unix.close rd;
      debug "sub: after close\n%!";
    )
    () in
  debug "after thread.create\n%!";
  let buf = String.create 10 in
  debug "after string.create\n%!";
  print_endline "reading...";
  ignore (Unix.read rd buf 0 10);
  debug "after read\n%!";
  print_endline "read returned"

let _ = Unix.handle_unix_error main ()

;;debug "that's all folks\n%!"
