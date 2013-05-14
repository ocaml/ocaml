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

let main () =
  let (rd, wr) = Unix.pipe() in
  let t = Thread.create
    (fun () ->
      ignore (Unix.write wr "0123456789" 0 10);
      Thread.delay 3.0;
      print_endline "closing fd...";
      Unix.close rd;
    )
    () in
  let buf = String.create 10 in
  print_endline "reading...";
  ignore (Unix.read rd buf 0 10);
  print_endline "read returned";
  t

let t = Unix.handle_unix_error main ()

let _ = Thread.join t
