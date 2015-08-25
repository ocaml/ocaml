(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                       Pierre Chambart, OCamlPro                     *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type t

external test_alloc : unit -> t = "caml_test_pr3612_alloc"
external get_counter : unit -> int = "caml_test_pr3612_counter"
(* The number of deserialized blocs minus the number of freed blocs *)

external init : unit -> unit = "caml_test_pr3612_init"

let test s =
  for i = 0 to 1_000_000 do
    ignore (Marshal.from_string s 0)
  done

let () =
  init ();
  let s = Marshal.to_string (test_alloc ()) [] in
  test s;
  Gc.full_major ();
  print_int (get_counter ());
  print_newline ()
