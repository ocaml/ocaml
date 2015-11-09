(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Loads after stdlib but before any user modules. This forces the loading of
   the Gc module. *)

let () =
  at_exit (fun () ->
      try
        let fn = Sys.getenv "OCAML_GC_STATS" in
        if fn <> "" then
          let oc = open_out fn in
          Gc.print_stat oc;
          close_out oc
      with _ -> ())
