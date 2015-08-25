(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

open Greet

let _ =
  let name =
    if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else
      "stranger"
  in
  greet
    (if name = "Caesar" then Nicely else Badly)
    name;
  Printf.printf "My name is %s\n" Sys.argv.(0)
;;
