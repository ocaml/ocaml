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

let bindir = ref Ocamlbuild_config.bindir;;
let libdir = ref begin
  Filename.concat
    (try Sys.getenv "OCAMLLIB"
     with Not_found -> Ocamlbuild_config.libdir)
    "ocamlbuild"
end;;
