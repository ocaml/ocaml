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

include Ocamlbuild_pack.Signatures.PLUGIN
  with module Pathname = Ocamlbuild_pack.Pathname
   and module Outcome  = Ocamlbuild_pack.My_std.Outcome
   and module Tags     = Ocamlbuild_pack.Tags
   and module Command  = Ocamlbuild_pack.Command
