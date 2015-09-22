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


(* Original author: Berke Durak *)
(* Glob_ast *)

exception Parse_error of string
type pattern =
| Epsilon
| Star of pattern
| Class of character_class
| Concat of pattern * pattern
| Union of pattern list
| Word of string
and character_class = (char * char) Bool.boolean
type 'pattern atom = Constant of string | Pattern of 'pattern
