(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Miscellaneous parameters *)

open Primitives
open Config
open Misc

let program_loaded = ref false
let program_name = ref ""
let socket_name = ref ""
let arguments = ref ""

let default_load_path = ref [ "."; Config.standard_library ]

let add_path dir =
  load_path := dir :: except dir !load_path;
  Envaux.reset_cache()

(* Used by emacs ? *)
let emacs = ref false
