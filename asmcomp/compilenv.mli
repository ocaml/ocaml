(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Compilation environments for compilation units *)

open Clambda

type unit_infos =
  { mutable ui_name: string;                    (* Name of unit implemented *)
    mutable ui_interface: Digest.t;             (* CRC of interface impl. *)
    mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
    mutable ui_imports_cmx: (string * Digest.t) list; (* Infos imported *)
    mutable ui_approx: value_approximation;     (* Approx of the structure *)
    mutable ui_curry_fun: int list;             (* Currying functions needed *)
    mutable ui_apply_fun: int list;             (* Apply functions needed *)
    mutable ui_force_link: bool }               (* Always linked *)

val reset: string -> Digest.t -> unit
        (* Reset the environment and record the name of the unit being
           compiled (first arg) and the CRC of the matching interface
           (second arg) *)

val current_unit_name: unit -> string
        (* Return the name of the unit being compiled *)

val global_approx: Ident.t -> Clambda.value_approximation
        (* Return the approximation for the given global identifier *)
val set_global_approx: Clambda.value_approximation -> unit
        (* Record the approximation of the unit being compiled *)

val need_curry_fun: int -> unit
val need_apply_fun: int -> unit
        (* Record the need of a currying (resp. application) function
           with the given arity *)

val read_unit_info: string -> unit_infos * Digest.t
        (* Read infos and CRC from a [.cmx] file. *)
val save_unit_info: string -> unit
        (* Save the infos for the current unit in the given file *)

val cmx_not_found_crc: Digest.t
        (* Special digest used in the [ui_imports_cmx] list to signal
           that no [.cmx] file was found and used for the imported unit *)

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string

exception Error of error

val report_error: error -> unit
