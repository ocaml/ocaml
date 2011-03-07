(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)
value print : Format.formatter -> exn -> unit;

value try_print : Format.formatter -> exn -> unit;

value to_string : exn -> string;

value try_to_string : exn -> string;

value register : (Format.formatter -> exn -> unit) -> unit;

module Register (Error : Sig.Error) : sig end;

module ObjTools : sig
  value print : Format.formatter -> Obj.t -> unit;
  value print_desc : Format.formatter -> Obj.t -> unit;
  (*Imported from the extlib*)
  value to_string : Obj.t -> string;
  value desc : Obj.t -> string;
end;
