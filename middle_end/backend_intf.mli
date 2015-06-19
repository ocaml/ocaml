(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Knowledge that the middle end needs about the backend. *)

module type S = sig
  val symbol_for_global' : (Ident.t -> Symbol.t)

  val really_import_approx : Simple_value_approx.t -> Simple_value_approx.t

  val import_global : Ident.t -> Simple_value_approx.t
  val import_symbol : Symbol.t -> Simple_value_approx.t

  val closure_symbol : Closure_id.t -> Symbol.t

  val size_int : int  (* cf. [Arch.size_int] *)
  val big_endian : bool
end
