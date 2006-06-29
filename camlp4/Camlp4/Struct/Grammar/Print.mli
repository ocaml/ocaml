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
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

module Make (Structure : Structure.S) : sig
  value flatten_tree : Structure.tree -> list (list Structure.symbol);
  value print_symbol : Format.formatter -> Structure.symbol -> unit;
  value print_meta :
    Format.formatter -> string -> list Structure.symbol -> unit;
  value print_symbol1 : Format.formatter -> Structure.symbol -> unit;
  value print_rule : Format.formatter -> list Structure.symbol -> unit;
  value print_level :
    Format.formatter ->
    (Format.formatter -> unit -> unit) ->
    list (list Structure.symbol) -> unit;
  value levels : Format.formatter -> list Structure.level -> unit;
  value entry : Format.formatter -> Structure.internal_entry -> unit;
end;

module MakeDump (Structure : Structure.S) : sig
  value print_symbol : Format.formatter -> Structure.symbol -> unit;
  value print_meta :
    Format.formatter -> string -> list Structure.symbol -> unit;
  value print_symbol1 : Format.formatter -> Structure.symbol -> unit;
  value print_rule : Format.formatter -> list Structure.symbol -> unit;
  value print_level :
    Format.formatter ->
    (Format.formatter -> unit -> unit) ->
    list (list Structure.symbol) -> unit;
  value levels : Format.formatter -> list Structure.level -> unit;
  value entry : Format.formatter -> Structure.internal_entry -> unit;
end;
