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

(** Registerinng and folding of Ast filters.
    Two kinds of filters must be handled:
      - Implementation filters: str_item -> str_item.
      - Interface filters: sig_item -> sig_item. *)
module type S = sig

  module Ast : Camlp4Ast.S;

  (** {6 Filters} *)

  type filter 'a = 'a -> 'a;

  value register_sig_item_filter : (filter Ast.sig_item) -> unit;
  value register_str_item_filter : (filter Ast.str_item) -> unit;

  value fold_interf_filters : ('a -> filter Ast.sig_item -> 'a) -> 'a -> 'a;
  value fold_implem_filters : ('a -> filter Ast.str_item -> 'a) -> 'a -> 'a;

end;
