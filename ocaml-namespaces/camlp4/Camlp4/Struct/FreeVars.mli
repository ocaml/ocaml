(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright   2006    Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

module Make (Ast : Sig.Camlp4Ast) : sig
  module S : Set.S with type elt = string;

  value fold_binding_vars : (string -> 'accu -> 'accu) -> Ast.binding -> 'accu -> 'accu;

  class c_fold_pattern_vars ['accu] : [string -> 'accu -> 'accu] -> ['accu] ->
    object
      inherit Ast.fold;
      value acc : 'accu;
      method acc : 'accu;
    end;

  value fold_pattern_vars : (string -> 'accu -> 'accu) -> Ast.patt -> 'accu -> 'accu;

  class fold_free_vars ['accu] : [string -> 'accu -> 'accu] -> [?env_init:S.t] -> ['accu] ->
    object ('self_type)
      inherit Ast.fold;
      value free : 'accu;
      value env : S.t;
      method free : 'accu;
      method set_env : S.t -> 'self_type;
      method add_atom : string -> 'self_type;
      method add_patt : Ast.patt -> 'self_type;
      method add_binding : Ast.binding -> 'self_type;
    end;

  value free_vars : S.t -> Ast.expr -> S.t;

end;
