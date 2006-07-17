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

(** Abstract syntax tree minimal signature.
    Types of this signature are abstract.
    See the {!Camlp4Ast.S} signature for a concrete definition. *)
module type S = sig

  module Loc : Loc.S;

  type meta_bool = 'abstract;
  type meta_option 'a = 'abstract;
  type ctyp = 'abstract;
  type patt = 'abstract;
  type expr = 'abstract;
  type module_type = 'abstract;
  type sig_item = 'abstract;
  type with_constr = 'abstract;
  type module_expr = 'abstract;
  type str_item = 'abstract;
  type class_type = 'abstract;
  type class_sig_item = 'abstract;
  type class_expr = 'abstract;
  type class_str_item = 'abstract;
  type match_case = 'abstract;
  type ident = 'abstract;
  type binding = 'abstract;
  type module_binding = 'abstract;

  value loc_of_ctyp : ctyp -> Loc.t;
  value loc_of_patt : patt -> Loc.t;
  value loc_of_expr : expr -> Loc.t;
  value loc_of_module_type : module_type -> Loc.t;
  value loc_of_module_expr : module_expr -> Loc.t;
  value loc_of_sig_item : sig_item -> Loc.t;
  value loc_of_str_item : str_item -> Loc.t;
  value loc_of_class_type : class_type -> Loc.t;
  value loc_of_class_sig_item : class_sig_item -> Loc.t;
  value loc_of_class_expr : class_expr -> Loc.t;
  value loc_of_class_str_item : class_str_item -> Loc.t;
  value loc_of_with_constr : with_constr -> Loc.t;
  value loc_of_binding : binding -> Loc.t;
  value loc_of_module_binding : module_binding -> Loc.t;
  value loc_of_match_case : match_case -> Loc.t;
  value loc_of_ident : ident -> Loc.t;

  (** This class is the base class for map traversal on the Ast.
      To make a custom traversal class one just extend it like that:
      
      This example swap pairs expression contents:
      open Camlp4.PreCast;
      [class swap = object
        inherit Ast.map as super;
        method expr e =
          match super#expr e with
          \[ <:expr\@_loc< ($e1$, $e2$) >> -> <:expr< ($e2$, $e1$) >>
          | e -> e \];
      end;
      value _loc = Loc.ghost;
      value map = (new swap)#expr;
      assert (map <:expr< fun x -> (x, 42) >> = <:expr< fun x -> (42, x) >>);]
  *)
  class map : object
    inherit Mapper.c;
    method meta_bool : meta_bool -> meta_bool;
    method meta_option : ! 'a 'b . ('a -> 'b) -> meta_option 'a -> meta_option 'b;
    method _Loc_t : Loc.t -> Loc.t;
    method expr : expr -> expr;
    method patt : patt -> patt;
    method ctyp : ctyp -> ctyp;
    method str_item : str_item -> str_item;
    method sig_item : sig_item -> sig_item;

    method module_expr : module_expr -> module_expr;
    method module_type : module_type -> module_type;
    method class_expr : class_expr -> class_expr;
    method class_type : class_type -> class_type;
    method class_sig_item : class_sig_item -> class_sig_item;
    method class_str_item : class_str_item -> class_str_item;
    method with_constr : with_constr -> with_constr;
    method binding : binding -> binding;
    method module_binding : module_binding -> module_binding;
    method match_case : match_case -> match_case;
    method ident : ident -> ident;
  end;

  class fold : object ('self_type)
    method string : string -> 'self_type;
    method int : int -> 'self_type;
    method float : float -> 'self_type;
    method bool : bool -> 'self_type;
    method list : ! 'a . ('self_type -> 'a -> 'self_type) -> list 'a -> 'self_type;
    method option : ! 'a . ('self_type -> 'a -> 'self_type) -> option 'a -> 'self_type;
    method array : ! 'a . ('self_type -> 'a -> 'self_type) -> array 'a -> 'self_type;
    method ref : ! 'a . ('self_type -> 'a -> 'self_type) -> ref 'a -> 'self_type;
    method meta_bool : meta_bool -> 'self_type;
    method meta_option : ! 'a . ('self_type -> 'a -> 'self_type) -> meta_option 'a -> 'self_type;
    method _Loc_t : Loc.t -> 'self_type;
    method expr : expr -> 'self_type;
    method patt : patt -> 'self_type;
    method ctyp : ctyp -> 'self_type;
    method str_item : str_item -> 'self_type;
    method sig_item : sig_item -> 'self_type;
    method module_expr : module_expr -> 'self_type;
    method module_type : module_type -> 'self_type;
    method class_expr : class_expr -> 'self_type;
    method class_type : class_type -> 'self_type;
    method class_sig_item : class_sig_item -> 'self_type;
    method class_str_item : class_str_item -> 'self_type;
    method with_constr : with_constr -> 'self_type;
    method binding : binding -> 'self_type;
    method module_binding : module_binding -> 'self_type;
    method match_case : match_case -> 'self_type;
    method ident : ident -> 'self_type;
  end;

end;
