(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

value zero_loc : Lexing.position;
value shift_pos : int -> Lexing.position -> Lexing.position;
value adjust_loc : Lexing.position -> MLast.loc -> MLast.loc;

value ctyp :           (MLast.loc -> MLast.loc) -> 'a -> MLast.ctyp -> MLast.ctyp;
value row_field :      (MLast.loc -> MLast.loc) -> 'a -> MLast.row_field -> MLast.row_field;
value class_infos :    ((MLast.loc -> MLast.loc) -> 'a -> 'b -> 'c) ->  (MLast.loc -> MLast.loc) -> 'a -> MLast.class_infos 'b -> MLast.class_infos 'c;
value patt :           (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.patt -> MLast.patt;
value expr :           (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.expr -> MLast.expr;
value module_type :    (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.module_type -> MLast.module_type;
value sig_item :       (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.sig_item -> MLast.sig_item;
value with_constr :    (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.with_constr -> MLast.with_constr;
value module_expr :    (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.module_expr -> MLast.module_expr;
value str_item :       (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.str_item -> MLast.str_item;
value class_type :     (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.class_type -> MLast.class_type;
value class_sig_item : (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.class_sig_item -> MLast.class_sig_item;
value class_expr :     (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.class_expr -> MLast.class_expr;
value class_str_item : (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.class_str_item -> MLast.class_str_item;
