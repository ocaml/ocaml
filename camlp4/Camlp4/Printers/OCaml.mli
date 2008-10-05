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

module Id : Sig.Id;

module Make (Syntax : Sig.Camlp4Syntax) : sig
  open Format;
  include Sig.Camlp4Syntax
           with module Loc     = Syntax.Loc
            and module Token   = Syntax.Token
            and module Ast     = Syntax.Ast
            and module Gram    = Syntax.Gram;

  type sep = format unit formatter unit;

  value list' :
    (formatter -> 'a -> unit) ->
      format 'b formatter unit ->
        format unit formatter unit ->
          formatter -> list 'a -> unit;

  value list :
    (formatter -> 'a -> unit) ->
      format 'b formatter unit ->
        formatter -> list 'a -> unit;

  value lex_string : string -> Token.t;
  value is_infix : string -> bool;
  value is_keyword : string -> bool;
  value ocaml_char : string -> string;
  value get_expr_args :
    Ast.expr -> list Ast.expr -> (Ast.expr * list Ast.expr);
  value get_patt_args :
    Ast.patt -> list Ast.patt -> (Ast.patt * list Ast.patt);
  value get_ctyp_args :
    Ast.ctyp -> list Ast.ctyp -> (Ast.ctyp * list Ast.ctyp);
  value expr_fun_args : Ast.expr -> (list Ast.patt * Ast.expr);

  (**
    [new printer ~curry_constr:True ~comments:False]
    Default values: curry_constr = False
                    comments = True
   *)
  class printer :
    [?curry_constr: bool] -> [?comments: bool] -> [unit] ->
  object ('a)
    method interf : formatter -> Ast.sig_item -> unit;
    method implem : formatter -> Ast.str_item -> unit;
    method sig_item : formatter -> Ast.sig_item -> unit;
    method str_item : formatter -> Ast.str_item -> unit;

    value pipe : bool;
    value semi : bool;
    value semisep : sep;
    value value_val : string;
    value value_let : string;
    method anti : formatter -> string -> unit;
    method class_declaration :
      formatter -> Ast.class_expr -> unit;
    method class_expr : formatter -> Ast.class_expr -> unit;
    method class_sig_item :
      formatter -> Ast.class_sig_item -> unit;
    method class_str_item :
      formatter -> Ast.class_str_item -> unit;
    method class_type : formatter -> Ast.class_type -> unit;
    method constrain :
      formatter -> (Ast.ctyp * Ast.ctyp) -> unit;
    method ctyp : formatter -> Ast.ctyp -> unit;
    method ctyp1 : formatter -> Ast.ctyp -> unit;
    method constructor_type : formatter -> Ast.ctyp -> unit;
    method dot_expr : formatter -> Ast.expr -> unit;
    method apply_expr : formatter -> Ast.expr -> unit;
    method expr : formatter -> Ast.expr -> unit;
    method expr_list : formatter -> list Ast.expr -> unit;
    method expr_list_cons : bool -> formatter -> Ast.expr -> unit;
    method functor_arg :
      formatter -> (string * Ast.module_type) -> unit;
    method functor_args :
      formatter ->
        list (string * Ast.module_type) -> unit;
    method ident : formatter -> Ast.ident -> unit;
    method numeric : formatter -> string -> string -> unit;
    method binding : formatter -> Ast.binding -> unit;
    method record_binding : formatter -> Ast.rec_binding -> unit;
    method match_case : formatter -> Ast.match_case -> unit;
    method match_case_aux : formatter -> Ast.match_case -> unit;
    method mk_expr_list : Ast.expr -> (list Ast.expr * option Ast.expr);
    method mk_patt_list : Ast.patt -> (list Ast.patt * option Ast.patt);
    method simple_module_expr : formatter -> Ast.module_expr -> unit;
    method module_expr : formatter -> Ast.module_expr -> unit;
    method module_expr_get_functor_args :
      list (string * Ast.module_type) ->
        Ast.module_expr ->
          (list (string * Ast.module_type) *
            Ast.module_expr *
            option Ast.module_type);
    method module_rec_binding : formatter -> Ast.module_binding -> unit;
    method module_type : formatter -> Ast.module_type -> unit;
    method mutable_flag : formatter -> Ast.meta_bool -> unit;
    method direction_flag : formatter -> Ast.meta_bool -> unit;
    method rec_flag : formatter -> Ast.meta_bool -> unit;
    method flag : formatter -> Ast.meta_bool -> string -> unit;
    method node : formatter -> 'b -> ('b -> Loc.t) -> unit;
    method patt : formatter -> Ast.patt -> unit;
    method patt1 : formatter -> Ast.patt -> unit;
    method patt2 : formatter -> Ast.patt -> unit;
    method patt3 : formatter -> Ast.patt -> unit;
    method patt4 : formatter -> Ast.patt -> unit;
    method patt5 : formatter -> Ast.patt -> unit;
    method patt_tycon : formatter -> Ast.patt -> unit;
    method patt_expr_fun_args :
      formatter -> (Ast.patt * Ast.expr) -> unit;
    method patt_class_expr_fun_args :
      formatter -> (Ast.patt * Ast.class_expr) -> unit;
    method print_comments_before : Loc.t -> formatter -> unit;
    method private_flag : formatter -> Ast.meta_bool -> unit;
    method virtual_flag : formatter -> Ast.meta_bool -> unit;
    method quoted_string : formatter -> string -> unit;
    method raise_match_failure : formatter -> Loc.t -> unit;
    method reset : 'a;
    method reset_semi : 'a;
    method semisep : sep;
    method set_comments : bool -> 'a;
    method set_curry_constr : bool -> 'a;
    method set_loc_and_comments : 'a;
    method set_semisep : sep -> 'a;
    method simple_ctyp : formatter -> Ast.ctyp -> unit;
    method simple_expr : formatter -> Ast.expr -> unit;
    method simple_patt : formatter -> Ast.patt -> unit;
    method seq : formatter -> Ast.expr -> unit;
    method string : formatter -> string -> unit;
    method sum_type : formatter -> Ast.ctyp -> unit;
    method type_params : formatter -> list Ast.ctyp -> unit;
    method class_params : formatter -> Ast.ctyp -> unit;
    method under_pipe : 'a;
    method under_semi : 'a;
    method var : formatter -> string -> unit;
    method with_constraint : formatter -> Ast.with_constr -> unit;
  end;

  value with_outfile :
    option string -> (formatter -> 'a -> unit) -> 'a -> unit;

  value print :
    option string -> (printer -> formatter -> 'a -> unit) -> 'a -> unit;
end;

module MakeMore (Syntax : Sig.Camlp4Syntax) : (Sig.Printer Syntax.Ast).S;
