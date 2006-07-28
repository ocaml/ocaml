(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 1998-2006 Institut National de Recherche en Informatique et   *)
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

(* $Id$ *)

(** Quotation operations. *)

type t =
  { q_name     : string ;
    q_loc      : string ;
    q_shift    : int    ;
    q_contents : string };

module type S = sig
  module Ast : Ast.S;
  open Ast;

  (** The Loc.t is the initial location. The option string is the optional name
      for the location variable. The string is the quotation contents. *)
  type expand_fun 'a = Loc.t -> option string -> string -> 'a;

  (** The type for quotation expanders kind:
  -      [ExStr exp] for an expander [exp] returning a string which
          can be parsed to create a syntax tree. Its boolean parameter
          tells whether the quotation is in position of an expression
          (True) or in position of a pattern (False). Quotations expanders
          created with this way may work for some particular language syntax,
          and not for another one (e.g. may work when used with Revised
          syntax and not when used with Ocaml syntax, and conversely).
  -      [ExAst (expr_exp, patt_exp)] for expanders returning directly
          syntax trees, therefore not necessiting to be parsed afterwards.
          The function [expr_exp] is called when the quotation is in
          position of an expression, and [patt_exp] when the quotation is
          in position of a pattern. Quotation expanders created with this
          way are independant from the language syntax. *)
  type expander =
    [ ExStr of bool -> expand_fun string
    | ExAst of (expand_fun Ast.expr) and (expand_fun Ast.patt) ];

  (** [add name exp] adds the quotation [name] associated with the
      expander [exp]. *)
  value add : string -> expander -> unit;

  (** [find name] returns the expander of the given quotation name. *)
  value find : string -> expander;

  (** [default] holds the default quotation name. *)
  value default : ref string;

  (** function translating quotation names; default = identity *)
  value translate : ref (string -> string);

  value expand_expr : (Loc.t -> string -> Ast.expr) -> Loc.t -> t -> Ast.expr;
  value expand_patt : (Loc.t -> string -> Ast.patt) -> Loc.t -> t -> Ast.patt;

  (** [dump_file] optionally tells Camlp4 to dump the
      result of an expander if this result is syntactically incorrect.
      If [None] (default), this result is not dumped. If [Some fname], the
      result is dumped in the file [fname]. *)
  value dump_file : ref (option string);

  module Error : Error.S;

end;
