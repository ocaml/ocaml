(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Typechecking of type expressions for the core language *)

open Parsetree
open Typedtree
open Ctype

exception Already_bound

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Longident.t
  | Type_arity_mismatch of Longident.t * int * int

exception Error of Location.t * error

(* Translation of type expressions *)

let type_variables = ref (Tbl.empty : (string, type_expr) Tbl.t)

let reset_type_variables () =
  type_variables := Tbl.empty

let enter_type_variable name =
  try
    Tbl.find name !type_variables; raise Already_bound
  with Not_found ->
    let v = newvar() in
    type_variables := Tbl.add name v !type_variables;
    v

let rec transl_simple_type env fixed styp =
  match styp.ptyp_desc with
    Ptyp_var name ->
      begin try
        Tbl.find name !type_variables
      with Not_found ->
        if fixed then
          raise(Error(styp.ptyp_loc, Unbound_type_variable name))
        else begin
          let v = newvar() in
          type_variables := Tbl.add name v !type_variables;
          v
        end
      end
  | Ptyp_arrow(st1, st2) ->
      Tarrow(transl_simple_type env fixed st1,
             transl_simple_type env fixed st2)
  | Ptyp_tuple stl ->
      Ttuple(List.map (transl_simple_type env fixed) stl)
  | Ptyp_constr(lid, stl) ->
      let (path, decl) =
        try
          Env.lookup_type lid env
        with Not_found ->
          raise(Error(styp.ptyp_loc, Unbound_type_constructor lid)) in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, Type_arity_mismatch(lid, decl.type_arity,
                                                           List.length stl)));
      Tconstr(path, List.map (transl_simple_type env fixed) stl)

let transl_type_scheme env styp =
  reset_type_variables();
  begin_def();
  let typ = transl_simple_type env false styp in
  end_def();
  generalize typ;
  typ

(* Error report *)

open Format
open Printtyp

let report_error = function
    Unbound_type_variable name ->
      print_string "Unbound type parameter "; print_string name
  | Unbound_type_constructor lid ->
      print_string "Unbound type constructor "; longident lid
  | Type_arity_mismatch(lid, expected, provided) ->
      open_hovbox 0;
      print_string "The type constructor "; longident lid;
      print_space(); print_string "expects "; print_int expected;
      print_string " argument(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " argument(s)";
      close_box()
