(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Inclusion checks for the class language *)

open Types

let class_types env cty1 cty2 =
  Ctype.match_class_types env cty1 cty2

let class_type_declarations env cty1 cty2 =
  Ctype.match_class_declarations env
    cty1.clty_params cty1.clty_type
    cty2.clty_params cty2.clty_type

let class_declarations env cty1 cty2 =
  match cty1.cty_new, cty2.cty_new with
    None, Some _ ->
      [Ctype.CM_Virtual_class]
  | _ ->
      Ctype.match_class_declarations env
        cty1.cty_params cty1.cty_type
        cty2.cty_params cty2.cty_type

open Format
open Ctype

let include_err =
  function
    CM_Virtual_class ->
      print_string "A class cannot be changed from virtual to concrete"
  | CM_Parameter_arity_mismatch (ls, lp) ->
      print_string
        "The classes do not have the same number of type parameters"     
  | CM_Type_parameter_mismatch trace ->
      open_box 0;
      Printtyp.unification_error false trace
        (function () ->
          print_string "One type parameter has type")
        (function () ->
          print_string "but is expected to have type");
      close_box ()
  | CM_Class_type_mismatch (cty1, cty2) ->
      open_box 0;
      print_string "The class type";  print_break 1 2;
      Printtyp.class_type cty1;
      print_space ();
      print_string "is not matched by the class type";
      print_break 1 2;
      Printtyp.class_type cty2;
      close_box ()
  | CM_Parameter_mismatch trace ->
      open_box 0;
      Printtyp.unification_error false trace
        (function () ->
          print_string "One parameter has type")
        (function () ->
          print_string "but is expected to have type");
      close_box ()
  | CM_Val_type_mismatch (lab, trace) ->
      open_box 0;
      Printtyp.unification_error false trace
        (function () ->
          print_string "The instance variable ";
          print_string lab; print_space ();
          print_string "has type")
        (function () ->
          print_string "but is expected to have type");
      close_box ()
  | CM_Meth_type_mismatch (lab, trace) ->
      open_box 0;
      Printtyp.unification_error false trace
        (function () ->
          print_string "The method ";
          print_string lab; print_space ();
          print_string "has type")
        (function () ->
          print_string "but is expected to have type");
      close_box ()
  | CM_Non_mutable_value lab ->
      open_box 0;
      print_string "The non-mutable instance variable ";
      print_string lab;
      print_string " cannot become mutable";
      close_box ()
  | CM_Missing_value lab ->
      open_box 0;
      print_string "The first class type has no instance variable ";
      print_string lab;
      close_box ()
  | CM_Missing_method lab ->
      open_box 0;
      print_string "The first class type has no method ";
      print_string lab;
      close_box ()
  | CM_Hide_public lab ->
      open_box 0;
      print_string "The public method ";
      print_string lab;
      print_string " cannot be hidden";
      close_box ()
  | CM_Hide_virtual lab ->
      open_box 0;
      print_string "The virtual method ";
      print_string lab;
      print_string " cannot be hidden";
      close_box ()
  | CM_Public_method lab ->
      open_box 0;
      print_string "The public method ";
      print_string lab;
      print_string " cannot become private";
      close_box ()
  | CM_Virtual_method lab ->
      open_box 0;
      print_string "The virtual method ";
      print_string lab;
      print_string " cannot become concrete";
      close_box ()
  | CM_Private_method lab ->
      open_box 0;
      print_string "The private method ";
      print_string lab;
      print_string " cannot become public";
      close_box ()

let report_error errlist =
  match errlist with
    [] -> ()
  | err :: rem ->
      open_vbox 0;
      include_err err;
      List.iter (fun err -> print_space(); include_err err) rem;
      close_box()
