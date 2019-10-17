(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inclusion checks for the class language *)

open Types

let class_types env cty1 cty2 =
  Ctype.match_class_types env cty1 cty2

let class_type_declarations ~loc env cty1 cty2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:cty1.clty_loc
    ~use:cty2.clty_loc
    loc
    cty1.clty_attributes cty2.clty_attributes
    (Path.last cty1.clty_path);
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

(*
let rec hide_params = function
    Tcty_arrow ("*", _, cty) -> hide_params cty
  | cty -> cty
*)

let include_err ppf =
  function
  | CM_Virtual_class ->
      I18n.fprintf ppf "A class cannot be changed from virtual to concrete"
  | CM_Parameter_arity_mismatch _ ->
      I18n.fprintf ppf
        "The classes do not have the same number of type parameters"
  | CM_Type_parameter_mismatch (env, trace) ->
      Printtyp.report_unification_error ppf env trace
        (I18n.dprintf "A type parameter has type")
        (I18n.dprintf "but is expected to have type")
  | CM_Class_type_mismatch (env, cty1, cty2) ->
      Printtyp.wrap_printing_env ~error:true env (fun () ->
        I18n.fprintf ppf
          "@[The class type@;<1 2>%a@ is not matched by \
           the class type@;<1 2>%a@]"
          Printtyp.class_type cty1
          Printtyp.class_type cty2)
  | CM_Parameter_mismatch (env, trace) ->
      Printtyp.report_unification_error ppf env trace
        (I18n.dprintf "A parameter has type")
        (I18n.dprintf "but is expected to have type")
  | CM_Val_type_mismatch (lab, env, trace) ->
      Printtyp.report_unification_error ppf env trace
        (I18n.dprintf "The instance variable %s@ has type" lab)
        (I18n.dprintf "but is expected to have type")
  | CM_Meth_type_mismatch (lab, env, trace) ->
      Printtyp.report_unification_error ppf env trace
        (I18n.dprintf "The method %s@ has type" lab)
        (I18n.dprintf "but is expected to have type")
  | CM_Non_mutable_value lab ->
      I18n.fprintf ppf
       "@[The non-mutable instance variable %s cannot become mutable@]" lab
  | CM_Non_concrete_value lab ->
      I18n.fprintf ppf
       "@[The virtual instance variable %s cannot become concrete@]" lab
  | CM_Missing_value lab ->
      I18n.fprintf ppf
       "@[The first class type has no instance variable %s@]" lab
  | CM_Missing_method lab ->
      I18n.fprintf ppf "@[The first class type has no method %s@]" lab
  | CM_Hide_public lab ->
     I18n.fprintf ppf "@[The public method %s cannot be hidden@]" lab
  | CM_Hide_virtual (k, lab) ->
      I18n.fprintf ppf "@[The virtual %s %s cannot be hidden@]" k lab
  | CM_Public_method lab ->
      I18n.fprintf ppf "@[The public method %s cannot become private@]" lab
  | CM_Virtual_method lab ->
      I18n.fprintf ppf "@[The virtual method %s cannot become concrete@]" lab
  | CM_Private_method lab ->
      I18n.fprintf ppf "@[The private method %s cannot become public@]" lab

let report_error ppf = function
  |  [] -> ()
  | err :: errs ->
      let print_errs ppf errs =
         List.iter (fun err -> fprintf ppf "@ %a" include_err err) errs in
      fprintf ppf "@[<v>%a%a@]" include_err err print_errs errs
