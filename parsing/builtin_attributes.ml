(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree

let string_of_cst = function
  | Pconst_string(s, _) -> Some s
  | _ -> None

let string_of_payload = function
  | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant c},_)}] ->
      string_of_cst c
  | _ -> None

let rec error_of_extension ext =
  match ext with
  | ({txt = ("ocaml.error"|"error") as txt; loc}, p) ->
    let rec sub_from inner =
      match inner with
      | {pstr_desc=Pstr_extension (ext, _)} :: rest ->
          error_of_extension ext :: sub_from rest
      | _ :: rest ->
          (Location.errorf ~loc
             "Invalid syntax for sub-error of extension '%s'." txt) ::
            sub_from rest
      | [] -> []
    in
    begin match p with
    | PStr({pstr_desc=Pstr_eval
              ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}::
           {pstr_desc=Pstr_eval
              ({pexp_desc=Pexp_constant(Pconst_string(if_highlight,_))}, _)}::
           inner) ->
        Location.error ~loc ~if_highlight ~sub:(sub_from inner) msg
    | PStr({pstr_desc=Pstr_eval
              ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}::inner) ->
        Location.error ~loc ~sub:(sub_from inner) msg
    | _ -> Location.errorf ~loc "Invalid syntax for extension '%s'." txt
    end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let rec deprecated_of_attrs = function
  | [] -> None
  | ({txt = "ocaml.deprecated"|"deprecated"; _}, p) :: _ ->
      begin match string_of_payload p with
      | Some txt ->  Some txt
      | None -> Some ""
      end
  | _ :: tl -> deprecated_of_attrs tl

let check_deprecated loc attrs s =
  match deprecated_of_attrs attrs with
  | None -> ()
  | Some "" -> Location.prerr_warning loc (Warnings.Deprecated s)
  | Some txt ->
      Location.prerr_warning loc (Warnings.Deprecated (s ^ "\n" ^ txt))

let rec check_deprecated_mutable loc attrs s =
  match attrs with
  | [] -> ()
  | ({txt = "ocaml.deprecated_mutable"|"deprecated_mutable"; _}, p) :: _ ->
      let txt =
        match string_of_payload p with
        | Some txt -> "\n" ^ txt
        | None -> ""
      in
      Location.prerr_warning loc
        (Warnings.Deprecated (Printf.sprintf "mutating field %s%s"
           s txt))
  | _ :: tl -> check_deprecated_mutable loc tl s

let rec deprecated_of_sig = function
  | {psig_desc = Psig_attribute a} :: tl ->
      begin match deprecated_of_attrs [a] with
      | None -> deprecated_of_sig tl
      | Some _ as r -> r
      end
  | _ -> None


let rec deprecated_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      begin match deprecated_of_attrs [a] with
      | None -> deprecated_of_str tl
      | Some _ as r -> r
      end
  | _ -> None


let emit_external_warnings =
  (* Note: this is run as a preliminary pass when type-checking an
     interface or implementation.  This allows to cover all kinds of
     attributes, but the drawback is that it doesn't take local
     configuration of warnings (with '@@warning'/'@@warnerror'
     attributes) into account.  We should rather check for
     'ppwarning' attributes during the actual type-checking, making
     sure to cover all contexts (easier and more ugly alternative:
     duplicate here the logic which control warnings locally). *)
  let open Ast_iterator in
  {
    default_iterator with
    attribute = (fun _ a ->
        match a with
        | {txt="ocaml.ppwarning"|"ppwarning"},
          PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant
                                         (Pconst_string (s, _))},_);
                pstr_loc}] ->
            Location.prerr_warning pstr_loc (Warnings.Preprocessor s)
        | _ -> ()
      )
  }


let warning_scope = ref []

let warning_enter_scope () =
  warning_scope := (Warnings.backup ()) :: !warning_scope
let warning_leave_scope () =
  match !warning_scope with
  | [] -> assert false
  | hd :: tl ->
      Warnings.restore hd;
      warning_scope := tl

let warning_attribute attrs =
  let process loc txt errflag payload =
    match string_of_payload payload with
    | Some s ->
        begin try Warnings.parse_options errflag s
        with Arg.Bad _ ->
          Location.prerr_warning loc
            (Warnings.Attribute_payload
               (txt, "Ill-formed list of warnings"))
        end
    | None ->
        Location.prerr_warning loc
          (Warnings.Attribute_payload
             (txt, "A single string literal is expected"))
  in
  List.iter
    (function
      | ({txt = ("ocaml.warning"|"warning") as txt; loc}, payload) ->
          process loc txt false payload
      | ({txt = ("ocaml.warnerror"|"warnerror") as txt; loc}, payload) ->
          process loc txt true payload
      | _ ->
          ()
    )
    attrs

let with_warning_attribute attrs f =
  try
    warning_enter_scope ();
    warning_attribute attrs;
    let ret = f () in
    warning_leave_scope ();
    ret
  with exn ->
    warning_leave_scope ();
    raise exn


let warn_on_literal_pattern =
  List.exists
    (function
      | ({txt="ocaml.warn_on_literal_pattern"|"warn_on_literal_pattern"; _}, _)
        -> true
      | _ -> false
    )

let explicit_arity =
  List.exists
    (function
      | ({txt="ocaml.explicit_arity"|"explicit_arity"; _}, _) -> true
      | _ -> false
    )

let immediate =
  List.exists
    (function
      | ({txt="ocaml.immediate"|"immediate"; _}, _) -> true
      | _ -> false
    )

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let check l (x, _) = List.mem x.txt l

let has_unboxed attr =
  List.exists (check ["ocaml.unboxed"; "unboxed"])
    attr

let has_boxed attr =
  List.exists (check ["ocaml.boxed"; "boxed"]) attr
