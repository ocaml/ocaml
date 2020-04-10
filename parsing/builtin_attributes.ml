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

let string_of_opt_payload p =
  match string_of_payload p with
  | Some s -> s
  | None -> ""

let error_of_extension ext =
  let submessage_from main_loc main_txt = function
    | {pstr_desc=Pstr_extension
           (({txt = ("ocaml.error"|"error"); loc}, p), _)} ->
        begin match p with
        | PStr([{pstr_desc=Pstr_eval
                     ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}]) ->
            { Location.loc; txt = fun ppf -> Format.pp_print_text ppf msg }
        | _ ->
            { Location.loc; txt = fun ppf ->
                Format.fprintf ppf
                  "Invalid syntax for sub-message of extension '%s'." main_txt }
        end
    | {pstr_desc=Pstr_extension (({txt; loc}, _), _)} ->
        { Location.loc; txt = fun ppf ->
            Format.fprintf ppf "Uninterpreted extension '%s'." txt }
    | _ ->
        { Location.loc = main_loc; txt = fun ppf ->
            Format.fprintf ppf
              "Invalid syntax for sub-message of extension '%s'." main_txt }
  in
  match ext with
  | ({txt = ("ocaml.error"|"error") as txt; loc}, p) ->
      begin match p with
      | PStr [] -> raise Location.Already_displayed_error
      | PStr({pstr_desc=Pstr_eval
                  ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}::
             inner) ->
          let sub = List.map (submessage_from loc txt) inner in
          Location.error_of_printer ~loc ~sub Format.pp_print_text msg
      | _ ->
          Location.errorf ~loc "Invalid syntax for extension '%s'." txt
      end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let kind_and_message = function
  | PStr[
      {pstr_desc=
         Pstr_eval
           ({pexp_desc=Pexp_apply
                 ({pexp_desc=Pexp_ident{txt=Longident.Lident id}},
                  [Nolabel,{pexp_desc=Pexp_constant (Pconst_string(s,_))}])
            },_)}] ->
      Some (id, s)
  | PStr[
      {pstr_desc=
         Pstr_eval
           ({pexp_desc=Pexp_ident{txt=Longident.Lident id}},_)}] ->
      Some (id, "")
  | _ -> None

let cat s1 s2 =
  if s2 = "" then s1 else s1 ^ "\n" ^ s2

let alert_attr x =
  match x.attr_name.txt with
  | "ocaml.deprecated"|"deprecated" ->
      Some (x, "deprecated", string_of_opt_payload x.attr_payload)
  | "ocaml.alert"|"alert" ->
      begin match kind_and_message x.attr_payload with
      | Some (kind, message) -> Some (x, kind, message)
      | None -> None (* note: bad payloads detected by warning_attribute *)
      end
  | _ -> None

let alert_attrs l =
  List.filter_map alert_attr l

let alerts_of_attrs l =
  List.fold_left
    (fun acc (_, kind, message) ->
       let upd = function
         | None | Some "" -> Some message
         | Some s -> Some (cat s message)
       in
       Misc.Stdlib.String.Map.update kind upd acc
    )
    Misc.Stdlib.String.Map.empty
    (alert_attrs l)

let check_alerts loc attrs s =
  Misc.Stdlib.String.Map.iter
    (fun kind message -> Location.alert loc ~kind (cat s message))
    (alerts_of_attrs attrs)

let check_alerts_inclusion ~def ~use loc attrs1 attrs2 s =
  let m2 = alerts_of_attrs attrs2 in
  Misc.Stdlib.String.Map.iter
    (fun kind msg ->
       if not (Misc.Stdlib.String.Map.mem kind m2) then
         Location.alert ~def ~use ~kind loc (cat s msg)
    )
    (alerts_of_attrs attrs1)

let rec deprecated_mutable_of_attrs = function
  | [] -> None
  | {attr_name =  {txt = "ocaml.deprecated_mutable"|"deprecated_mutable"; _};
     attr_payload = p} :: _ ->
     Some (string_of_opt_payload p)
  | _ :: tl -> deprecated_mutable_of_attrs tl

let check_deprecated_mutable loc attrs s =
  match deprecated_mutable_of_attrs attrs with
  | None -> ()
  | Some txt ->
      Location.deprecated loc (Printf.sprintf "mutating field %s" (cat s txt))

let check_deprecated_mutable_inclusion ~def ~use loc attrs1 attrs2 s =
  match deprecated_mutable_of_attrs attrs1,
        deprecated_mutable_of_attrs attrs2
  with
  | None, _ | Some _, Some _ -> ()
  | Some txt, None ->
      Location.deprecated ~def ~use loc
        (Printf.sprintf "mutating field %s" (cat s txt))

let rec attrs_of_sig = function
  | {psig_desc = Psig_attribute a} :: tl ->
      a :: attrs_of_sig tl
  | _ ->
      []

let alerts_of_sig sg = alerts_of_attrs (attrs_of_sig sg)

let rec attrs_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      a :: attrs_of_str tl
  | _ ->
      []

let alerts_of_str str = alerts_of_attrs (attrs_of_str str)

let check_no_alert attrs =
  List.iter
    (fun (a, _, _) ->
       Location.prerr_warning a.attr_loc
         (Warnings.Misplaced_attribute a.attr_name.txt)
    )
    (alert_attrs attrs)

let warn_payload loc txt msg =
  Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg))

let warning_attribute ?(ppwarning = true) =
  let process loc txt errflag payload =
    match string_of_payload payload with
    | Some s ->
        begin try Warnings.parse_options errflag s
        with Arg.Bad msg -> warn_payload loc txt msg
        end
    | None ->
        warn_payload loc txt "A single string literal is expected"
  in
  let process_alert loc txt = function
    | PStr[{pstr_desc=
              Pstr_eval(
                {pexp_desc=Pexp_constant(Pconst_string(s,_))},
                _)
           }] ->
        begin try Warnings.parse_alert_option s
        with Arg.Bad msg -> warn_payload loc txt msg
        end
    | k ->
        match kind_and_message k with
        | Some ("all", _) ->
            warn_payload loc txt "The alert name 'all' is reserved"
        | Some _ -> ()
        | None -> warn_payload loc txt "Invalid payload"
  in
  function
  | {attr_name = {txt = ("ocaml.warning"|"warning") as txt; _};
     attr_loc;
     attr_payload;
     } ->
      process attr_loc txt false attr_payload
  | {attr_name = {txt = ("ocaml.warnerror"|"warnerror") as txt; _};
     attr_loc;
     attr_payload
    } ->
      process attr_loc txt true attr_payload
  | {attr_name = {txt="ocaml.ppwarning"|"ppwarning"; _};
     attr_loc = _;
     attr_payload =
       PStr [
         { pstr_desc=
             Pstr_eval({pexp_desc=Pexp_constant (Pconst_string (s, _))},_);
           pstr_loc }
       ];
    } when ppwarning ->
     Location.prerr_warning pstr_loc (Warnings.Preprocessor s)
  | {attr_name = {txt = ("ocaml.alert"|"alert") as txt; _};
     attr_loc;
     attr_payload;
     } ->
      process_alert attr_loc txt attr_payload
  | _ ->
     ()

let warning_scope ?ppwarning attrs f =
  let prev = Warnings.backup () in
  try
    List.iter (warning_attribute ?ppwarning) (List.rev attrs);
    let ret = f () in
    Warnings.restore prev;
    ret
  with exn ->
    Warnings.restore prev;
    raise exn


let warn_on_literal_pattern =
  List.exists
    (fun a -> match a.attr_name.txt with
       | "ocaml.warn_on_literal_pattern"|"warn_on_literal_pattern" -> true
       | _ -> false
    )

let explicit_arity =
  List.exists
    (fun a -> match a.attr_name.txt with
       | "ocaml.explicit_arity"|"explicit_arity" -> true
       | _ -> false
    )

let immediate =
  List.exists
    (fun a -> match a.attr_name.txt with
       | "ocaml.immediate"|"immediate" -> true
       | _ -> false
    )

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let check l a = List.mem a.attr_name.txt l

let has_unboxed attr =
  List.exists (check ["ocaml.unboxed"; "unboxed"])
    attr

let has_boxed attr =
  List.exists (check ["ocaml.boxed"; "boxed"]) attr
