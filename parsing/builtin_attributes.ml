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
open Ast_helper


module Attribute_table = Hashtbl.Make (struct
  type t = string with_loc

  let hash : t -> int = Hashtbl.hash
  let equal : t -> t -> bool = (=)
end)
let unused_attrs = Attribute_table.create 128
let mark_used t = Attribute_table.remove unused_attrs t

(* [attr_order] is used to issue unused attribute warnings in the order the
   attributes occur in the file rather than the random order of the hash table
*)
let attr_order a1 a2 =
  match String.compare a1.loc.loc_start.pos_fname a2.loc.loc_start.pos_fname
  with
  | 0 -> Int.compare a1.loc.loc_start.pos_cnum a2.loc.loc_start.pos_cnum
  | n -> n

let warn_unused () =
  let keys = List.of_seq (Attribute_table.to_seq_keys unused_attrs) in
  let keys = List.sort attr_order keys in
  List.iter (fun sloc ->
    Location.prerr_warning sloc.loc (Warnings.Misplaced_attribute sloc.txt))
    keys

(* These are the attributes that are tracked in the builtin_attrs table for
   misplaced attribute warnings. *)
let builtin_attrs =
  [ "alert"; "ocaml.alert"
  ; "boxed"; "ocaml.boxed"
  ; "deprecated"; "ocaml.deprecated"
  ; "deprecated_mutable"; "ocaml.deprecated_mutable"
  ; "explicit_arity"; "ocaml.explicit_arity"
  ; "immediate"; "ocaml.immediate"
  ; "immediate64"; "ocaml.immediate64"
  ; "inline"; "ocaml.inline"
  ; "inlined"; "ocaml.inlined"
  ; "noalloc"; "ocaml.noalloc"
  ; "ppwarning"; "ocaml.ppwarning"
  ; "tailcall"; "ocaml.tailcall"
  ; "unboxed"; "ocaml.unboxed"
  ; "untagged"; "ocaml.untagged"
  ; "unrolled"; "ocaml.unrolled"
  ; "warnerror"; "ocaml.warnerror"
  ; "warning"; "ocaml.warning"
  ; "warn_on_literal_pattern"; "ocaml.warn_on_literal_pattern"
  ]

let builtin_attrs =
  let tbl = Hashtbl.create 128 in
  List.iter (fun attr -> Hashtbl.add tbl attr ()) builtin_attrs;
  tbl

let is_builtin_attr s = Hashtbl.mem builtin_attrs s

type attr_tracking_time = Parser | Invariant_check

let register_attr attr_tracking_time name =
  match attr_tracking_time with
  | Parser when !Clflags.all_ppx <> [] -> ()
  | Parser | Invariant_check ->
    if is_builtin_attr name.txt then
      Attribute_table.replace unused_attrs name ()

let string_of_cst = function
  | Pconst_string(s, _, _) -> Some s
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
                     ({pexp_desc=Pexp_constant(Pconst_string(msg,_,_))}, _)}
               ]) ->
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
                  ({pexp_desc=Pexp_constant(Pconst_string(msg,_,_))}, _)}::
             inner) ->
          let sub = List.map (submessage_from loc txt) inner in
          Location.error_of_printer ~loc ~sub Format.pp_print_text msg
      | _ ->
          Location.errorf ~loc "Invalid syntax for extension '%s'." txt
      end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let mark_alert_used a =
  match a.attr_name.txt with
  | "ocaml.deprecated"|"deprecated"|"ocaml.alert"|"alert" ->
    mark_used a.attr_name
  | _ -> ()

let mark_alerts_used l = List.iter mark_alert_used l

let mark_warn_on_literal_pattern_used l =
  List.iter (fun a ->
    match a.attr_name.txt with
    | "ocaml.warn_on_literal_pattern"|"warn_on_literal_pattern" ->
      mark_used a.attr_name
    | _ -> ())
    l

let mark_deprecated_mutable_used l =
  List.iter (fun a ->
    match a.attr_name.txt with
    | "ocaml.deprecated_mutable"|"deprecated_mutable" ->
      mark_used a.attr_name
    | _ -> ())
    l

let mark_payload_attrs_used payload =
  let iter =
    { Ast_iterator.default_iterator
      with attribute = fun self a ->
        mark_used a.attr_name;
        Ast_iterator.default_iterator.attribute self a
    }
  in
  iter.payload iter payload

let kind_and_message = function
  | PStr[
      {pstr_desc=
         Pstr_eval
           ({pexp_desc=Pexp_apply
                 ({pexp_desc=Pexp_ident{txt=Longident.Lident id}},
                  [Nolabel,{pexp_desc=Pexp_constant (Pconst_string(s,_,_))}])
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

let warn_payload loc txt msg =
  Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg))

let warning_attribute ?(ppwarning = true) =
  let process loc name errflag payload =
    mark_used name;
    match string_of_payload payload with
    | Some s ->
        begin try
          Option.iter (Location.prerr_alert loc)
            (Warnings.parse_options errflag s)
        with Arg.Bad msg -> warn_payload loc name.txt msg
        end
    | None ->
        warn_payload loc name.txt "A single string literal is expected"
  in
  let process_alert loc name = function
    | PStr[{pstr_desc=
              Pstr_eval(
                {pexp_desc=Pexp_constant(Pconst_string(s,_,_))},
                _)
           }] ->
        begin
          mark_used name;
          try Warnings.parse_alert_option s
          with Arg.Bad msg -> warn_payload loc name.txt msg
        end
    | k ->
        (* Don't [mark_used] in the [Some] cases - that happens in [Env] or
           [type_mod] if they are in a valid place.  Do [mark_used] in the
           [None] case, which is just malformed and covered by the "Invalid
           payload" warning. *)
        match kind_and_message k with
        | Some ("all", _) ->
            warn_payload loc name.txt "The alert name 'all' is reserved"
        | Some _ -> ()
        | None -> begin
            mark_used name;
            warn_payload loc name.txt "Invalid payload"
          end
  in
  function
  | {attr_name = {txt = ("ocaml.warning"|"warning"); _} as name;
     attr_loc;
     attr_payload;
     } ->
      process attr_loc name false attr_payload
  | {attr_name = {txt = ("ocaml.warnerror"|"warnerror"); _} as name;
     attr_loc;
     attr_payload
    } ->
      process attr_loc name true attr_payload
  | {attr_name = {txt="ocaml.ppwarning"|"ppwarning"; _} as name;
     attr_loc = _;
     attr_payload =
       PStr [
         { pstr_desc=
             Pstr_eval({pexp_desc=Pexp_constant (Pconst_string (s, _, _))},_);
           pstr_loc }
       ];
    } when ppwarning ->
    (mark_used name;
     Location.prerr_warning pstr_loc (Warnings.Preprocessor s))
  | {attr_name = {txt = ("ocaml.alert"|"alert"); _} as name;
     attr_loc;
     attr_payload;
     } ->
      process_alert attr_loc name attr_payload
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

let has_attribute nms attrs =
  List.exists
    (fun a ->
       if List.mem a.attr_name.txt nms
       then (mark_used a.attr_name; true)
       else false)
    attrs

type attr_action = Mark_used_only | Return
let filter_attributes actions attrs =
  List.filter (fun a ->
    List.exists (fun (nm, action) ->
      String.equal nm a.attr_name.txt &&
      begin
        mark_used a.attr_name;
        action = Return
      end)
      actions
  ) attrs

let warn_on_literal_pattern attrs =
  has_attribute ["ocaml.warn_on_literal_pattern"; "warn_on_literal_pattern"]
    attrs

let explicit_arity attrs =
  has_attribute ["ocaml.explicit_arity"; "explicit_arity"] attrs

let immediate attrs = has_attribute ["ocaml.immediate"; "immediate"] attrs

let immediate64 attrs = has_attribute ["ocaml.immediate64"; "immediate64"] attrs

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let has_unboxed attrs = has_attribute ["ocaml.unboxed"; "unboxed"] attrs

let has_boxed attrs = has_attribute ["ocaml.boxed"; "boxed"] attrs
