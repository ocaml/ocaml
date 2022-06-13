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

let compiler_stops_before_attributes_consumed () =
  let stops_before_lambda =
    match !Clflags.stop_after with
    | None -> false
    | Some pass -> Clflags.Compiler_pass.(compare pass Lambda) < 0
  in
  stops_before_lambda || !Clflags.print_types

let warn_unused () =
  let keys = List.of_seq (Attribute_table.to_seq_keys unused_attrs) in
  Attribute_table.clear unused_attrs;
  if not (compiler_stops_before_attributes_consumed ()) then
    let keys = List.sort attr_order keys in
    List.iter (fun sloc ->
      Location.prerr_warning sloc.loc (Warnings.Misplaced_attribute sloc.txt))
      keys

(* These are the attributes that are tracked in the builtin_attrs table for
   misplaced attribute warnings. *)
let builtin_attrs =
  [ "alert"
  ; "atomic"
  ; "boxed"
  ; "deprecated"
  ; "deprecated_mutable"
  ; "explicit_arity"
  ; "immediate"
  ; "immediate64"
  ; "inline"
  ; "inlined"
  ; "noalloc"
  ; "poll"
  ; "ppwarning"
  ; "remove_aliases"
  ; "specialise"
  ; "specialised"
  ; "tailcall"
  ; "tail_mod_cons"
  ; "unboxed"
  ; "untagged"
  ; "unrolled"
  ; "warnerror"
  ; "warning"
  ; "warn_on_literal_pattern"
  ]

let builtin_attrs =
  let tbl = Hashtbl.create 128 in
  List.iter (fun attr -> Hashtbl.add tbl attr ()) builtin_attrs;
  tbl

let drop_ocaml_attr_prefix s =
  let len = String.length s in
  if String.starts_with ~prefix:"ocaml." s && len > 6 then
    String.sub s 6 (len - 6)
  else
    s

let is_builtin_attr s = Hashtbl.mem builtin_attrs (drop_ocaml_attr_prefix s)

type current_phase = Parser | Invariant_check

let register_attr current_phase name =
  match current_phase with
  | Parser when !Clflags.all_ppx <> [] -> ()
  | Parser | Invariant_check ->
    if is_builtin_attr name.txt then
      Attribute_table.replace unused_attrs name ()

let string_of_cst const =
  match const.pconst_desc with
  | Pconst_string(s, _, _) -> Some s
  | _ -> None

let string_of_exp exp =
  match exp.pexp_desc with
  | Pexp_constant c -> string_of_cst c
  | _ -> None

let string_of_payload = function
  | PStr[{pstr_desc=Pstr_eval(e,_)}] ->
      string_of_exp e
  | _ -> None

let string_of_opt_payload p =
  match string_of_payload p with
  | Some s -> s
  | None -> ""

let int_of_cst const =
  match const.pconst_desc with
  | Pconst_integer(s, None) -> Some (int_of_string s)
  | _ -> None

let int_of_exp exp =
  match exp.pexp_desc with
  | Pexp_constant c -> int_of_cst c
  | _ -> None

let bool_of_exp exp =
  match exp.pexp_desc with
  | Pexp_construct ({txt = Longident.Lident "true" }, None) -> Some true
  | Pexp_construct ({txt = Longident.Lident "false"}, None) -> Some false
  | _ -> None

let list_of_exp exp =
  let rec loop acc = function
  | {pexp_desc = Pexp_construct ({txt = Longident.Lident "[]"; _}, None)} ->
      Ok (List.rev acc)
  | {pexp_desc =
       Pexp_construct (
         {txt = Longident.Lident "::"; _},
         Some {pexp_desc = Pexp_tuple [(None, e1); (None, e2)]}
     )} ->
      loop (e1 :: acc) e2
  | {pexp_loc = loc} ->
      Error loc
  in loop [] exp

let list_of_payload loc = function
  | PStr[{pstr_desc = Pstr_eval (li, _)}] ->
      list_of_exp li
  | _ -> Error loc

module Style = Misc.Style

let error_of_extension ext =
  let submessage_from main_loc main_txt = function
    | {pstr_desc=Pstr_extension
           (({txt = ("ocaml.error"|"error"); loc}, p), _)} ->
        begin match p with
        | PStr([{pstr_desc=Pstr_eval
                     ({pexp_desc=Pexp_constant
                           {pconst_desc=Pconst_string(msg, _, _); _}}, _)}
               ]) ->
            Location.msg ~loc "%a" Format_doc.pp_print_text msg
        | _ ->
            Location.msg ~loc "Invalid syntax for sub-message of extension %a."
              Style.inline_code main_txt
        end
    | {pstr_desc=Pstr_extension (({txt; loc}, _), _)} ->
        Location.msg ~loc "Uninterpreted extension '%a'."
          Style.inline_code txt
    | _ ->
        Location.msg ~loc:main_loc
          "Invalid syntax for sub-message of extension %a."
          Style.inline_code main_txt
  in
  match ext with
  | ({txt = ("ocaml.error"|"error") as txt; loc}, p) ->
      begin match p with
      | PStr [] -> raise Location.Already_displayed_error
      | PStr({pstr_desc=Pstr_eval
                  ({pexp_desc=Pexp_constant
                      {pconst_desc=Pconst_string(msg, _, _)}}, _)}::
             inner) ->
          let sub = List.map (submessage_from loc txt) inner in
          Location.error_of_printer ~loc ~sub Format_doc.pp_print_text msg
      | _ ->
          Location.errorf ~loc "Invalid syntax for extension '%s'." txt
      end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let attr_equals_builtin {attr_name = {txt; _}; _} s =
  (* Check for attribute s or ocaml.s.  Avoid allocating a fresh string. *)
  txt = s ||
  (   String.length txt = 6 + String.length s
   && String.starts_with ~prefix:"ocaml." txt
   && String.ends_with ~suffix:s txt)

let mark_alert_used a =
  if attr_equals_builtin a "deprecated" || attr_equals_builtin a "alert"
  then mark_used a.attr_name

let mark_alerts_used l = List.iter mark_alert_used l

let mark_warn_on_literal_pattern_used l =
  List.iter (fun a ->
    if attr_equals_builtin a "warn_on_literal_pattern"
    then mark_used a.attr_name)
    l

let mark_deprecated_mutable_used l =
  List.iter (fun a ->
    if attr_equals_builtin a "deprecated_mutable"
    then mark_used a.attr_name)
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
                  [Nolabel,{pexp_desc=Pexp_constant
                                {pconst_desc=Pconst_string(s,_,_); _}}])
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
  if attr_equals_builtin x "deprecated" then
    Some (x, "deprecated", string_of_opt_payload x.attr_payload)
  else if attr_equals_builtin x "alert" then
    begin match kind_and_message x.attr_payload with
    | Some (kind, message) -> Some (x, kind, message)
    | None -> None (* note: bad payloads detected by warning_attribute *)
    end
  else None

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
  | attr :: _ when attr_equals_builtin attr "deprecated_mutable" ->
    Some (string_of_opt_payload attr.attr_payload)
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

let alerts_of_sig ~mark sg =
  let a = attrs_of_sig sg in
  if mark then mark_alerts_used a;
  alerts_of_attrs a

let rec attrs_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      a :: attrs_of_str tl
  | _ ->
      []

let alerts_of_str ~mark str =
  let a = attrs_of_str str in
  if mark then mark_alerts_used a;
  alerts_of_attrs a

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
                {pexp_desc=Pexp_constant {pconst_desc=Pconst_string(s,_,_); _}},
                _)
           }] ->
        begin
          mark_used name;
          try Warnings.parse_alert_option s
          with Arg.Bad msg -> warn_payload loc name.txt msg
        end
    | k ->
        match kind_and_message k with
        | Some ("all", _) ->
            warn_payload loc name.txt "The alert name 'all' is reserved"
        | Some _ ->
            (* Do [mark_used] in the [Some] case only if Warning 53 is
               disabled. Later, they will be marked used (provided they are in a
               valid place) in [compile_common], when they are extracted to be
               persisted inside the [.cmi] file. *)
            if not (Warnings.is_active (Misplaced_attribute ""))
            then mark_used name
        | None -> begin
            (* Do [mark_used] in the [None] case, which is just malformed and
               covered by the "Invalid payload" warning. *)
            mark_used name;
            warn_payload loc name.txt "Invalid payload"
          end
  in
  fun ({attr_name; attr_loc; attr_payload} as attr) ->
    if attr_equals_builtin attr "warning" then
      process attr_loc attr_name false attr_payload
    else if attr_equals_builtin attr "warnerror" then
      process attr_loc attr_name true attr_payload
    else if attr_equals_builtin attr "alert" then
      process_alert attr_loc attr_name attr_payload
    else if ppwarning && attr_equals_builtin attr "ppwarning" then
      begin match attr_payload with
      | PStr [{ pstr_desc=
                  Pstr_eval({pexp_desc=Pexp_constant
                                 {pconst_desc=Pconst_string (s, _, _); _}},_);
                pstr_loc }] ->
        (mark_used attr_name;
         Location.prerr_warning pstr_loc (Warnings.Preprocessor s))
      | _ ->
        (mark_used attr_name;
         warn_payload attr_loc attr_name.txt
           "A single string literal is expected")
      end

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

let has_attribute nm attrs =
  List.exists
    (fun a ->
       if attr_equals_builtin a nm
       then (mark_used a.attr_name; true)
       else false)
    attrs

type attr_action = Mark_used_only | Return
let select_attributes actions attrs =
  List.filter (fun a ->
    List.exists (fun (nm, action) ->
      attr_equals_builtin a nm &&
      begin
        mark_used a.attr_name;
        action = Return
      end)
      actions
  ) attrs

let warn_on_literal_pattern attrs =
  has_attribute "warn_on_literal_pattern" attrs

let explicit_arity attrs = has_attribute "explicit_arity" attrs

let immediate attrs = has_attribute "immediate" attrs

let immediate64 attrs = has_attribute "immediate64" attrs

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let has_unboxed attrs = has_attribute "unboxed" attrs

let has_boxed attrs = has_attribute "boxed" attrs

let has_remove_aliases attrs = has_attribute "remove_aliases" attrs

let has_atomic attrs = has_attribute "atomic" attrs

let find_shapes attrs : Asttypes.shape_name list option =
  let err loc msg =
    warn_payload loc "shape" msg;
    None
  in
  let shape_of_exp exp : Asttypes.shape_name option =
    let open Asttypes in
    let loc = exp.pexp_loc in
    match exp.pexp_desc with
    | Pexp_ident {loc; txt = Longident.Lident shape_name} ->
        let open Asttypes in
        begin match shape_name with
        | "any" -> Some Any
        | "int" -> Some Int
        | "float" -> Some Float
        | "string" -> Some String
        | "tuple" -> Some (Tuple { size = None })
        | "array" -> Some Array
        | "floatarray" -> Some Floatarray
        | "function" -> Some Function
        | "object" -> Some Object
        | "continuation" -> Some Continuation
        | "extensible_variant" -> Some Extensible_variant
        | "abstract" -> Some (Abstract { size = None })
        | "custom" -> Some (Custom { size = None })
        | _ ->
            Printf.ksprintf (err loc) "Unknown shape name %s." shape_name
        end
    | Pexp_apply ({pexp_desc = Pexp_ident {loc; txt = Longident.Lident name; _}}, args) ->
        let consume_arg arg_of_exp = function
          | (Nolabel, e) :: rest ->
              Option.map (fun v -> (v, rest)) (arg_of_exp e)
          | _ -> None
        in
        let consume_labelled_arg expected_label arg_of_exp = function
          | (Labelled label, e) :: rest ->
              if not (String.equal label expected_label) then None
              else Option.map (fun v -> (v, rest)) (arg_of_exp e)
          | _ -> None
        in
        begin match name with
        | "imm" ->
            begin match consume_arg int_of_exp args with
            | Some (n, []) -> Some (Imm n)
            | _ ->
              err loc "The 'imm' shape-former expects a literal integer, for example: imm 2."
            end
        | ("tuple" | "abstract" | "custom") ->
            let f size = match name with
              | "tuple" -> Tuple { size }
              | "abstract" -> Abstract { size }
              | "custom" -> Custom { size }
              | _ -> assert false
            in
            begin match consume_labelled_arg "size" int_of_exp args with
            | Some (size, []) -> Some (f (Some size))
            | _ ->
              Printf.ksprintf (err loc)
                "The '%s' shape-former excepts an optional ~size label \
                 with an integer argument, for example: %s ~size:3."
                name name
            end
        | "constructor" ->
            let fail () =
              err loc "The 'constructor' shape-former expects a tag \
                       argument followed by an optional ~size:n \
                       argument, for example: constructor 0, \
                       or constructor 3 ~size:2."
            in
            begin match consume_arg int_of_exp args with
            | None -> fail ()
            | Some (tag, []) -> Some (Constructor { tag; size = None })
            | Some (tag, args) ->
            match consume_labelled_arg "size" int_of_exp args with
            | Some (size, []) -> Some (Constructor {tag; size = Some size})
            | _ -> fail ()
            end
        | "polymorphic_variant" ->
            let fail () =
              err loc "The 'polymorphic_variant' shape-former expects \
                       two labelled boolean arguments, ~has_consts and \
                       ~has_nonconsts (in that order), for example: \
                       polymorphic_variant ~has_consts:true \
                       ~has_nonconsts:false."
            in
            begin match consume_labelled_arg "has_consts" bool_of_exp args with
            | None -> fail ()
            | Some (has_consts, args) ->
            match consume_labelled_arg "has_nonconsts" bool_of_exp args with
            | Some (has_nonconsts, []) ->
              Some (Polymorphic_variant { has_consts; has_nonconsts })
            | _ -> fail ()
            end
        | _ -> Printf.ksprintf (err loc) "Unsupported shape-former '%s'" name
        end
    | _ ->
        err loc "Unsupported shape format."
  in
  let shape_of_payload loc payload =
    match list_of_payload loc payload with
    | Error loc -> err loc "A shape list such as [int; lazy; custom] was expected."
    | Ok args -> Some (List.filter_map shape_of_exp args)
  in
  let find_shape_payload a =
    match a.attr_name.txt with
    | "shape" | "ocaml.shape" ->
        shape_of_payload a.attr_loc a.attr_payload
    | _ -> None
  in
  match List.filter_map find_shape_payload attrs with
  | [] -> None
  | (_ :: _) as shape_specs ->
      Some (List.fold_left List.rev_append [] shape_specs)
