(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Frederic Bour,             *)
(*                           Xavier Van de Woestyne                       *)
(*                                                                        *)
(*   Copyright 2025 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external reraise : exn -> 'a = "%reraise"

let with_saved_types ?save_part f =
  let saved_types = Cmt_format.get_saved_types () in
  Cmt_format.set_saved_types [];
  try
    let result = f () in
    begin
      match save_part with
      | None -> ()
      | Some f -> Cmt_format.set_saved_types (f result :: saved_types)
    end;
    result
  with exn when !Clflags.typing_recovery ->
    let saved_types' = Cmt_format.get_saved_types () in
    Cmt_format.set_saved_types (saved_types' @ saved_types);
    reraise exn


module Saved_parts = struct
  let attribute = Location.mknoloc "ocaml.saved-parts"

  module H = Ephemeron.K1.Make(
    struct
      type t = string
      let hash = Hashtbl.hash
      let equal = String.equal
    end)

  let table = H.create 7

  let gensym =
    let counter = ref 0 in
    fun () -> incr counter; !counter

  let find = function
    | Parsetree.Pconst_integer (id, None) ->
        begin
          try H.find table id
          with Not_found -> []
        end
    | _ -> assert false

  let store parts =
    let id = string_of_int (gensym ()) in
    let key = Parsetree.Pconst_integer (id, None) in
    H.add table id parts;
    key
end

let flush_saved_types () =
  match Cmt_format.get_saved_types () with
  | [] -> []
  | parts ->
    Cmt_format.set_saved_types [];
    let open Ast_helper in
    let pconst_desc = Saved_parts.store parts in
    let pexp = Exp.constant { pconst_desc; pconst_loc = !default_loc } in
    let pstr = Str.eval pexp in
    [ Attr.mk Saved_parts.attribute (Parsetree.PStr [ pstr ]) ]

let incorrect_attribute =
  Ast_helper.Attr.mk (Location.mknoloc "ocaml.incorrect") (Parsetree.PStr [])

let recovery_attributes attrs =
  let attrs' = incorrect_attribute :: flush_saved_types () in
  match attrs with
  | [] -> attrs'
  | attrs -> attrs' @ attrs

let rec erroneous_expr_check e =
  Typing_recovery.erroneous_type_check e.Typedtree.exp_type
  || match e.Typedtree.exp_desc with
  | Typedtree.Texp_ident (p, _, _) when Ident.name (Path.head p) = "_" -> true
  | Typedtree.Texp_apply (e', _) -> erroneous_expr_check e'
  | _ -> false

let rec get_saved_types_from_attributes = function
  | [] -> []
  | attr :: attrs ->
      if attr.Parsetree.attr_name = Saved_parts.attribute then
        let open Parsetree in
        begin match attr.Parsetree.attr_payload with
        | PStr
            ({ pstr_desc =
                 Pstr_eval
                   ({ pexp_desc = Pexp_constant
                          { pconst_desc = key; _ }; _ }, _);
               _
             }
             :: _) -> Saved_parts.find key
        | _ -> []
        end
      else get_saved_types_from_attributes attrs
