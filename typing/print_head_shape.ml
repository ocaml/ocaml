(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Partout, INRIA Saclay                        *)
(*   Nicolas Chataing, ENS Paris                                          *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Head_shape_types

let doc_or_any doc_those ppf = function
  | Any -> Format_doc.fprintf ppf "Any"
  | Those t -> doc_those ppf t

let doc_list doc_elem ppf li =
  Format_doc.fprintf ppf "[%a]"
    (Format_doc.pp_print_list
       ~pp_sep:(fun ppf () -> Format_doc.fprintf ppf ";@ ")
       doc_elem)
    li

let doc_imm ppf (Imm n) =
  Format_doc.fprintf ppf "%d" n

let doc_size ppf (Size s) =
  Format_doc.fprintf ppf "%d" s

let doc_tag ppf (Tag tag) =
  let special_tags = [
    Obj.forcing_tag, "forcing";
    Obj.cont_tag, "cont";
    Obj.lazy_tag, "lazy";
    Obj.closure_tag, "closure";
    Obj.object_tag, "object";
    Obj.infix_tag, "infix";
    Obj.forward_tag, "forward";
    Obj.abstract_tag, "abstract";
    Obj.string_tag, "string";
    Obj.double_tag, "double";
    Obj.double_array_tag, "double_array";
    Obj.custom_tag, "custom";
  ] in
  match List.assoc tag special_tags with
  | name -> Format_doc.fprintf ppf "%s" name
  | exception Not_found -> Format_doc.fprintf ppf "%d" tag

let doc_imm_set ppf imm_set =
  doc_or_any (fun ppf set ->
    doc_list doc_imm ppf (ImmSet.to_list set)
  ) ppf imm_set

let doc_size_set ppf = function
  | Any -> ()
  | Those sizes ->
      Format_doc.fprintf ppf "@ size@ %a"
        (doc_list doc_size) (SizeSet.to_list sizes)

let doc_tag_map ppf tag_map =
  let doc_binding ppf (tag, sizes) =
    Format_doc.fprintf ppf "%a%a"
      doc_tag tag
      doc_size_set sizes
  in
  doc_list doc_binding ppf (TagMap.to_list tag_map)

let doc_block_set ppf block_set =
  doc_or_any doc_tag_map ppf block_set

let doc ppf {imms; blocks} =
  Format_doc.fprintf ppf "@[{imm = @[%a@];@ blocks = @[%a@];}@]"
    doc_imm_set imms
    doc_block_set blocks

let pp ppf sh = Format_doc.compat doc ppf sh



let print_type_declaration ~shape_of_type_path ppf env tydecl =
  let open Typedtree in
  (* compute the head shape *)
  let head_shape = shape_of_type_path env (Path.Pident tydecl.typ_id) in
  (* format it, with the name of the type declaration first *)
  let any_params =
    (* we want a trailing space unless this is empty *)
    match tydecl.typ_params with
    | [] -> ""
    | [_] -> "_ "
    | _::rest -> (* [ty1; ty2] should give "(_, _) " *)
        ("(_" :: List.map (fun _ -> ", _") rest @ [") "])
        |> String.concat ""
  in
  Format.fprintf ppf "@[<2>shape of %s%s:@ %a@]@."
    any_params tydecl.typ_name.txt
    pp head_shape

let print_in_structure ~shape_of_type_path ppf st =
  let open Tast_iterator in
  let cur_final_env = ref None in
  let parent = default_iterator in
  let iterator =
    { parent with
      structure = begin fun self st ->
        cur_final_env := Some st.str_final_env;
        parent.structure self st
      end;
      structure_item = fun self it ->
        parent.structure_item self it;
        let env = Option.get !cur_final_env in
        let print_tydecl decl =
          print_type_declaration ~shape_of_type_path ppf env decl
        in
        match it.str_desc with
        | Tstr_type (_rec, tydecls) ->
            List.iter print_tydecl tydecls
        | _ -> ()
    }
  in iterator.structure iterator st

let print_in_signature ~shape_of_type_path ppf si =
  let open Tast_iterator in
  let cur_final_env = ref None in
  let parent = default_iterator in
  let iterator =
    { parent with
      signature = begin fun self si ->
        cur_final_env := Some si.sig_final_env;
        parent.signature self si
      end;
      signature_item = fun self it ->
        parent.signature_item self it;
        let env = Option.get !cur_final_env in
        let print_tydecl decl =
          print_type_declaration ~shape_of_type_path ppf env decl
        in
        match it.sig_desc with
        | Tsig_type (_rec, tydecls) ->
            List.iter print_tydecl tydecls
        | _ -> ()
    }
  in iterator.signature iterator si
