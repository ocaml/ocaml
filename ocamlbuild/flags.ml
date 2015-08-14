(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open Command
open Bool (* FIXME remove me *)
open Tags.Operators

type decl = {
  tags: Tags.t;
  flags: Command.spec;
  deprecated: bool;
}
let flags_of_decl { flags; _ } = flags
let tags_of_decl { tags; _ } = tags

let all_decls = ref []

let of_tags matched_tags =
  S begin
    List.fold_left begin fun acc { tags; flags; _ } ->
      if Tags.does_match matched_tags tags then flags :: acc
      else acc
    end [] !all_decls
  end

let () = Command.tag_handler := of_tags

let of_tag_list x = of_tags (Tags.of_list x)

let add_decl decl =
  all_decls := decl :: !all_decls

let flag ?(deprecated=false) tags flags =
  let tags = Tags.of_list tags in
  add_decl { tags; flags; deprecated }

let pflags_doc = ref []

let pflag tags ptag ?doc_param flags =
  pflags_doc := (tags, ptag, doc_param, flags) :: !pflags_doc;
  Param_tags.declare ptag
    (fun param -> flag (Param_tags.make ptag param :: tags) (flags param))

let add x xs = x :: xs
let remove me = List.filter (fun x -> me <> x)

let pretty_print header tags sflag =
  let pp fmt = Log.raw_dprintf (-1) fmt in
  pp "@[<2>%s@ {. %a .}@ %S@]@\n@\n" header Tags.print tags sflag

let pretty_print_flag { tags; flags; deprecated } =
  let sflag = Command.string_of_command_spec flags in
  let header = if deprecated then "deprecated flag" else "flag" in
  pretty_print header tags sflag

let pretty_print_pflag (tags, ptag, doc_param, flags) =
  let apply tag param = Printf.sprintf "%s(%s)" tag param in
  let header = "parametrized flag" in
  let doc_param = match doc_param with
      | None -> "X"
      | Some param -> param in
  let tags = Tags.of_list (apply ptag doc_param :: tags) in
  let sflag =
    try Command.string_of_command_spec (flags doc_param)
    with _ -> "<fun>" in
  pretty_print header tags sflag

let show_documentation () =
  List.iter pretty_print_pflag !pflags_doc;
  List.iter
    (fun decl -> if not decl.deprecated then pretty_print_flag decl)
    !all_decls;
  List.iter
    (fun decl -> if decl.deprecated then pretty_print_flag decl)
    !all_decls;
  let pp fmt = Log.raw_dprintf (-1) fmt in
  pp "@."

let used_tags = ref Tags.empty

let mark_tag_used tag =
  used_tags := Tags.add tag !used_tags

let get_used_tags () =
  List.fold_left (fun acc decl -> Tags.union acc decl.tags)
    !used_tags !all_decls
