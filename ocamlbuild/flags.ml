(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open Command
open Bool (* FIXME remove me *)
open Tags.Operators

type decl = {
  tags: Tags.t;
  flags: Command.spec;
}
let flags_of_decl { flags; _ } = flags
let tags_of_decl { tags; _ } = tags

let all_decls = ref []

let of_tags matched_tags =
  S begin
    List.fold_left begin fun acc { tags; flags } ->
      if Tags.does_match matched_tags tags then flags :: acc
      else acc
    end [] !all_decls
  end

let () = Command.tag_handler := of_tags

let of_tag_list x = of_tags (Tags.of_list x)

let add_decl decl =
  all_decls := decl :: !all_decls

let flag tags flags =
  let tags = Tags.of_list tags in
  add_decl { tags; flags }

let pflag tags ptag flags =
  Param_tags.declare ptag
    (fun param -> flag (Param_tags.make ptag param :: tags) (flags param))

let add x xs = x :: xs
let remove me = List.filter (fun x -> me <> x)

let show_documentation () =
  let pp fmt = Log.raw_dprintf (-1) fmt in
  List.iter begin fun { tags; flags } ->
    let sflag = Command.string_of_command_spec flags in
    pp "@[<2>flag@ {. %a .}@ %S@]@\n@\n" Tags.print tags sflag
  end !all_decls;
  pp "@."
