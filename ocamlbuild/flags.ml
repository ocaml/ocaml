(***********************************************************************)
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
let all_flags = ref []

let of_tags tags =
  S begin
    List.fold_left begin fun acc (xtags, xflags) ->
      if Tags.does_match tags xtags then xflags :: acc
      else acc
    end [] !all_flags
  end

let () = Command.tag_handler := of_tags

let of_tag_list x = of_tags (Tags.of_list x)

let set_flags tags flags =
  all_flags := (tags, flags) :: !all_flags

let flag tags flags = set_flags (Tags.of_list tags) flags

let add x xs = x :: xs
let remove me = List.filter (fun x -> me <> x)
let to_spec l =
  S begin
    List.fold_right begin fun (x, y) acc ->
      A ("-"^x) :: A y :: acc
    end l []
  end

let get_flags () = !all_flags
