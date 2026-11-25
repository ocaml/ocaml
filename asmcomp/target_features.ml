(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Zane Hambly                               *)
(*                                                                        *)
(*   Copyright 2026 Zane Hambly                                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = {
  arch : string;
  features : (string * string) list;
  levels : (string * string list) list;
  config : string;
  mutable level : string list option;
  mutable overrides : (string * bool) list;
}

let known t name = List.mem_assoc name t.features

let add name set = if List.mem name set then set else name :: set
let remove name set = List.filter (fun f -> f <> name) set

let apply source t set s =
  let item set s =
    match String.trim s with
    | "" -> set
    | name when List.mem_assoc name t.levels -> List.assoc name t.levels
    | name ->
        let on, feature =
          if String.starts_with ~prefix:"no-" name
          then false, String.sub name 3 (String.length name - 3)
          else true, name
        in
        if not (known t feature) then
          Location.raise_errorf "Unknown %s target feature %s in %s"
            t.arch name source;
        if on then add feature set else remove feature set
  in
  List.fold_left item set (String.split_on_char ',' s)

let create ~arch ~features ~levels config =
  let t = { arch; features; levels; config; level = None; overrides = [] } in
  let check (level, fs) =
    List.iter (fun f ->
        if not (known t f) then
          Misc.fatal_errorf "Level %s names unknown %s target feature %s"
            level arch f)
      fs
  in
  List.iter check levels;
  t

let enabled t name =
  if not (known t name) then
    Misc.fatal_errorf "Unknown %s target feature %s" t.arch name;
  let ocamlparam = List.fold_left (apply "OCAMLPARAM" t) in
  let set = apply "Config.target_features" t [] t.config in
  let set = ocamlparam set !Clflags.target_features_first in
  let set = Option.value t.level ~default:set in
  let set =
    List.fold_left (fun set (f, on) -> if on then add f set else remove f set)
      set (List.rev t.overrides)
  in
  let set = ocamlparam set !Clflags.target_features_last in
  List.mem name set

let set t name b =
  t.overrides <- (name, b) :: List.remove_assoc name t.overrides

let command_line_options t =
  let level =
    "-ftarget-level",
    Arg.Symbol (List.map fst t.levels,
                fun l -> t.level <- Some (List.assoc l t.levels)),
    Printf.sprintf
      " Use the %s instruction set extensions of the given level" t.arch
  in
  let feature (name, doc) =
    [ "-f" ^ name, Arg.Unit (fun () -> set t name true), " Use " ^ doc;
      "-fno-" ^ name, Arg.Unit (fun () -> set t name false),
      " Do not use " ^ doc ]
  in
  level :: List.concat_map feature t.features
