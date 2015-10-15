(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module T = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    ident : Ident.t;
  }

  let compare v1 v2 =
    let c = Ident.compare v1.ident v2.ident in
    if c = 0
    then Compilation_unit.compare v1.compilation_unit v2.compilation_unit
    else c
  let output c v = Ident.output c v.ident
  let hash v = Ident.hash v.ident
  let equal v1 v2 =
    Ident.same v1.ident v2.ident &&
    Compilation_unit.equal v1.compilation_unit v2.compilation_unit
  let print ppf v =
    Format.fprintf ppf "%a.%a"
      Compilation_unit.print v.compilation_unit
      Ident.print v.ident
end

include T
include Ext_types.Identifiable.Make (T)

let create ?current_compilation_unit name =
  let compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  { compilation_unit;
    ident = Ident.create name;
  }

let of_ident ident = create (Ident.name ident)

let unwrap t = t.ident

let unique_ident t =
  { t.ident with
    name =
      Format.asprintf "%a_%s"
        Compilation_unit.print t.compilation_unit
        t.ident.name;
  }

let rename ?current_compilation_unit ?append t =
  let compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  let ident =
    match append with
    | None -> Ident.rename t.ident
    | Some s -> Ident.create (t.ident.Ident.name ^ s)
  in
  { compilation_unit = compilation_unit;
    ident;
  }

let freshen t =
  rename t ~current_compilation_unit:(Compilation_unit.get_current_exn ())

let in_compilation_unit cu t =
  Compilation_unit.equal cu t.compilation_unit

let get_compilation_unit t = t.compilation_unit

let unique_name t = Ident.unique_name t.ident

let output_full c t =
  Compilation_unit.output c t.compilation_unit;
  Printf.fprintf c ".";
  Ident.output c t.ident

let print_list ppf ts =
  List.iter (fun t -> Format.fprintf ppf "@ %a" print t) ts

let debug_when_stamp_matches t ~stamp ~f =
  if t.ident.stamp = stamp then f ()

let print_opt ppf = function
  | None -> Format.fprintf ppf "<no var>"
  | Some t -> print ppf t

type pair = t * t
module Pair = Ext_types.Identifiable.Make (Ext_types.Pair (T) (T))

let compare_lists l1 l2 = Misc.compare_lists compare l1 l2
