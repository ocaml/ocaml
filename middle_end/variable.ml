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
    var_unit : Compilation_unit.t;
    var_var : Ident.t
  }

  let compare v1 v2 =
    let c = Ident.compare v1.var_var v2.var_var in
    if c = 0
    then Compilation_unit.compare v1.var_unit v2.var_unit
    else c
  let output c v = Ident.output c v.var_var
  let hash v = Ident.hash v.var_var
  let equal v1 v2 =
    Ident.same v1.var_var v2.var_var &&
    Compilation_unit.equal v1.var_unit v2.var_unit
  let print ppf v =
    Format.fprintf ppf "%a.%a"
      Compilation_unit.print v.var_unit
      Ident.print v.var_var
end

include T
include Ext_types.Identifiable.Make (T)

let create ~current_compilation_unit name =
  { var_unit = current_compilation_unit;
    var_var = Ident.create name;
  }

let unwrap var = var.var_var
let unique_ident var =
  let open Ident in
  { var.var_var with
    name =
      Format.asprintf "%a_%s"
        Compilation_unit.print var.var_unit
        var.var_var.name;
  }

let rename ~current_compilation_unit ?append var =
  let var_var =
    match append with
    | None -> Ident.rename var.var_var
    | Some s -> Ident.create (var.var_var.Ident.name ^ s) in
  { var_unit = current_compilation_unit;
    var_var;
  }

let in_compilation_unit cu var =
  Compilation_unit.equal cu var.var_unit

let get_compilation_unit var = var.var_unit

let unique_name var = Ident.unique_name var.var_var

let output_full c t =
  Compilation_unit.output c t.var_unit;
  Printf.fprintf c ".";
  Ident.output c t.var_var
