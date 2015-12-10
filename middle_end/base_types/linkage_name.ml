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
    name : string;
    hash : int;
  }

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let c = t1.hash - t2.hash in
      if c <> 0 then c
      else compare t1.name t2.name

  let equal t1 t2 =
    if t1 == t2 then true
    else
      t1.hash = t2.hash
        && Pervasives.compare t1.name t2.name = 0

  let hash t = t.hash
  let print ppf t = Format.fprintf ppf "%s" t.name
  let output chan t = output_string chan t.name
end

include T
include Ext_types.Identifiable.Make (T)

let create t =
  { name = t;
    hash = Hashtbl.hash t;
  }

let to_string t = t.name
