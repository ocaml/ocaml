(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Joel Reymont                                     *)
(*                                                                        *)
(*   Copyright 2024 Joel Reymont                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  start : Code_address.t;
  end_ : Code_address.t;
}

let create ~start ~end_ =
  { start; end_ }

let start t = t.start

let end_ t = t.end_

let contains t addr =
  Code_address.compare t.start addr <= 0 &&
  Code_address.compare addr t.end_ < 0

let overlaps t1 t2 =
  (* Two ranges overlap if either starts within the other *)
  contains t1 t2.start || contains t2 t1.start ||
  (* Or if one completely contains the other *)
  (Code_address.compare t1.start t2.start <= 0 &&
   Code_address.compare t2.end_ t1.end_ <= 0) ||
  (Code_address.compare t2.start t1.start <= 0 &&
   Code_address.compare t1.end_ t2.end_ <= 0)

let to_string t =
  Printf.sprintf "[%s, %s)"
    (Code_address.to_string t.start)
    (Code_address.to_string t.end_)

let print ppf t =
  Format.fprintf ppf "%s" (to_string t)

let compare t1 t2 =
  let c = Code_address.compare t1.start t2.start in
  if c <> 0 then c
  else Code_address.compare t1.end_ t2.end_

let equal t1 t2 =
  Code_address.equal t1.start t2.start &&
  Code_address.equal t1.end_ t2.end_
