(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Type equality witness *)

type (_, _) eq = Equal: ('a, 'a) eq

(* Type identifiers *)

module Id = struct
  type _ id = ..
  module type ID = sig
    type t
    type _ id += Id : t id
  end

  type 'a t = (module ID with type t = 'a)

  let make () (type s) =
    let module T = struct
      type t = s
      type _ id += Id : t id end
    in
    (module T : ID with type t = s)

  let equal (type t0) (type t1) (t0 : t0 t) (t1 : t1 t) : (t0, t1) eq option
    =
    let module T0 = (val t0 : ID with type t = t0) in
    let module T1 = (val t1 : ID with type t = t1) in
    match T0.Id with T1.Id -> Some Equal | _ -> None
end
