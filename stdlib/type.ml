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

  type !'a t = (module ID with type t = 'a)

  let make (type a) () : a t =
    (module struct type t = a type _ id += Id : t id end)

  let[@inline] uid (type a) ((module A) : a t) =
    Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)

  let provably_equal
      (type a b) ((module A) : a t) ((module B) : b t) : (a, b) eq option
    =
    match A.Id with B.Id -> Some Equal | _ -> None
end
