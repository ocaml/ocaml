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

  let make () (type s) : s t =
    (module struct type t = s type _ id += Id : t id end)

  let some_equal = Some Equal

  let equal (type a) (type b)
      ((module A) : a t) ((module B) : b t) : (a, b) eq option
    =
    match A.Id with B.Id -> some_equal | _ -> None
end
