(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*               Jacques Garrigue, Nagoya University                      *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* PR#6435 *)

module F (M : sig
           type t
           module Set : Set.S with type elt = t
         end) =
struct
 let test set = Printf.printf "%d\n" (M.Set.cardinal set)
end

module M = F (Offset)

let () = M.test (Offset.M.Set.singleton "42")
let v = Pr6726.Test.v
