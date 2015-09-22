(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*              Jacques Garrigue, Nagoya University                    *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

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
