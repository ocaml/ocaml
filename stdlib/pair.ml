(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b) t = 'a * 'b

let make a b = (a, b)
let fst (a, _) = a
let snd (_, b) = b
let swap (a, b) = (b, a)

let fold f (a, b) = f a b
let map f g (a, b) = (f a, g b)
let iter f g (a, b) = f a; g b
let map_fst f (a, b) = (f a, b)
let map_snd g (a, b) = (a, g b)

let equal eqa eqb (a, b) (a', b')  =
  eqa a a' && eqb b b'
let compare cmpa cmpb (a, b) (a', b')  =
   let c = cmpa a a' in
   if c <> 0 then c
   else cmpb b b'
