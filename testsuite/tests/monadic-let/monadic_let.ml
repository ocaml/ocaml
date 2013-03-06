(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                           Wojciech Meyer                            *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

module IdentityMonad = struct
  let bind x f = f x
  let ret x = x
  let fail x = ret
end

module ListMonad = struct
  let bind x f = f x
  let ret x = x
  let fail x = ret
end

module ExceptionMonad = struct
  let bind x f = match x with Some x -> f x | None -> None
  let ret x = Some x
  let fail s = failwith s
end

let foo =
  let* x = 123 in
  x + 4

let () =
  IdentityMonad.
  (let* x = [1;2;3] in
   let* y = [4;5;6] in
   let* _ = [] in
   ret x @ y)

let () =
  ListMonad.
  (let* x = [1;2;3] in
   let* y = [4;5;6] in
   let* _ = [] in
   ret x + y)
