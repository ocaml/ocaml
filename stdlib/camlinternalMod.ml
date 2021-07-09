(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Xavier Leroy, projet Cristal, INRIA Rocquencourt              *)
(*                                                                        *)
(*   Copyright 2004 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type shape =
  | Function
  | Lazy
  | Class
  | Module of shape array
  | Value of Obj.t

let rec init_mod_field modu i loc shape =
  let init =
    match shape with
    | Function ->
       let rec fn (x : 'a) =
         let fn' : 'a -> 'b = Obj.obj (Obj.field modu i) in
         if fn == fn' then
           raise (Undefined_recursive_module loc)
         else
           fn' x in
       Obj.repr fn
    | Lazy ->
       let rec l =
         lazy (
           let l' = Obj.obj (Obj.field modu i) in
           if l == l' then
             raise (Undefined_recursive_module loc)
           else
             Lazy.force l') in
       Obj.repr l
    | Class ->
       Obj.repr (CamlinternalOO.dummy_class loc)
    | Module comps ->
       Obj.repr (init_mod_block loc comps)
    | Value v -> v
  in
  Obj.set_field modu i init

and init_mod_block loc comps =
  let length = Array.length comps in
  let modu = Obj.new_block 0 length in
  for i = 0 to length - 1 do
    init_mod_field modu i loc comps.(i)
  done;
  modu

let init_mod loc shape =
  match shape with
  | Module comps ->
     Obj.repr (init_mod_block loc comps)
  | _ -> failwith "CamlinternalMod.init_mod: not a module"

let rec update_mod_field modu i shape n =
  match shape with
  | Function | Lazy ->
     Obj.set_field modu i n
  | Value _ ->
     () (* the value is already there *)
  | Class ->
     assert (Obj.tag n = 0 && Obj.size n = 4);
     let cl = Obj.field modu i in
     for j = 0 to 3 do
       Obj.set_field cl j (Obj.field n j)
     done
  | Module comps ->
     update_mod_block comps (Obj.field modu i) n

and update_mod_block comps o n =
  assert (Obj.tag n = 0 && Obj.size n >= Array.length comps);
  for i = 0 to Array.length comps - 1 do
    update_mod_field o i comps.(i) (Obj.field n i)
  done

let update_mod shape o n =
  match shape with
  | Module comps ->
     update_mod_block comps o n
  | _ -> failwith "CamlinternalMod.update_mod: not a module"
