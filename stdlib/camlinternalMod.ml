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

external make_forward : Obj.t -> Obj.t -> unit = "caml_obj_make_forward"

type shape =
  | Function
  | Lazy
  | Class
  | Module of shape array
  | Value of Obj.t

let overwrite o n =
  assert (Obj.size o >= Obj.size n);
  for i = 0 to Obj.size n - 1 do
    Obj.set_field o i (Obj.field n i)
  done

let overwrite_closure o n =
  (* We need to use the [raw_field] functions at least on the code
     pointer, which is not a valid value in -no-naked-pointers
     mode. *)
  assert (Obj.tag n = Obj.closure_tag);
  assert (Obj.size o >= Obj.size n);
  let n_start_env = Obj.Closure.((info n).start_env) in
  let o_start_env = Obj.Closure.((info o).start_env) in
  (* if the environment of n starts before the one of o,
     clear the raw fields in between. *)
  for i = n_start_env to o_start_env - 1 do
    Obj.set_raw_field o i Nativeint.one
  done;
  (* if the environment of o starts before the one of n,
     clear the environment fields in between. *)
  for i = o_start_env to n_start_env - 1 do
    Obj.set_field o i (Obj.repr ())
  done;
  for i = 0 to n_start_env - 1 do
    (* code pointers, closure info fields, infix headers *)
    Obj.set_raw_field o i (Obj.raw_field n i)
  done;
  for i = n_start_env to Obj.size n - 1 do
    (* environment fields *)
    Obj.set_field o i (Obj.field n i)
  done;
  for i = Obj.size n to Obj.size o - 1 do
    (* clear the leftover space *)
    Obj.set_field o i (Obj.repr ())
  done;
  ()

let rec init_mod loc shape =
  match shape with
  | Function ->
      (* Two code pointer words (curried and full application), arity
         and eight environment entries makes 11 words. *)
      let closure = Obj.new_block Obj.closure_tag 11 in
      let template =
        Obj.repr (fun _ -> raise (Undefined_recursive_module loc))
      in
      overwrite_closure closure template;
      closure
  | Lazy ->
      Obj.repr (lazy (raise (Undefined_recursive_module loc)))
  | Class ->
      Obj.repr (CamlinternalOO.dummy_class loc)
  | Module comps ->
      Obj.repr (Array.map (init_mod loc) comps)
  | Value v ->
      v

let rec update_mod shape o n =
  match shape with
  | Function ->
      (* In bytecode, the RESTART instruction checks the size of closures.
         Hence, the optimized case [overwrite o n] is valid only if [o] and
         [n] have the same size.  (See PR#4008.)
         In native code, the size of closures does not matter, so overwriting
         is possible so long as the size of [n] is no greater than that of [o].
      *)
      if Obj.tag n = Obj.closure_tag
      && (Obj.size n = Obj.size o
          || (Sys.backend_type = Sys.Native
              && Obj.size n <= Obj.size o))
      then begin overwrite_closure o n end
      else overwrite_closure o (Obj.repr (fun x -> (Obj.obj n : _ -> _) x))
  | Lazy ->
      if Obj.tag n = Obj.lazy_tag then
        Obj.set_field o 0 (Obj.field n 0)
      else if Obj.tag n = Obj.forward_tag then begin (* PR#4316 *)
        make_forward o (Obj.field n 0)
      end else begin
        (* forwarding pointer was shortcut by GC *)
        make_forward o n
      end
  | Class ->
      assert (Obj.tag n = 0 && Obj.size n = 4);
      overwrite o n
  | Module comps ->
      assert (Obj.tag n = 0 && Obj.size n >= Array.length comps);
      for i = 0 to Array.length comps - 1 do
        update_mod comps.(i) (Obj.field o i) (Obj.field n i)
      done
  | Value _ -> () (* the value is already there *)
