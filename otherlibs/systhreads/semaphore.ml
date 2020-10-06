(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Xavier Leroy, Collège de France and INRIA Paris               *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Semaphores *)

type t = {
  mut: Mutex.t;                         (* protects [v] *)
  mutable v: int;                       (* the current value *)
  capacity: int option;                 (* the maximal value *)
  nonzero: Condition.t                  (* signaled when [v > 0] *)
}

let make ?capacity v =
  let cap = Option.value capacity ~default:max_int in
  if v < 0 || v > cap then
    raise (Sys_error "Semaphore.init: wrong initial value");
  if cap <= 0 then
    raise (Sys_error "Semaphore.init: wrong capacity");
  { mut = Mutex.create(); v; capacity; nonzero = Condition.create() }

let make_binary b =
  { mut = Mutex.create();
    v = if b then 1 else 0;
    capacity = Some 1;
    nonzero = Condition.create() }

let release s =
  Mutex.lock s.mut;
  begin match s.capacity with
    | None ->
        if s.v < max_int then
          s.v <- s.v + 1
        else begin
          Mutex.unlock s.mut;
          raise (Sys_error "Semaphore.release: overflow")
        end
    | Some cap ->
        if s.v < cap then
          s.v <- s.v + 1
  end;
  Condition.signal s.nonzero;
  Mutex.unlock s.mut

let acquire s =
  Mutex.lock s.mut;
  while s.v = 0 do Condition.wait s.nonzero s.mut done;
  s.v <- s.v - 1;
  Mutex.unlock s.mut

let try_acquire s =
  Mutex.lock s.mut;
  let ret = if s.v = 0 then false else (s.v <- s.v - 1; true) in
  Mutex.unlock s.mut;
  ret

