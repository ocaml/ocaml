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

type sem = {
  mut: Mutex.t;                         (* protects [v] *)
  mutable v: int;                       (* the current value *)
  nonzero: Condition.t                  (* signaled when [v > 0] *)
}

module Counting = struct

type t = sem

let make v =
  if v < 0 then invalid_arg "Semaphore.Counting.init: wrong initial value";
  { mut = Mutex.create(); v; nonzero = Condition.create() }

let release s =
  Mutex.lock s.mut;
  if s.v < max_int then begin
    s.v <- s.v + 1;
    Condition.signal s.nonzero;
    Mutex.unlock s.mut
  end else begin
    Mutex.unlock s.mut;
    raise (Sys_error "Semaphore.Counting.release: overflow")
  end

let release_multi s n =
  if n < 1 then invalid_arg "release_multi"
  else if n = 1 then release s
  else (
    Mutex.lock s.mut;
    if s.v + n < max_int then begin
      s.v <- s.v + n;
      Condition.broadcast s.nonzero;
      Mutex.unlock s.mut
    end else begin
      Mutex.unlock s.mut;
      raise (Sys_error "Semaphore.Counting.release_multi: overflow")
    end
  )

let acquire s =
  Mutex.lock s.mut;
  while s.v = 0 do Condition.wait s.nonzero s.mut done;
  s.v <- s.v - 1;
  Mutex.unlock s.mut

let acquire_multi s n =
  if n < 1 then invalid_arg "Semaphore.Counting.acquire_multi"
  else if n = 1 then acquire s
  else (
    Mutex.lock s.mut;
    while s.v < n do Condition.wait s.nonzero s.mut done;
    s.v <- s.v - n;
    Mutex.unlock s.mut
  )

let try_acquire s =
  Mutex.lock s.mut;
  let ret = if s.v = 0 then false else (s.v <- s.v - 1; true) in
  Mutex.unlock s.mut;
  ret

let get_value s = s.v

let with_acquire s f =
  acquire s;
  Fun.protect f
    ~finally:(fun () -> release s)

let with_acquire_multi s n f =
  acquire_multi s n;
  Fun.protect f
    ~finally:(fun () -> release_multi s n)
end

module Binary = struct

type t = sem

let make b =
  { mut = Mutex.create();
    v = if b then 1 else 0;
    nonzero = Condition.create() }

let release s =
  Mutex.lock s.mut;
  s.v <- 1;
  Condition.signal s.nonzero;
  Mutex.unlock s.mut

let acquire s =
  Mutex.lock s.mut;
  while s.v = 0 do Condition.wait s.nonzero s.mut done;
  s.v <- 0;
  Mutex.unlock s.mut

let try_acquire s =
  Mutex.lock s.mut;
  let ret = if s.v = 0 then false else (s.v <- 0; true) in
  Mutex.unlock s.mut;
  ret

let with_acquire s f =
  acquire s;
  Fun.protect
    ~finally:(fun () -> release s)
    f

end
