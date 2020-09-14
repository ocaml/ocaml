(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1995 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Thread.t list Atomic.t

let create () =
  Atomic.make []

let rec wait cond mut =
  let cur = Atomic.get cond in
  if not (Atomic.compare_and_set cond cur (Thread.self() :: cur))
  then wait cond mut
  else begin
    Mutex.unlock mut;
    Thread.suspend ();
    Mutex.lock mut
  end

let rec signal cond =
  let cur = Atomic.get cond in
  let next = match cur with [] -> [] | _ :: t -> t in
  if not (Atomic.compare_and_set cond cur next)
  then signal cond
  else begin
    match cur with
    | [] -> ()
    | h :: _ -> Thread.notify h
  end

let rec broadcast cond =
  let cur = Atomic.get cond in
  if not (Atomic.compare_and_set cond cur [])
  then broadcast cond
  else List.iter Thread.notify cur
