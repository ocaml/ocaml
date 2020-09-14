(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Xavier Leroy and Pascal Cuoq, INRIA Rocquencourt             *)
(*                                                                        *)
(*   Copyright 1995 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type status =
  | Unlocked
  | Locked of Thread.t list  (* list of all waiters *)

type t = status Atomic.t

let create () =
  Atomic.make Unlocked

let rec lock m =
  let cur = Atomic.get m in
  match cur with
  | Unlocked ->
      if not (Atomic.compare_and_set m cur (Locked [])) then lock m
  | Locked l ->
      if not (Atomic.compare_and_set m cur (Locked (Thread.self() :: l)))
      then lock m
      else begin
        Thread.suspend ();
        lock m
      end

let rec try_lock m =
  let cur = Atomic.get m in
  match cur with
  | Unlocked ->
      if not (Atomic.compare_and_set m cur (Locked []))
      then try_lock m
      else true
  | Locked _ ->
      false

let rec unlock m =
  let cur = Atomic.get m in
  if not (Atomic.compare_and_set m cur Unlocked) then unlock m else begin
    match cur with
    | Unlocked -> ()   (* tolerance... *)
    | Locked l -> List.iter Thread.notify l
  end
