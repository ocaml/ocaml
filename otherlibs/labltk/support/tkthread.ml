(***********************************************************************)
(*                                                                     *)
(*              LablTk, Tcl/Tk interface of Objective Caml             *)
(*                                                                     *)
(*         Jacques Garrigue, Nagoya University Mathematics Dept.       *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let jobs : (unit -> unit) Queue.t = Queue.create ()
let m = Mutex.create ()
let with_jobs f =
  Mutex.lock m; let y = f jobs in Mutex.unlock m; y

let loop_id = ref None
let reset () = loop_id := None
let cannot_sync () =
  match !loop_id with None -> true
  | Some id -> Thread.id (Thread.self ()) = id

let gui_safe () =
  not (Sys.os_type = "Win32") || !loop_id = Some(Thread.id (Thread.self ()))

let has_jobs () = not (with_jobs Queue.is_empty)
let n_jobs () = with_jobs Queue.length
let do_next_job () = with_jobs Queue.take ()
let async j x = with_jobs (Queue.add (fun () -> j x))
let sync f x =
  if cannot_sync () then f x else
  let m = Mutex.create () in
  let res = ref None in
  Mutex.lock m;
  let c = Condition.create () in
  let j x =
    let y = f x in Mutex.lock m; res := Some y; Mutex.unlock m;
    Condition.signal c
  in
  async j x;
  Condition.wait c m;
  match !res with Some y -> y | None -> assert false

let rec job_timer () =
  Timer.set ~ms:10 ~callback:
    (fun () -> for i = 1 to n_jobs () do do_next_job () done; job_timer())

let thread_main () =
  try
    ignore (Protocol.openTk());
    job_timer();
    loop_id := Some (Thread.id (Thread.self ()));
    Protocol.mainLoop();
    loop_id := None;
  with exn ->
    loop_id := None;
    raise exn

let start () =
  Thread.create thread_main ()

let top = Widget.default_toplevel
