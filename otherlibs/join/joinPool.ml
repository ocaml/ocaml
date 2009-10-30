(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)


(* Enumerations *)

type ('a, 'b) enum =
  { start : unit -> 'a;
    step : 'a -> ('b * 'a) option; }

let enum_of_interval inf sup =
  { start = (fun () -> inf);
    step = (fun x -> if x <= sup then Some (x, succ x) else None) }

let enum_of_list l =
  { start = (fun () -> l);
    step = (function hd :: tl -> Some (hd, tl) | [] -> None) }


(* Pools *)

type ('a, 'b, 'c) t =
  { register : ('a -> 'b) Join.chan;
    wait : unit -> 'c; }

let create e comb y0 =

  def loop(monitor,enum) & agent(worker) = match e.step enum with
    | Some (x,next) ->
        let id = monitor.JoinCount.Monitor.enter(x) in
        loop(monitor,next) &
        call_worker(monitor, id, x, worker)
    | None ->
        do_again(monitor) & agent(worker)

  or do_again(monitor) & agent(worker) = (* re-issue pending tasks *)
    begin match monitor.JoinCount.Monitor.get_pendings() with
    | [] -> monitor.JoinCount.Monitor.finished()
    | xs -> again(monitor,xs)
    end &
    agent(worker)

  (* Re-perform tasks *)
  or again(monitor,(id,x)::xs) & agent(worker) =
    again(monitor,xs) &
    if monitor.JoinCount.Monitor.is_pending(id) then begin
      call_worker(monitor,id,x,worker)
    end else begin
      agent(worker)
    end

  or again(monitor,[]) = do_again(monitor)

  or agent(worker) & compute(monitor,id,x) =
    call_worker(monitor,id,x,worker)

  and call_worker(monitor,id,x,worker) =
    let r = try Some (worker(x)) with _ -> None in
    match r with
    | None -> compute(monitor,id,x)
    | Some v ->
        monitor.JoinCount.Monitor.leave(id,v) ;
        agent(worker)

  in
  let wait () =
    let monitor = JoinCount.Monitor.create comb y0 in
    spawn loop(monitor, e.start ()) ;
    monitor.JoinCount.Monitor.wait ()

  in
  { register = agent ; wait = wait ; }

