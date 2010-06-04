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

open Printf

module Simple = struct

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

end

module Shared = struct

module type Config = sig
  val debug : bool
  val nagain : int
end

module type Enumerable = sig

  type t
  type elt
  type enum

  val start : t -> enum
  val step : enum -> (elt * enum) option
end


(** Standard workers *)
  type ('elt,'partial) worker = 'elt -> 'partial

(** Interuptible workers *)
  type subtask_id = int (** Subtask identifier *)
  type ('elt,'partial) interuptible_worker =
      subtask_id * 'elt -> 'partial option (** Worker proper *)
  type kill = subtask_id Join.chan (** Abort given subtask *)


  module type S = sig
  type elt         (** Element from a collection *)
  type collection  (** Collection *)

  type ('partial, 'result) t = {
      register : (elt,'partial) worker Join.chan;
      register_interuptible :
        ((elt,'partial) interuptible_worker * kill) Join.chan;
      fold : collection -> ('partial -> 'result -> 'result) -> 'result -> 'result;
    }
    val create : unit ->  ('partial, 'result) t 
  end

  module Make(C:Config) (E:Enumerable) = struct

    type elt = E.elt
    type collection = E.t

(** Standard workers *)
  type 'a worker = elt -> 'a

(** Interuptible workers *)
  type subtask_id = int (** Subtask identifier *)
  type 'a interuptible_worker =
      subtask_id * elt -> 'a option (** Worker proper *)
  type kill = subtask_id Join.chan (** Abort given subtask *)

  type ('partial, 'result) t = {
      register : 'partial worker Join.chan;
      register_interuptible : ('partial interuptible_worker * kill) Join.chan;
      fold : collection -> ('partial -> 'result -> 'result) -> 'result -> 'result;
    }

(* Record killers *)

    module K = struct
      type agent_id = int

      type t =
          { enter : (subtask_id * kill * int) -> agent_id ;
            leave : (subtask_id * agent_id) -> unit ;
              active : subtask_id -> (kill * int) list ; }

      let get_env id e =
        try Hashtbl.find e id 
        with Not_found -> []

      and replace_env id ks e = match ks with
      | [] -> Hashtbl.remove e id
      | _  -> Hashtbl.replace e id ks 

      let create () =

        def enter(id,k,n) & env(next_id,e) =
          let ks = get_env id e in
          replace_env id ((next_id,(k,n))::ks) e ;
          env(next_id+1,e) & reply next_id to enter
        or leave(id,tok) & env(next_id,e) =
          let ks =  get_env id e in
          let ks = List.remove_assoc tok ks in
          replace_env id ks e ;
          env (next_id,e) & reply to leave
        or active(id) & env(next_id,e) =
          let ks =  get_env id e in
          replace_env id [] e ;
          reply List.map snd ks to active & env(next_id,e) in

        spawn env(0,Hashtbl.create 17) ;
        { enter=enter; leave=leave; active=active; }
    end


type key = int

type ('b,'c) monitor =
  { enter : E.elt -> key ;
    leave : key * 'b -> unit ;
    is_active : key -> bool ;
    get_active : unit -> (key * E.elt) list ;
    wait : unit -> 'c ;
    finished : unit Join.chan ;
    killers : K.t ;
  }

let to_string active =
  String.concat ","
    (List.map  (fun (x,_) -> string_of_int x) active)

    let create_monitor gather default =
      def state(new_id, active, result) & enter(x) =
        state(new_id+1, (new_id,x)::active, result) &
        reply new_id to enter

      or state(new_id, active, result) & leave(id,v) =
        reply to leave &
        if List.mem_assoc id active then
          let result' = gather v result in
          let active'= List.remove_assoc id active in
          state(new_id, active', result')
        else
          state(new_id, active, result)

      or state(new_id, active, result) & is_active(id) =
        state(new_id, active, result) &
        let b = List.mem_assoc id active in
        reply b to is_active

      or state(new_id, active, result) & get_active() =
        if C.debug then Join.debug "DIST" "Get %s" (to_string active) ;
        state(new_id, active, result) &
        reply active to get_active

      or state(new_id, [], result) & wait() & finished() =
        state(new_id, [], result) & reply result to wait

      in spawn state(0, [], default) ;

      {  enter=enter ; leave=leave ;
         is_active=is_active ;
         get_active=get_active ;
         wait=wait; finished=finished ;
         killers = K.create () ; }

(* 2-levels queues *)
    type 'a queue = E | Q of ('a list * 'a list)

    let put c q = match q with
    | E -> Q ([c],[])
    | Q (xs,ys) -> Q (xs,c::ys)

    and put_front c q = match q with
    | E -> Q ([c],[])
    | Q (xs,ys) -> Q (c::xs,ys)

    let rec get = function
      | ([c],[])|([],[c]) -> c,E
      | (x::xs,ys) -> x,Q (xs,ys)
      | ([],(_::_ as ys)) -> get (List.rev ys,[])
      | ([],[]) -> assert false

(* Gensym *)

def st(next_id) & fresh_nounce() = st(next_id+1) & reply next_id to fresh_nounce

    let () = spawn st(0)

(* Pool proper *)      
    type 'a agent =
      | W of 'a worker
      | WK of 'a interuptible_worker * kill

    type priority = High | Low

    let create () =

      def pool(high,low) & addPool(c) = pool(put c high,low)

      or pool(Q high,low) & agent(worker) =
        let (monitor,enum),high = get high in
        match E.step enum with
        | Some (x,next) ->
            let id = monitor.enter(x) in
            pool(put (monitor,next) high,low) &
            call_worker(High,monitor, id, x, worker)
        | None ->
            agent(worker) &
            monitor.finished() &
            pool(high,put ([],C.nagain,monitor) low)


      (* Re-perform tasks *)
      or pool(E,Q low) & agent(worker) =
        let (xs,n,m),low = get low in
        match xs with 
        | (id,x)::xs ->
            pool (E,put_front (xs,n,m) low) &
            begin if m.is_active id then
              call_worker(Low,m,id,x,worker)
            else
              agent(worker)
            end
        | [] ->
            agent(worker) &
	    if n > 0 then begin
	      let again = m.get_active () in
	      match again with
              | [] ->
                  pool(E,low)
              | _  ->
                  pool(E,put (again,n-1,m) low)
            end else begin
              pool(E,low)
            end

      or compute(prio,monitor,id,x) & agent(worker) =
        call_worker(prio,monitor,id,x,worker)

      and call_worker(prio,monitor,id,x,worker) =
        match worker with
        | W w ->
            let r = try Some (w x) with _ -> None in
            begin match r with
            | None ->
                begin match prio with
                | High -> compute(prio,monitor,id,x)
                | Low -> 0
                end
            | Some v ->
                monitor.leave(id,v) ;
                agent(worker)
            end
        | WK (w,k) ->
            let nounce = fresh_nounce() in
            if C.debug then Join.debug "POOL" "WK<%i,%i>: Start%!" nounce id ;
            let tok = monitor.killers.K.enter(id,k,nounce) in
            let r = try Some (w (nounce,x)) with _ -> None in
            monitor.killers.K.leave(id,tok) ;
            begin match r with
            | None ->
                (* Distant client considered dead, forget agent,
                   re-issue high priority task *)
                if C.debug then Join.debug "POOL" "WK<%i,%i>: Dead%!" nounce id ;
                begin match prio with
                | High -> compute(prio,monitor,id,x)
                | Low -> 0
                end
            | Some None ->
                (* Distant client failed, do not re-issue task *)
                if C.debug then Join.debug "POOL" "WK<%i,%i>: None%!" nounce id ;
                agent(worker)
            | Some (Some v) ->
                if C.debug then Join.debug "POOL" "WK<%i,%i>: Some%!" nounce id ;
                monitor.leave(id,v) ;
                begin
                  let ks = monitor.killers.K.active(id) in
                  List.iter (fun (k,nounce) -> spawn k(nounce)) ks ;
                  0
                end & agent(worker)
            end
              

      in
      spawn pool(E,E) ;

      let fold sc gather default =
        let monitor = create_monitor gather default in
        spawn addPool(monitor, E.start sc) ;
        monitor.wait () in

      def register(worker) = agent(W worker)
      and register_interuptible(w,k) = agent(WK (w,k)) in
      { fold = fold ; register = register ;
        register_interuptible = register_interuptible ; }

  end

end
