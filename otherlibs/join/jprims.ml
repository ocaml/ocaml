(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(*
  Abstract type of join-definitions.
  Basically, a join-definition contains
    * Some messages queues, one per defined channel, sorted by
      increasing channel indexes (0, 1, ... )
    * Some ``matches'', one per defined channel, sorted by
      increasing channel indexes (0, 1, ... )
    * Some ``guards'' one per guarded process, sorted by
      increasing guarded process indexes (0,1, ...)

  The numbers of channels and guarded processes are given while creating
  the join-definitions.
*)

type automaton

(*
  Match tables, they are vectors, containing integers,
    m1, i1, m2, i2, etc.
    where - mi is a matching pattern, encoded as a bit field of
            channel indexes.
          - ii is the index of te match guard
*)
type matches

(* Guards are functions.
Given a clause
   chan_1 arg_1 & chan_2 arg_2 & ... & chan_n arg_n = P
(elementary join patterns are sorted by increasing channel indexes)
the guard is somthing as:
   (fun pat_1 pat_2 ... pat_n -> compilation of P)
where pat_i is
   - arg_i when chan_i is asynchronous.
   - (cont_i, arg_i) when chan_i is synchronous.
*)
type guard

(* Type of continuations for synchronous channels *)
type continuation

type thread

type internal_thread = {
    condition : Condition.t;
    mutable body : unit -> unit;
    mutable th_loc: location;
    mutable running : bool;
    mutable thread : Thread.t;
  }
  
type internal_location = {
    mutable father : location;
    mutable halted : bool;
    mutable threads_spool: thread list;
    mutable threads: thread list;
    mutable locations_spool : location list;
    mutable locations: location list;
    mutable automatons: automaton Weakset.t;
  }

type internal_automaton = {
    mutable auto_loc : location;  
  }
  
(* Functions to swap between stubs and local objects. Maybe this should
  be the same function ? *)
    
external internal_location : 
  location -> internal_location = "%identity" (* "caml_in_location" *)
external make_location : 
  internal_location -> location = "%identity" (* "caml_make_location" *)

external internal_thread : 
  thread -> internal_thread = "%identity" (* "caml_in_thread" *)
external make_thread : 
  internal_thread -> thread = "%identity" (* "caml_make_thread" *)

external internal_automaton : 
  automaton -> internal_automaton = "%identity" (* "caml_in_automaton" *)
external make_automaton : 
  internal_automaton -> automaton = "%identity" (* "caml_make_automaton" *)

(*********************************************************)
  
let join_lock = Mutex.create ()
  
let threads_spool = ref []

let super_location =
  let l = {
    father = Obj.magic 0;
    halted = false;
    threads_spool = [];
    threads = [];
    locations_spool = [];
      locations = [];
      automatons = Weakset.create 100;
    } in
  l.father <- make_location l;
  l.father

let super_thread = 
  let t = {
    condition =  Condition.create ();
    body = (fun _ -> ());
    th_loc = super_location;
    running = true;
    thread = Thread.self ();
    } in
  make_thread t

let self_id () =
  Thread.id (Thread.self ())

let _ = 
  Thread.set_context (Thread.self ()) super_thread;
  (internal_location super_location).threads <- [super_thread]
  
let unsafe_self_location () =
  (internal_thread ((Thread.get_context (Thread.self ())) : thread)).th_loc
  
let self_location () =
  Mutex.lock join_lock;
  let location = unsafe_self_location () in
  Mutex.unlock join_lock;
  location

let create_location () =
  Mutex.lock join_lock;
  let father = unsafe_self_location () in
  let father_in = internal_location father in
  let location = match father_in.locations_spool with
      [] -> 
        let location = {
            father = father;
            halted = false;
            threads_spool = [];
            threads = [];
            locations_spool = [];
            locations = [];
            automatons = Weakset.create 100;
          } in
        let location = make_location location in
        father_in.locations <- location :: father_in.locations;
        location
    | location :: locations ->
        let l = internal_location location in
        father_in.locations_spool <- locations;
        l.halted <- false;
        location
  in
  Mutex.unlock join_lock;
  location

let create_process_location l f =
  Mutex.lock join_lock;
  let location = internal_location l in
  match location.threads_spool with
    t :: left ->
      
      let thread = internal_thread t in
      location.threads_spool <- left;
      thread.body <- f;
      thread.running <- true;
      Condition.signal thread.condition;
      Mutex.unlock join_lock
      
  | _ ->
      match !threads_spool with
        t :: left ->
          
          let thread = internal_thread t in          
          threads_spool := left;
          thread.body <- f;
          thread.th_loc <- l;
          location.threads <- t :: location.threads;
          thread.running <- true;
          Condition.signal thread.condition;
          Mutex.unlock join_lock
          
      | [] -> 

          let thread = {
              condition = Condition.create ();
              body = f;
              th_loc = l;
              running = false;
              thread = Obj.magic (); (* UGLY *)
            } in
      
          let t = make_thread thread in
          
          let th =
            Thread.create (fun _ ->
                Mutex.lock join_lock;
                while true do
                  if thread.running then begin
                      Mutex.unlock join_lock;
                      Printexc.catch thread.body ();
                      Mutex.lock join_lock;
                      thread.running <- false;
                      location.threads_spool <- t
                      :: 
                      location.threads_spool
                    end
                  else 
                    Condition.wait thread.condition join_lock;
                done) ()
          in
          
          thread.thread <- th;
          Thread.set_context th thread;
          location.threads <- t :: location.threads;
          thread.running <- true;
          Condition.signal thread.condition;
          Mutex.unlock join_lock
          
let create_process f =
  create_process_location (self_location ()) f

let halt_condition = Condition.create ()
let halt_locations = ref []
  
let halt () =
  Mutex.lock join_lock;
  let l = self_location () in
  let location = internal_location l in
  if not location.halted then begin
      location.halted <- true;
      halt_locations :=  l :: !halt_locations;
(* BUG: what happens if the location try to move at the same time ? *)
      Condition.signal halt_condition;
    end;
  Mutex.unlock join_lock;
  Thread.exit ()
  
let rec halt_location keep_threads l =
  if l == super_location then Pervasives.exit 0;
  
  let location = internal_location l in
  let father = internal_location location.father in
  father.locations_spool <- l :: father.locations_spool;
  
  location.halted <- true;  
  location.locations_spool <- [];
  List.iter (halt_location false) location.locations;
(* kill all running threads *)
  location.threads_spool <- [];
  List.iter (fun t -> 
      let thread = internal_thread t in
      if thread.running then
        Thread.kill thread.thread
      else
      if keep_threads then
        location.threads_spool <- t :: location.threads_spool
      else
        threads_spool := t :: !threads_spool
  ) location.threads;
  location.threads <- location.threads_spool
  
let _ = 
  Thread.create (fun _ ->
      Mutex.lock join_lock;
      while true do
        Condition.wait halt_condition join_lock;
        List.iter (halt_location true) !halt_locations;
        halt_locations := []
      done
  ) ()

(* How can I remove this thread from all runqueues ? 
There should be some way in Caml to do stack jumps without taking
try ... with ... into account.

The thread will remain in running status until a kill is issued.
*)
  
let exit () = Thread.exit ()
  
(* Change internal_location in location *)
  
let create_process_location l f =
  let location = internal_location l in
  create_process_location l f
  
(* TO BE IMPLEMENTED *)

external send_sync : automaton -> int -> 'a -> unit = "send_sync"
external send_async : automaton -> int -> 'a -> unit = "send_async"

(* create_automatons nchannels nguards *)
external create_automaton : int -> int -> automaton = "create_automaton"
external create_automaton_location : location -> int -> int -> automaton = "create_automaton_location"
external patch_match : automaton -> int -> matches -> unit = "patch_match"
external patch_guard : automaton -> int -> guard -> unit = "patch_guard"
external reply_to : 'a -> continuation -> unit = "reply_to"
