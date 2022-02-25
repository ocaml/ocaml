(* TEST
* hasunix
include unix
** bytecode
** native
*)

(*
 * Copyright (c) 2015, Theo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Michael-Scott queue *)

module type BS =  sig
  type t
  val create : ?max:int -> unit -> t
  val once : t -> unit
  val reset : t -> unit
end

module B : BS = struct

  type t = int * int ref

  let _ = Random.self_init ()

  let create ?(max=32) () = (max, ref 1)

  let once (maxv, r) =
    let t = Random.int (!r) in
    r := min (2 * !r) maxv;
    if t = 0 then ()
    else ignore (Unix.select [] [] [] (0.001 *. (float_of_int t)))

  let reset (_,r) = r := 1

end

(* TODO KC: Replace with concurrent lock free bag --
 * http://dl.acm.org/citation.cfm?id=1989550 *)


module type QS = sig
  type 'a t
  val create      : unit -> 'a t
  val is_empty    : 'a t -> bool
  val push        : 'a t -> 'a -> unit
  val pop         : 'a t -> 'a option
  val clean_until : 'a t -> ('a -> bool) -> unit

  type 'a cursor
  val snapshot  : 'a t -> 'a cursor
  val next      : 'a cursor -> ('a * 'a cursor) option
end

module Q : QS = struct

  type 'a node =
    | Nil
    | Next of 'a * 'a node Atomic.t

  type 'a t =
    { head : 'a node Atomic.t ;
      tail : 'a node Atomic.t }

  let create () =
    let head = (Next (Obj.magic (), Atomic.make Nil)) in
    { head = Atomic.make head ; tail = Atomic.make head }

  let is_empty q =
    match Atomic.get q.head with
    | Nil -> failwith "MSQueue.is_empty: impossible"
    | Next (_,x) ->
        ( match Atomic.get x with
          | Nil -> true
          | _ -> false )

  let pop q =
    let b = B.create () in
    let rec loop () =
      let s = Atomic.get q.head in
      let nhead = match s with
        | Nil -> failwith "MSQueue.pop: impossible"
        | Next (_, x) -> Atomic.get x
      in match nhead with
       | Nil -> None
       | Next (v, _) when Atomic.compare_and_set q.head s nhead -> Some v
       | _ -> ( B.once b ; loop () )
    in loop ()

  let push q v =
    let rec find_tail_and_enq curr_end node =
      if Atomic.compare_and_set curr_end Nil node then ()
      else match Atomic.get curr_end with
           | Nil -> find_tail_and_enq curr_end node
           | Next (_, n) -> find_tail_and_enq n node
    in
    let newnode = Next (v, Atomic.make Nil) in
    let tail = Atomic.get q.tail in
    match tail with
    | Nil         -> failwith "HW_MSQueue.push: impossible"
    | Next (_, n) -> begin
        find_tail_and_enq n newnode;
        ignore (Atomic.compare_and_set q.tail tail newnode)
    end

  let rec clean_until q f =
    let b = B.create () in
    let rec loop () =
      let s = Atomic.get q.head in
      let nhead = match s with
        | Nil -> failwith "MSQueue.pop: impossible"
        | Next (_, x) -> Atomic.get x
      in match nhead with
       | Nil -> ()
       | Next (v, _) ->
           if not (f v) then
              if Atomic.compare_and_set q.head s nhead
              then (B.reset b; loop ())
              else (B.once b; loop ())
           else ()
    in loop ()

  type 'a cursor = 'a node

  let snapshot q =
    match Atomic.get q.head with
    | Nil -> failwith "MSQueue.snapshot: impossible"
    | Next (_, n) -> Atomic.get n

  let next c =
    match c with
    | Nil -> None
    | Next (a, n) -> Some (a, Atomic.get n)

end

module Scheduler =
struct
  open Effect
  open Effect.Deep

  type 'a cont = ('a, unit) continuation

  type _ t += Suspend : ('a cont -> 'a option) -> 'a t
            | Resume : ('a cont * 'a) -> unit t
            | GetTid : int t
            | Spawn : (unit -> unit) -> unit t
            | Yield : unit t

  let suspend f = perform (Suspend f)
  let resume t v = perform (Resume (t, v))
  let get_tid () = perform GetTid
  let spawn f = perform (Spawn f)
  let yield () = perform Yield

  let pqueue = Q.create ()

  let get_free_pid () = Oo.id (object end)

  let enqueue k = Q.push pqueue k; Gc.minor ()

  let rec dequeue () =
    match Q.pop pqueue with
      | Some k ->
          continue k ()
      | None ->
          ignore (Unix.select [] [] [] 0.01);
          dequeue ()

  let rec exec f =
    let pid = get_free_pid () in
      match_with f ()
      { retc = (fun () -> dequeue ());
        exnc = (fun e -> raise e);
        effc = fun (type a) (e : a t) ->
          match e with
          | Suspend f -> Some (fun (k : (a, _) continuation) ->
              match f k with
                | None -> dequeue ()
                | Some v -> continue k v)
        | Resume (t, v) -> Some (fun (k : (a, _) continuation) ->
            enqueue k;
            continue t v)
        | GetTid -> Some (fun (k : (a, _) continuation) ->
            continue k pid)
        | Spawn f -> Some (fun (k : (a, _) continuation) ->
            enqueue k;
            exec f)
        | Yield -> Some (fun (k : (a, _) continuation) ->
            enqueue k;
            dequeue ())
        | _ -> None }

  let num_threads = 2

  let start f =
    for i = 1 to num_threads - 1 do
      ignore (Domain.spawn dequeue)
    done;
    exec f


end

let _ =
  let procs = 4 in
  let counter = Atomic.make 0 in
  let rec finish () =
    let v = Atomic.get counter in
    if not (Atomic.compare_and_set counter v (v+1)) then finish ();
    if v + 1 = procs then exit 0
  in
  let rec worker n =
    let r = ref 0 in
    for i = 1 to 10000 do
      Scheduler.yield ();
      for j = 1 to 10000 do
        incr r
      done
    done;
    print_string (Printf.sprintf "done %d\n" !r); flush stdout;
    finish ()
  in
    Scheduler.start
      (fun () ->
         for i = 1 to procs do
           (*Scheduler.yield ();*)
           Scheduler.spawn (fun () -> worker i)
         done;
      )
