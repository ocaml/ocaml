(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*  David Nowak and Xavier Leroy, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Events *)
type 'a basic_event =
  { poll: unit -> bool;
      (* If communication can take place immediately, return true. *)
    suspend: unit -> unit;
      (* Offer the communication on the channel and get ready
         to suspend current process. *)
    result: unit -> 'a }
      (* Return the result of the communication *)

type 'a event =
    Communication of (int ref -> Condition.t -> int -> 'a basic_event)
  | Choose of 'a event list
  | Guard of (unit -> 'a event)

(* Communication channels *)
type 'a channel =
  { writes_pending: 'a communication Queue.t;  (* All offers to write on it *)
    reads_pending:  'a communication Queue.t } (* All offers to read from it *)

(* Communication offered *)
and 'a communication =
  { performed: int ref;  (* -1 if not performed yet, set to the number *)
                         (* of the matching communication after rendez-vous. *)
    condition: Condition.t;             (* To restart the blocked thread. *)
    mutable data: 'a option;            (* The data sent or received. *)
    event_number: int }                 (* Event number in select *)

(* Create a channel *)

let new_channel () =
  { writes_pending = Queue.new();
    reads_pending = Queue.new() }

(* Basic synchronization function *)

let masterlock = Mutex.new()

let basic_sync genev =
  let performed = ref (-1) in
  let condition = Condition.new() in
  let bev = Array.new(Array.length genev) (genev.(0) performed condition 0) in
  for i = 1 to Array.length genev - 1 do
    bev.(i) <- genev.(i) performed condition i
  done;
  (* See if any of the events is already activable *)
  let rec poll_events i =
    if i >= Array.length bev
    then false
    else bev.(i).poll() or poll_events (i+1) in
  Mutex.lock masterlock;
  if not (poll_events 0) then begin
    (* Suspend on all events *)
    for i = 0 to Array.length bev - 1 do bev.(i).suspend() done;
    (* Wait until the condition is signalled *)
    Condition.wait condition masterlock
  end;
  Mutex.unlock masterlock;
  (* Extract the result *)
  bev.(!performed).result()

(* Apply a random permutation on an array *)

let scramble_array a =
  let len = Array.length a in
  for i = len - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = a.(i) in a.(i) <- a.(j); a.(j) <- temp
  done;
  a

(* Main synchronization function *)

let rec flatten_event ev accu =
  match ev with
    Communication bev -> bev :: accu
  | Choose evl -> List.fold_right flatten_event evl accu
  | Guard fn -> flatten_event (fn ()) accu

let sync ev =
  basic_sync(scramble_array(Array.of_list(flatten_event ev [])))

(* Event polling -- like sync, but non-blocking *)

let basic_poll genev =
  let performed = ref (-1) in
  let condition = Condition.new() in
  let bev = Array.new(Array.length genev) (genev.(0) performed condition 0) in
  for i = 1 to Array.length genev - 1 do
    bev.(i) <- genev.(i) performed condition i
  done;
  (* See if any of the events is already activable *)
  let rec poll_events i =
    if i >= Array.length bev
    then false
    else bev.(i).poll() or poll_events (i+1) in
  Mutex.lock masterlock;
  let ready = poll_events 0 in
  if ready then begin
    (* Extract the result *)
    Mutex.unlock masterlock;
    Some(bev.(!performed).result())
  end else begin
    (* Cancel the communication offers *)
    performed := 0;
    Mutex.unlock masterlock;
    None
  end

let poll ev =
  basic_poll(scramble_array(Array.of_list(flatten_event ev [])))

(* Event construction *)

let send channel data =
  Communication(fun performed condition evnum ->
    let wcomm =
      { performed = performed;
        condition = condition;
        data = Some data;
        event_number = evnum } in
    { poll = (fun () ->
        let rec poll () =
          let rcomm = Queue.take channel.reads_pending in
          if !(rcomm.performed) >= 0 then
            poll ()
          else begin
            rcomm.data <- wcomm.data;
            performed := evnum;
            rcomm.performed := rcomm.event_number;
            Condition.signal rcomm.condition
          end in
        try
          poll();
          true
        with Queue.Empty ->
          false);
      suspend = (fun () -> Queue.add wcomm channel.writes_pending);
      result = (fun () -> ()) })

let receive channel =
  Communication(fun performed condition evnum ->
    let rcomm =
      { performed = performed;
        condition = condition;
        data = None;
        event_number = evnum } in
    { poll = (fun () ->
        let rec poll () =
          let wcomm = Queue.take channel.writes_pending in
          if !(wcomm.performed) >= 0 then
            poll ()
          else begin
            rcomm.data <- wcomm.data;
            performed := evnum;
            wcomm.performed := wcomm.event_number;
            Condition.signal wcomm.condition
          end in
        try
          poll();
          true
        with Queue.Empty ->
          false);
    suspend = (fun () ->
      Queue.add rcomm channel.reads_pending);
    result = (fun () ->
      match rcomm.data with
        None -> invalid_arg "Event.receive"
      | Some res -> res) })

let choose = function
    [] -> invalid_arg "Event.choose"
  | evl -> Choose evl

let guard fn = Guard fn

let rec wrap ev fn =
  match ev with
    Communication genev ->
      Communication(fun performed condition evnum ->
        let bev = genev performed condition evnum in
        { poll = bev.poll;
          suspend = bev.suspend;
          result = (fun () -> fn(bev.result())) })
  | Choose evl ->
      Choose(List.map (fun ev -> wrap ev fn) evl)
  | Guard gu ->
      Guard(fun () -> wrap (gu()) fn)

(* Convenience functions *)

let select evl = sync(Choose evl)
