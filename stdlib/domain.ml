module Raw = struct
  (* Low-level primitives provided by the runtime *)
  type t = private int
  external spawn : (unit -> unit) -> Mutex.t -> t
    = "caml_domain_spawn"
  external self : unit -> t
    = "caml_ml_domain_id"
  external cpu_relax : unit -> unit
    = "caml_ml_domain_cpu_relax"
end

module DLS = struct

  type dls_state = Obj.t array

  let unique_value = Obj.repr (ref 0)

  external get_dls_state : unit -> dls_state = "%dls_get"

  external set_dls_state : dls_state -> unit =
    "caml_domain_dls_set" [@@noalloc]

  let create_dls () =
    let st = Array.make 8 unique_value in
    set_dls_state st

  let _ = create_dls ()

  type 'a key = int * (unit -> 'a)

  let key_counter = Atomic.make 0

  let new_key f =
    let k = Atomic.fetch_and_add key_counter 1 in
    (k, f)

  (* If necessary, grow the current domain's local state array such that [idx]
   * is a valid index in the array. *)
  let maybe_grow idx =
    let st = get_dls_state () in
    let sz = Array.length st in
    if idx < sz then st
    else begin
      let rec compute_new_size s =
        if idx < s then s else compute_new_size (2 * s)
      in
      let new_sz = compute_new_size sz in
      let new_st = Array.make new_sz unique_value in
      Array.blit st 0 new_st 0 sz;
      set_dls_state new_st;
      new_st
    end

  let set (idx, _init) x =
    let st = maybe_grow idx in
    st.(idx) <- Obj.repr x

  let get (idx, init) =
    let st = maybe_grow idx in
    let v = st.(idx) in
    if v == unique_value then
      let v' = Obj.repr (init ()) in
      st.(idx) <- v';
      Obj.magic v'
    else Obj.magic v

end
type nanoseconds = int64
external timer_ticks : unit -> (int64 [@unboxed]) =
  "caml_ml_domain_ticks" "caml_ml_domain_ticks_unboxed" [@@noalloc]

module Sync = struct
  let cpu_relax () = Raw.cpu_relax ()
  external poll : unit -> unit = "%poll"
end

type id = Raw.t

type 'a state =
| Running
| Joining of ('a, exn) result option ref
| Finished of ('a, exn) result
| Joined

type 'a t = {
  domain : Raw.t;
  termination_mutex: Mutex.t;
  state: 'a state Atomic.t }

exception Retry
let rec spin f =
  try f () with Retry ->
      Sync.cpu_relax ();
      spin f

let cas r vold vnew =
  if not (Atomic.compare_and_set r vold vnew) then raise Retry

let spawn f =
  let termination_mutex = Mutex.create () in
  let state = Atomic.make Running in
  let body () =
    DLS.create_dls ();
    let result = match f () with
      | x -> Ok x
      | exception ex -> Error ex in
    spin (fun () ->
      match Atomic.get state with
      | Running ->
         cas state Running (Finished result)
      | Joining x as old ->
         cas state old Joined;
         x := Some result
      | Joined | Finished _ ->
         failwith "internal error: I'm already finished?")
  in
  { domain = Raw.spawn body termination_mutex; termination_mutex; state }

let termination_wait termination_mutex =
  (* Raw.spawn returns with the mutex locked, so this will block if the
     domain has not terminated yet *)
  Mutex.lock termination_mutex;
  Mutex.unlock termination_mutex

let join { termination_mutex; state; _ } =
  let res = spin (fun () ->
    match Atomic.get state with
    | Running -> begin
      let x = ref None in
      cas state Running (Joining x);
      termination_wait termination_mutex;
      match !x with
      | None -> failwith "internal error: termination signaled but result not passed"
      | Some r -> r
    end
    | Finished x as old ->
      cas state old Joined;
      termination_wait termination_mutex;
      x
    | Joining _ | Joined ->
      raise (Invalid_argument "This domain has already been joined")
    )
  in
  match res with
  | Ok x -> x
  | Error ex -> raise ex

let get_id { domain; _ } = domain

let self () = Raw.self ()


