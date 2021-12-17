(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                   Tom Kelly, OCaml Labs Consultancy                    *)
(*                                                                        *)
(*   Copyright 2019 Indian Institute of Technology, Madras                *)
(*   Copyright 2014 University of Cambridge                               *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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

let cpu_relax () = Raw.cpu_relax ()

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
    (* [Sys.opaque_identity] ensures that flambda does not look at the type of
     * [x], which may be a [float] and conclude that the [st] is a float array.
     * We do not want OCaml's float array optimisation kicking in here. *)
    st.(idx) <- Obj.repr (Sys.opaque_identity x)

  let get (idx, init) =
    let st = maybe_grow idx in
    let v = st.(idx) in
    if v == unique_value then
      let v' = Obj.repr (init ()) in
      st.(idx) <- (Sys.opaque_identity v');
      Obj.magic v'
    else Obj.magic v

end

(* first spawn, domain startup and at exit functionality *)
let first_domain_spawned = Atomic.make false

let first_spawn_function = ref (fun () -> ())

let at_first_spawn f =
  if Atomic.get first_domain_spawned then
    raise (Invalid_argument "First domain already spawned")
  else begin
    let old_f = !first_spawn_function in
    let new_f () = f (); old_f () in
    first_spawn_function := new_f
  end

let do_at_first_spawn () =
  if not (Atomic.get first_domain_spawned) then begin
    Atomic.set first_domain_spawned true;
    !first_spawn_function();
    (* Release the old function *)
    first_spawn_function := (fun () -> ())
  end

let exit_function = Atomic.make (fun () -> ())

let rec at_exit f =
  let wrapped_f () = try f () with _ -> () in
  let old_exit = Atomic.get exit_function in
  let new_exit () = wrapped_f (); old_exit () in
  let success = Atomic.compare_and_set exit_function old_exit new_exit in
  if success then
    Stdlib.at_exit wrapped_f
  else at_exit f

let do_at_exit () = (Atomic.get exit_function) ()

let startup_function = Atomic.make (fun () -> ())

let rec at_startup f =
  let old_startup = Atomic.get startup_function in
  let new_startup () = f (); old_startup () in
  let success =
    Atomic.compare_and_set startup_function old_startup new_startup
  in
  if success then
    ()
  else
    at_startup f

(* Spawn and join functionality *)
exception Retry
let rec spin f =
  try f () with Retry ->
      cpu_relax ();
      spin f

let cas r vold vnew =
  if not (Atomic.compare_and_set r vold vnew) then raise Retry

let spawn f =
  do_at_first_spawn ();
  (* the termination_mutex is used to block a joining thread *)
  let termination_mutex = Mutex.create () in
  let state = Atomic.make Running in
  let at_startup = Atomic.get startup_function in
  let body () =
    let result = match DLS.create_dls (); at_startup (); f () with
      | x -> Ok x
      | exception ex -> Error ex
    in
    do_at_exit ();
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
      | None ->
          failwith "internal error: termination signaled but result not passed"
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

external set_name : string -> unit = "caml_ml_domain_set_name"

let is_main_domain () = (self () :> int) == 0
