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

  (* The layouts of [state] and [term_sync] are hard-coded in
     [runtime/domain.c] *)

  type 'a state =
    | Running
    | Finished of ('a, exn) result [@warning "-unused-constructor"]

  type 'a term_sync = {
    (* protected by [mut] *)
    mutable state : 'a state [@warning "-unused-field"] ;
    mut : Mutex.t ;
    cond : Condition.t ;
  }

  external spawn : (unit -> 'a) -> 'a term_sync -> t
    = "caml_domain_spawn"
  external self : unit -> t
    = "caml_ml_domain_id" [@@noalloc]
  external cpu_relax : unit -> unit
    = "caml_ml_domain_cpu_relax"
  external get_recommended_domain_count: unit -> int
    = "caml_recommended_domain_count" [@@noalloc]
end

let cpu_relax () = Raw.cpu_relax ()

type id = Raw.t

type 'a t = {
  domain : Raw.t;
  term_sync : 'a Raw.term_sync;
}

module DLS = struct

  module Obj_opt : sig
    type t
    val none : t
    val some : 'a -> t
    val is_some : t -> bool

    (** [unsafe_get obj] may only be called safely
        if [is_some] is true.

        [unsafe_get (some v)] is equivalent to
        [Obj.obj (Obj.repr v)]. *)
    val unsafe_get : t -> 'a
  end = struct
    type t = Obj.t
    let none = Obj.repr (ref 0)
    let some v = Obj.repr v
    let is_some obj = (obj != none)
    let unsafe_get obj = Obj.obj obj
  end

  type dls_state = Obj_opt.t array

  external get_dls_state : unit -> dls_state = "%dls_get"

  external set_dls_state : dls_state -> unit =
    "caml_domain_dls_set" [@@noalloc]

  external compare_and_set_dls_state : dls_state -> dls_state -> bool =
    "caml_domain_dls_compare_and_set" [@@noalloc]

  let create_dls () =
    let st = Array.make 8 Obj_opt.none in
    set_dls_state st

  let _ = create_dls ()

  type 'a key = int * (unit -> 'a)

  let key_counter = Atomic.make 0

  type key_initializer =
    KI: 'a key * ('a -> 'a) -> key_initializer

  let parent_keys = Atomic.make ([] : key_initializer list)

  let rec add_parent_key ki =
    let l = Atomic.get parent_keys in
    if not (Atomic.compare_and_set parent_keys l (ki :: l))
    then add_parent_key ki

  let new_key ?split_from_parent init_orphan =
    let idx = Atomic.fetch_and_add key_counter 1 in
    let k = (idx, init_orphan) in
    begin match split_from_parent with
    | None -> ()
    | Some split -> add_parent_key (KI(k, split))
    end;
    k

  (* If necessary, grow the current domain's local state array such that [idx]
   * is a valid index in the array. *)
  let rec maybe_grow idx =
    let st = get_dls_state () in
    let sz = Array.length st in
    if idx < sz then st
    else begin
      let rec compute_new_size s =
        if idx < s then s else compute_new_size (2 * s)
      in
      let new_sz = compute_new_size sz in
      let new_st = Array.make new_sz Obj_opt.none in
      Array.blit st 0 new_st 0 sz;
      (* We want a implementation that is safe with respect to
         single-domain multi-threading: retry if the DLS state has
         changed under our feet.
         Note that the number of retries will be very small in
         contended scenarios, as the array only grows, with
         exponential resizing. *)
      if compare_and_set_dls_state st new_st
      then new_st
      else maybe_grow idx
    end

  let set (type a) (idx, _init) (x : a) =
    let st = maybe_grow idx in
    (* [Sys.opaque_identity] ensures that flambda does not look at the type of
     * [x], which may be a [float] and conclude that the [st] is a float array.
     * We do not want OCaml's float array optimisation kicking in here. *)
    st.(idx) <- Obj_opt.some (Sys.opaque_identity x)


  let[@inline never] array_compare_and_set a i oldval newval =
    (* Note: we cannot use [@poll error] due to the
       allocations on a.(i) in the Double_array case. *)
    let curval = a.(i) in
    if curval == oldval then (
      Array.unsafe_set a i newval;
      true
    ) else false

  let get (type a) ((idx, init) : a key) : a =
    let st = maybe_grow idx in
    let obj = st.(idx) in
    if Obj_opt.is_some obj
    then (Obj_opt.unsafe_get obj : a)
    else begin
      let v : a = init () in
      let new_obj = Obj_opt.some (Sys.opaque_identity v) in
      (* At this point, [st] or [st.(idx)] may have been changed
         by another thread on the same domain.

         If [st] changed, it was resized into a larger value,
         we can just reuse the new value.

         If [st.(idx)] changed, we drop the current value to avoid
         letting other threads observe a 'revert' that forgets
         previous modifications. *)
      let st = get_dls_state () in
      if array_compare_and_set st idx obj new_obj
      then v
      else begin
        (* if st.(idx) changed, someone must have initialized
           the key in the meantime. *)
        let updated_obj = st.(idx) in
        if Obj_opt.is_some updated_obj
        then (Obj_opt.unsafe_get updated_obj : a)
        else assert false
      end
    end

  type key_value = KV : 'a key * 'a -> key_value

  let get_initial_keys () : key_value list =
    List.map
      (fun (KI (k, split)) -> KV (k, (split (get k))))
      (Atomic.get parent_keys)

  let set_initial_keys (l: key_value list) =
    List.iter (fun (KV (k, v)) -> set k v) l
end

(******** Identity **********)

let get_id { domain; _ } = domain

let self () = Raw.self ()

let is_main_domain () = (self () :> int) = 0

(******** Callbacks **********)

(* first spawn, domain startup and at exit functionality *)
let first_domain_spawned = Atomic.make false

let first_spawn_function = ref (fun () -> ())

let before_first_spawn f =
  if Atomic.get first_domain_spawned then
    raise (Invalid_argument "first domain already spawned")
  else begin
    let old_f = !first_spawn_function in
    let new_f () = old_f (); f () in
    first_spawn_function := new_f
  end

let do_before_first_spawn () =
  if not (Atomic.get first_domain_spawned) then begin
    Atomic.set first_domain_spawned true;
    !first_spawn_function();
    (* Release the old function *)
    first_spawn_function := (fun () -> ())
  end

let at_exit_key = DLS.new_key (fun () -> (fun () -> ()))

let at_exit f =
  let old_exit : unit -> unit = DLS.get at_exit_key in
  let new_exit () =
    f (); old_exit ()
  in
  DLS.set at_exit_key new_exit

let do_at_exit () =
  let f : unit -> unit = DLS.get at_exit_key in
  f ()

let _ = Stdlib.do_domain_local_at_exit := do_at_exit

(******* Creation and Termination ********)

let spawn f =
  do_before_first_spawn ();
  let pk = DLS.get_initial_keys () in

  (* [term_sync] is used to synchronize with the joining domains *)
  let term_sync =
    Raw.{ state = Running ;
          mut = Mutex.create () ;
          cond = Condition.create () }
  in

  let body () =
    match
      DLS.create_dls ();
      DLS.set_initial_keys pk;
      let res = f () in
      res
    with
    (* Run the [at_exit] callbacks when the domain computation either
       terminates normally or exceptionally. *)
    | res ->
        (* If the domain computation terminated normally, but the
           [at_exit] callbacks raised an exception, then return the
           exception. *)
        do_at_exit ();
        res
    | exception exn ->
        (* If both the domain computation and the [at_exit] callbacks
           raise exceptions, then ignore the exception from the
           [at_exit] callbacks and return the original exception. *)
        (try do_at_exit () with _ -> ());
        raise exn
  in
  let domain = Raw.spawn body term_sync in
  { domain ; term_sync }

let join { term_sync ; _ } =
  let open Raw in
  let rec loop () =
    match term_sync.state with
    | Running ->
        Condition.wait term_sync.cond term_sync.mut;
        loop ()
    | Finished res ->
        res
  in
  match Mutex.protect term_sync.mut loop with
  | Ok x -> x
  | Error ex -> raise ex

let recommended_domain_count = Raw.get_recommended_domain_count
