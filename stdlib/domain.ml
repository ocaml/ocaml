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
    = "caml_ml_domain_id"
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

  let get_initial_keys () : (int * Obj.t) list =
    List.map
      (fun (KI ((idx, _) as k, split)) ->
           (idx, Obj.repr (split (get k))))
      (Atomic.get parent_keys)

  let set_initial_keys (l: (int * Obj.t) list) =
    List.iter
      (fun (idx, v) ->
        let st = maybe_grow idx in st.(idx) <- v)
      l

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
    (* The domain termination callbacks ([at_exit]) are run in
       last-in-first-out (LIFO) order in order to be symmetric with the domain
       creation callbacks ([at_each_spawn]) which run in first-in-fisrt-out
       (FIFO) order. *)
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
