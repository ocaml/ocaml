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

type state = Obj_opt.t array

module type State_operations = sig
  val get : unit -> state
  val compare_and_set : state -> state -> bool
end

module type Key = sig
    type 'a t

    val make : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a t

    val get : 'a t -> 'a

    val set : 'a t -> 'a -> unit

    type key_value = KV : 'a t * 'a -> key_value
    val get_initial_keys : unit -> key_value list
    val set_initial_keys : key_value list -> unit

    val at_exit : (unit -> unit) -> unit
    val do_at_exit : unit -> unit
end

module Make (State : State_operations)
  : Key with type 'a t = int * (unit -> 'a)
= struct
  type 'a t = int * (unit -> 'a)

  (* Manipulating existing keys. *)

  (* If necessary, grow the current domain's local state array such that [idx]
   * is a valid index in the array. *)
  let rec maybe_grow idx =
    let st = State.get () in
    let sz = Array.length st in
    if idx < sz then st
    else begin
      let rec compute_new_size s =
        if idx < s then s else compute_new_size (2 * s)
      in
      let new_sz = compute_new_size (max sz 8) in
      let new_st = Array.make new_sz Obj_opt.none in
      Array.blit st 0 new_st 0 sz;
      (* We want a implementation that is safe with respect to
         single-domain multi-threading: retry if the DLS state has
         changed under our feet.
         Note that the number of retries will be very small in
         contended scenarios, as the array only grows, with
         exponential resizing. *)
      if State.compare_and_set st new_st
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

  let get (type a) ((idx, init) : a t) : a =
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
      let st = State.get () in
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

  (* Creating new keys. *)

  let key_counter = Atomic.make 0

  let make init =
    let idx = Atomic.fetch_and_add key_counter 1 in
    (idx, init)

  (* Keys initialized by the parent. *)

  type key_initializer =
    KI: 'a t * ('a -> 'a) -> key_initializer

  let parent_keys = Atomic.make ([] : key_initializer list)

  let rec add_parent_key ki =
    let l = Atomic.get parent_keys in
    if not (Atomic.compare_and_set parent_keys l (ki :: l))
    then add_parent_key ki

  let make ?split_from_parent init_orphan =
    let k = make init_orphan in
    begin match split_from_parent with
    | None -> ()
    | Some split -> add_parent_key (KI(k, split))
    end;
    k

  type key_value = KV : 'a t * 'a -> key_value

  let get_initial_keys () : key_value list =
    List.map
      (fun (KI (k, split)) -> KV (k, (split (get k))))
      (Atomic.get parent_keys)

  let set_initial_keys (l: key_value list) =
    List.iter (fun (KV (k, v)) -> set k v) l

  (* at_exit *)

  let nothing () = ()
  let at_exit_key : (unit -> unit) t =
    make (fun () -> nothing)

  let at_exit f =
    let old_exit : unit -> unit = get at_exit_key in
    let new_exit () =
      f (); old_exit ()
    in
    set at_exit_key new_exit

  let do_at_exit () =
    let f : unit -> unit = get at_exit_key in
    set at_exit_key nothing;
    f ()
end

module DLS_operations = struct
  external get : unit -> state = "%dls_get"

  external compare_and_set : state -> state -> bool =
    "caml_domain_dls_compare_and_set" [@@noalloc]
end

module TLS_operations = struct
  external get : unit -> state =
    "caml_domain_tls_get" [@@noalloc]

  external compare_and_set : state -> state -> bool =
    "caml_domain_tls_compare_and_set" [@@noalloc]
end

(* If we just define
     module DLS = Make(DLS_operations)
   then non-flambda compilers struggle to optimize the functor application,
   and they compile DLS.get into an indirect call to a closure.

   The trick we use below is to duplicate the happy/fast path of DLS.get,
   so that the compiler can easily see that it is a closed function
   and optimize direct applications.
*)
module DLS0 = Make(DLS_operations)
module DLS = struct
  include DLS0
  let slow_get = DLS0.get

  let get (type a) (key : a t) : a =
    let (idx, _) = key in
    let st = DLS_operations.get () in
    if Array.length st <= idx then slow_get key
    else begin
      let obj = st.(idx) in
      if Obj_opt.is_some obj
      then (Obj_opt.unsafe_get obj : a)
      else slow_get key
    end
end

module TLS0 = Make(TLS_operations)
module TLS = struct
  include TLS0
  let slow_get = TLS0.get

  let get (type a) (key : a t) : a =
    let (idx, _) = key in
    let st = TLS_operations.get () in
    if Array.length st <= idx then slow_get key
    else begin
      let obj = st.(idx) in
      if Obj_opt.is_some obj
      then (Obj_opt.unsafe_get obj : a)
      else slow_get key
    end
end

let _ = Stdlib.do_domain_local_at_exit := DLS.do_at_exit
let _ = Stdlib.do_thread_local_at_exit := TLS.do_at_exit
