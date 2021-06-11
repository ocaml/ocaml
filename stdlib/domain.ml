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
    CamlinternalDomain.initialise_dls ();
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

module DLS = struct

  type 'a key = int ref * (unit -> 'a)

  type entry = {key_id: int ref; mutable slot: Obj.t}

  external get_dls_list : unit -> entry list = "%dls_get"

  external set_dls_list : entry list  -> unit
    = "caml_domain_dls_set" [@@noalloc]

  let new_key f = (ref 0, f)

  let set k x =
    let cs = Obj.repr x in
    let vals = get_dls_list () in
    let rec add_or_update_entry k v l =
      match l with
      | [] -> Some {key_id = k; slot = v}
      | hd::tl ->
        if (hd.key_id == k) then begin
          hd.slot <- v;
          None
        end
        else add_or_update_entry k v tl
    in
    let (key, _) = k in
    match add_or_update_entry key cs vals with
    | None -> ()
    | Some e -> set_dls_list (e::vals)

  let get k =
    let rec search key_id init dls_list l =
      match l with
      | [] ->
        begin
          let slot = Obj.repr (init ()) in
          set_dls_list ({key_id; slot}::dls_list);
          slot
        end
      | hd::tl ->
        if hd.key_id == key_id then hd.slot else search key_id init dls_list tl
    in
    let dls_list = get_dls_list () in
    let (key_id, init) = k in
    Obj.obj @@ search key_id init dls_list dls_list

end
