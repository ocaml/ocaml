(*./ocamlopt.opt -nostdlib -I stdlib benchmarks_atomic.ml -o benchmarks_atomic.exe *)

module Backoff : sig
  (** Exponential backoff mechanism. *)

  type t [@@immediate]
  (** Type of backoff values. *)

  val max_wait_log : int
  (** Logarithm of the maximum allowed value for wait. *)

  val create : ?lower_wait_log:int -> ?upper_wait_log:int -> unit -> t
  (** [create] creates a backoff value. [upper_wait_log], [lower_wait_log]
      override the logarithmic upper and lower bound on the number of spins
      executed by {!once}. *)

  val default : t
  (** [default] is equivalent to [create ()]. *)

  val once : t -> t
  (** [once b] executes one wait and returns a new backoff with logarithm
      of the current maximum value incremented unless it is already at
      [upper_wait_log] of [b]. *)

  val reset : t -> t
  (** [reset b] returns a backoff equivalent to [b] except with
      current value set to the [lower_wait_log] of [b]. *)

  val get_wait_log : t -> int
  (** [get_wait_log b] returns logarithm of the maximum value of wait for next
      {!once}. *)
end = struct
  type t = int

  (* externals imported to avoid dependency cycles *)
  external bool_to_int : bool -> int = "%identity"
  external cpu_relax : unit -> unit
    = "caml_ml_domain_cpu_relax"
  external get_recommended_domain_count: unit -> int
    = "caml_recommended_domain_count" [@@noalloc]

  let single_mask = bool_to_int (get_recommended_domain_count () = 1) - 1
  let bits = 5
  let max_wait_log = 30 (* [Random.bits] returns 30 random bits. *)
  let mask = (1 lsl bits) - 1

  let create ?(lower_wait_log = 4) ?(upper_wait_log = 17) () =
    assert (
      0 <= lower_wait_log
      && lower_wait_log <= upper_wait_log
      && upper_wait_log <= max_wait_log);
    (upper_wait_log lsl (bits * 2))
    lor (lower_wait_log lsl bits) lor lower_wait_log

  let get_upper_wait_log backoff = backoff lsr (bits * 2)
  let get_lower_wait_log backoff = (backoff lsr bits) land mask
  let get_wait_log backoff = backoff land mask

  let reset backoff =
    let lower_wait_log = get_lower_wait_log backoff in
    backoff land lnot mask lor lower_wait_log

  (* We don't want [once] to be inlined.  This may avoid code bloat. *)
  let[@inline never] once backoff =
    let wait_log = get_wait_log backoff in
    let wait_mask = (1 lsl wait_log) - 1 in
    (* We use a ref and a countdown while-loop (uses one variable)
       instead of a for-loop (uses two variables) to reduce register
       pressure.  Local ref does not allocate with native compiler. *)
    let t = ref (wait_mask land single_mask) in
    while 0 <= !t do
      cpu_relax ();
      t := !t - 1
    done;
    let upper_wait_log = get_upper_wait_log backoff in
    (* We recompute [wait_log] to reduce register pressure. *)
    let wait_log = get_wait_log backoff in
    (* [bool_to_int] generates branchless code, this reduces branch predictor
       pressure and generates shorter code. *)
    let next_wait_log = wait_log + bool_to_int (wait_log < upper_wait_log) in
    backoff - wait_log + next_wait_log

  let default = create ()
end

let compare_and_exchange_c a ~expected ~set =
  Atomic.compare_and_exchange a ~expected ~set

let compare_and_exchange_ocaml a ~expected ~set =
  let rec aux backoff =
    let current = Atomic.get a in
    if not (current == expected) then current else
    if Atomic.compare_and_set a expected set then expected else
      aux (Backoff.once backoff)
  in aux Backoff.default


let get_param p =
  try Sys.getenv p with _ ->
    Printf.ksprintf failwith
      "The environment variable %S must be defined." p

let get_int_param p =
  let s = get_param p in
  try int_of_string s with _ ->
    Printf.ksprintf failwith
      "The environment variable %S=%S must be an integer."
      p s

let get_dict_param p dict =
  let s = get_param p in
  try List.assoc s dict with _ ->
    Printf.ksprintf failwith
      "The environment variable %S=%S must be among [ %s ]."
      p s (String.concat ", " (List.map fst dict))

type impl =
  | C_primitive
  | Ocaml_function

let n_iters = get_int_param "NITERS"
let n_domains = get_int_param "DOMAINS"

let impl = get_dict_param "IMPL" [
  "c_primitive", C_primitive;
  "ocaml_function", Ocaml_function;
]

let state = Atomic.make 0

let next x = x + 1

let c_primitive () =
  for _ = 1 to n_iters do
    let backoff = ref Backoff.default in
    while
      let cur = Atomic.get state in
      let res = compare_and_exchange_c state ~expected:cur ~set:(next cur) in
      cur <> res
    do backoff := Backoff.once !backoff done
  done

let ocaml_function () =
  for _ = 1 to n_iters do
    let backoff = ref Backoff.default in
    while
      let cur = Atomic.get state in
      let res = compare_and_exchange_ocaml
                  state ~expected:cur ~set:(next cur) in
      cur <> res
    do backoff := Backoff.once !backoff done
  done

let work () =
  match impl with
  | C_primitive -> c_primitive ()
  | Ocaml_function -> ocaml_function ()

let () =
  let go = Atomic.make false in
  let wait () =
    while not (Atomic.get go) do
      Domain.cpu_relax ()
    done
  in
  let domains =
    let start () = wait () ; work () in
    List.init (n_domains - 1) (fun _ -> Domain.spawn start)
  in
  Atomic.set go true;
  work ();
  List.iter Domain.join domains
  