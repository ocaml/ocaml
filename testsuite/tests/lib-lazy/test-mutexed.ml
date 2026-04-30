(* TEST
 shared-libraries;
 hassysthreads;
 flags = "-I ${ocamlsrcdir}/otherlibs/unix -I ${ocamlsrcdir}/otherlibs/systhreads";
 ld_library_path += " ${ocamlsrcdir}/otherlibs/unix";
 ld_library_path += " ${ocamlsrcdir}/otherlibs/systhreads";
 expect;
*)

#load "unix.cma";;
#load "threads.cma";;
[%%expect{|
|}]

module LazyM = Lazy.Mutexed
[%%expect{|
module LazyM = Lazy.Mutexed
|}]

(* direct value return *)
let it =
  let v = LazyM.from_val 42 in
  assert (LazyM.is_val v);
  (LazyM.force v, LazyM.force v)
[%%expect{|
val it : int * int = (42, 42)
|}]

(* value return *)
let it =
  let v = LazyM.from_fun (fun () -> 43) in
  assert (not (LazyM.is_val v));
  (LazyM.force v, LazyM.force v)
[%%expect{|
val it : int * int = (43, 43)
|}]


(* exception case *)
let it =
  let fail = LazyM.from_fun (fun () -> raise Exit) in
  let check () = match LazyM.force fail with
  | exception Exit ->
      assert (not (LazyM.is_val fail));
      true
  | exception _ | _ -> false
  in
  check () && check ()
[%%expect{|
val it : bool = true
|}]

(* sharing test *)
let it =
  let r = ref 0 in
  let test = LazyM.from_fun (fun () -> incr r) in
  assert (not (LazyM.is_val test));
  (* side-effects must not be repeated in sequential settings. *)
  LazyM.force test;
  assert (LazyM.is_val test);
  LazyM.force test;
  if !r = 1 then Ok () else Error !r
[%%expect{|
val it : (unit, int) result = Ok ()
|}]

(* Recursive forcing test *)
let it =
  let thunk = ref (LazyM.from_val 0) in
  thunk := LazyM.from_fun (fun () -> LazyM.force !thunk + 1);
  (* We expect to get a Sys_error exception due to recursive forcing.
     Hide its payload which contains a non-portable error message
     (#14590). *)
  match LazyM.force !thunk with
  | exception Sys_error _ -> Ok "failed as expected"
  | _ -> Error ()
[%%expect{|
val it : (string, unit) result = Ok "failed as expected"
|}]

(* Concurrency test *)
let it =
  (* [nb_threads] will force a single thunk concurrently -- to create
     concurrency we ensure that the initialization function yields
     [nb_yields] times. *)
  let nb_threads = 3 in
  let nb_yields = 30 in

  (* helper functions *)
  let spawn (f : unit -> 'a) =
    let result = ref (None : ('a, exn) result option) in
    let thread = Thread.create (fun () ->
      match f () with
      | v -> result := Some (Ok v)
      | exception exn -> result := Some (Error exn)
    ) () in
    (result, thread)
  in
  let join (result, thread) =
    Thread.join thread;
    Option.get !result
  in

  (* The initialization function of the thunk increments a global
     atomic counter and returns the pre-increment value. We expect
     that it runs only once, and thus returns [0]. *)
  let count = Atomic.make 0 in
  let thunk = LazyM.from_fun (fun () ->
    let id = Atomic.fetch_and_add count 1 in
    (* Now yield a lot so that concurrent forcers have a chance to
       observe the forcing state. *)
    for _ = 1 to nb_yields do Thread.yield () done;
    assert (Atomic.get count = id + 1);
    id
  ) in

  (* We use [go] as a barrier to make all threads start computing at
     roughly the same time, after they have all spawned. *)
  let go = Mutex.create () in
  Mutex.lock go; (* don't go yet *)
  let worker () =
    Mutex.lock go; Mutex.unlock go;
    LazyM.force thunk
  in
  let threads = List.init nb_threads (fun _ -> spawn worker) in
  Mutex.unlock go; (* now you can go *)
  let results =
    List.map join threads
    |> List.sort Stdlib.compare
  in

  (* We expect a list of [Ok 0] as results: initialization function
     has run once, returns the initial counter state [0], and all
     other count block and observe this value once available. *)
  let expected = List.init nb_threads (fun _ -> Ok 0) in
  let status =
    if results = expected then "pass" else "fail"
  in
  status, results

[%%expect{|
val it : string * (int, exn) result list = ("pass", [Ok 0; Ok 0; Ok 0])
|}]


(* Check that the documentation examples are well-typed. *)
module Example1 (Config : sig
  type t
  val default : unit -> t
  val read_from_path : string -> t
end) = struct
  let config = Lazy.Mutexed.from_fun (fun () ->
    match Sys.getenv_opt "MYLIB_CONFIG_PATH" with
    | None | Some "" -> Config.default ()
    | Some path -> Config.read_from_path path
  )
end
[%%expect{|
module Example1 :
  (Config : sig
              type t
              val default : unit -> t
              val read_from_path : string -> t
            end)
    -> sig val config : Config.t Lazy.Mutexed.t end
|}]

module Example2 = struct
  let entropy =
    (* we use a mibibyte of random data from /dev/urandom *)
    Lazy.Mutexed.from_fun (fun () ->
      In_channel.with_open_bin "/dev/urandom" (fun chan ->
        In_channel.really_input_string chan (1024 * 1024)
      )
    )
end
[%%expect {|
module Example2 : sig val entropy : string option Lazy.Mutexed.t end
|}]
