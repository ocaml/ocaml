(* TEST
 expect;
*)

module LazyAR = Lazy.Atomic_repeating
[%%expect{|
module LazyAR = Lazy.Atomic_repeating
|}]

(* direct value return *)
let it =
  let v = LazyAR.from_val 42 in
  (LazyAR.force v, LazyAR.force v)
[%%expect{|
val it : int * int = (42, 42)
|}]

(* value return *)
let it =
  let v = LazyAR.from_fun (fun () -> 43) in
  (LazyAR.force v, LazyAR.force v)
[%%expect{|
val it : int * int = (43, 43)
|}]


(* exception case *)
let it =
  let fail = LazyAR.from_fun (fun () -> raise Exit) in
  let check () = match LazyAR.force fail with
  | exception Exit -> true
  | exception _ | _ -> false
  in
  check () && check ()
[%%expect{|
val it : bool = true
|}]

(* sharing check *)
let it =
  let r = ref 0 in
  let test = LazyAR.from_fun (fun () -> incr r) in
  (* side-effects must not be repeated in sequential settings. *)
  LazyAR.force test;
  LazyAR.force test;
  if !r = 1 then Ok () else Error !r
[%%expect{|
val it : (unit, int) result = Ok ()
|}]

(* Fake concurrency tests : we can use reentrancy to emulate concurrency. *)
let it =
  let step = ref 0 in
  let thunk =
    let self = ref (LazyAR.from_fun (fun () -> 500)) in
    self := begin
      (* The first call to reach !step = 100 will finish with the value 0.
         Other calls will finish with higher values, but those will be discarded. *)
      let discard n =
        if n = 0 then prerr_endline "Discard error!"
      in
      LazyAR.from_fun ~discard (fun () ->
        if !step >= 100 then 0
        else (incr step; LazyAR.force !self + 1)
      )
    end;
    !self
  in
  let result1 = LazyAR.force thunk in
  let result2 = LazyAR.force thunk in
  if result1 = 0 && result2 = 0 && !step = 100
  then Ok ()
  else Error (~result1, ~result2, ~step:!step)
[%%expect{|
val it : (unit, result1:int * result2:int * step:int) result = Ok ()
|}]


(* Check that the documentation examples are well-typed. *)
module Example1 (Config : sig
  type t
  val default : unit -> t
  val read_from_path : string -> t
end) = struct
  let config = Lazy.Atomic_repeating.from_fun (fun () ->
    match Sys.getenv "MYLIB_CONFIG_PATH" with
    | exception _ -> Config.default ()
    | path -> Config.read_from_path path
  )
end
[%%expect{|
module Example1 :
  (Config : sig
              type t
              val default : unit -> t
              val read_from_path : string -> t
            end)
    -> sig val config : Config.t Lazy.Atomic_repeating.t end
|}]

module Example2 () = struct
  let log_file_and_channel =
    let acquire () =
      match Sys.getenv "MYLIB_LOG_PATH" with
      | exception _ ->
          let path, chan = Filename.open_temp_file "mylib" ".log" in
          (`Temp path), chan
      | path ->
          let chan = Out_channel.open_bin path in
          (`User path), chan
    in
    let discard (source, chan) =
      Out_channel.close chan;
      match source with
      | `User _ -> ()
      | `Temp path -> Sys.remove path
    in
    Lazy.Atomic_repeating.from_fun ~discard acquire
end
[%%expect{|
module Example2 :
  () ->
    sig
      val log_file_and_channel :
        ([ `Temp of string | `User of string ] * Out_channel.t)
        Lazy.Atomic_repeating.t
    end
|}]

module Example3 = struct
  let entropy =
    (* we use a mibibyte of random data from /dev/urandom *)
    let init_mutex = Mutex.create () in
    let result = ref None in
    Lazy.Atomic_repeating.from_fun (fun () ->
      Mutex.protect init_mutex (fun () ->
        match !result with
        | Some v -> v
        | None ->
            let v =
              In_channel.with_open_bin "/dev/urandom" (fun chan ->
                In_channel.really_input_string chan (1024 * 1024)
              )
            in
            result := Some v;
            v
      )
    )
end
[%%expect {|
module Example3 : sig val entropy : string option Lazy.Atomic_repeating.t end
|}]
