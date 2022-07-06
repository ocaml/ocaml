(* TEST *)

(* Due to GCs running at non-deterministic places, the output from these tests
 * are unreliable except the bad value checks and as a check for catastrophic
 * failures i.e) segfaults. *)

let debug = false

open Printf
open Ephemeron

let dprintf x =
  if debug then printf x
  else ifprintf stdout x

let is_true test s b =
  if debug then
    printf "%s %s: %s\n" test s (if b then "OK" else "FAIL")

let is_false test s b = is_true test s (not b)

let is_data_value test eph k (v:int) =
  match K1.query eph k with
  | Some x ->
      if !x = v
      then dprintf "%s data set: OK\n" test
      else printf "%s data set: FAIL(bad value %i)\n" test (!x)
  | None -> dprintf "%s data set: FAIL\n" test

let is_data_unset test eph k =
  is_true test "data unset" (Option.is_none (K1.query eph k))

module M () = struct

let make_ra () = ref (ref 1) [@@inline never]
let make_rb () = ref (ref (ref 2)) [@@inline never]
let ra = make_ra ()
let rb = make_rb ()

(** test: key alive data dangling *)
let test1 () =
  let test = "test1" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.make !ra (ref 42) in
  is_data_value test eph !ra 42;
  Gc.minor ();
  is_data_value test eph !ra 42;
  Gc.full_major ();
  is_data_value test eph !ra 42;
  ra := ref 12;
  Gc.full_major ();
  is_data_unset test eph !ra

(** test: key dangling data dangling *)
let test2 () =
  let test = "test2" in
  Gc.minor ();
  Gc.full_major ();
  let k = ref 125 in
  let eph : (int ref, int ref) K1.t = K1.make k (ref 42) in
  is_data_value test eph k 42;
  Gc.minor ();
  is_data_unset test eph (ref 42)

(** test: key dangling data alive *)
let test3 () =
  let test = "test3" in
  Gc.minor ();
  Gc.full_major ();
  ra := ref 13;
  let k = ref 125 in
  let eph : (int ref, int ref) K1.t = K1.make k !ra in
  is_data_value test eph k 13;
  ra := ref 14;
  Gc.minor ();
  is_data_unset test eph (ref 125)

(** test: key alive but one away, data dangling *)
let test4 () =
  let test = "test4" in
  Gc.minor ();
  Gc.full_major ();
  rb := ref (ref 3);
  let eph : (int ref, int ref) K1.t = K1.make !(!rb) (ref 43) in
  is_data_value test eph !(!rb) 43;
  Gc.minor ();
  Gc.minor ();
  is_data_value test eph !(!rb) 43

(** test: key dangling but one away, data dangling *)
let test5 () =
  let test = "test5" in
  Gc.minor ();
  Gc.full_major ();
  rb := ref (ref 3);
  let eph : (int ref, int ref) K1.t = K1.make !(!rb) (ref 43) in
  is_data_value test eph !(!rb) 43;
  !rb := ref 4;
  Gc.minor ();
  Gc.minor ();
  is_data_unset test eph !(!rb)

(** test: key accessible from data but all dangling *)
let test6 () =
  let test = "test6" in
  Gc.minor ();
  Gc.full_major ();
  rb := ref (ref 3);
  let eph : (int ref, int ref ref) K1.t = K1.make !(!rb) (ref !(!rb)) in
  Gc.minor ();
  !rb := ref 4;
  Gc.full_major ();
  is_data_unset test eph !(!rb)

(** test: ephemeron accessible from data but they are dangling *)
type t =
  | No
  | Ephe of (int ref, t) K1.t

let rc = ref No

let test7 () =
  let test = "test7" in
  Gc.minor ();
  Gc.full_major ();
  ra := ref 42;
  let weak : t Weak.t = Weak.create 1 in
  let eph : (int ref, t) K1.t ref = ref (K1.make !ra !rc) in
  rc := Ephe !eph;
  Weak.set weak 0 (Some !rc);
  Gc.minor ();
  is_true test "before" (Weak.check weak 0);
  eph := K1.make (ref 0) No;
  rc := No;
  Gc.full_major ();
  Gc.full_major ();
  Gc.full_major ();
  is_false test "after" (Weak.check weak 0)

let run () =
  (test1 [@inlined never]) ();
  (test2 [@inlined never]) ();
  (test3 [@inlined never]) ();
  (test4 [@inlined never]) ();
  (test5 [@inlined never]) ();
  (test6 [@inlined never]) ();
  (test7 [@inlined never]) ();
  ()
end

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE")
  with Not_found | Failure _ -> 0

let _ =
  if test_size <= 1 then exit 0

let _ =
  for _ = 1 to 5 do
    let d = Array.init 3 (fun _ -> let module Mx = M () in Domain.spawn Mx.run) in
    let module Mx = M() in
    Mx.run ();
    Array.iter Domain.join d
  done
