(* TEST
*)

let debug = false

open Printf
open Ephemeron

let is_true test s b = printf "%s %s: %s\n" test s (if b then "OK" else "FAIL")
let is_false test s b = is_true test s (not b)

let final r v = Gc.finalise_last (fun () -> r := false) v

let is_key_value test (key_alive, _) = is_true test "key set" !key_alive
let is_data_value test (_, data_alive) = is_true test "data set" !data_alive

let is_key_unset test (key_alive, _) = is_false test "key unset" !key_alive
let is_data_unset test (_, data_alive) = is_false test "data unset" !data_alive

let make_ra () = ref (ref 1) [@@inline never]
let make_rb () = ref (ref (ref 2)) [@@inline never]
let ra = make_ra ()
let rb = make_rb ()

let create key data =
  let key_alive = ref true in
  let data_alive = ref true in
  let eph = K1.make key data in
  final key_alive key;
  final data_alive data;
  (eph, (key_alive, data_alive))

(** test: key alive data dangling *)
let test1 () =
  let test = "test1" in
  Gc.minor ();
  Gc.full_major ();
  let (eph, flags) = create !ra (ref 42) in
  is_key_value test flags;
  is_data_value test flags;
  Gc.minor ();
  is_key_value test flags;
  is_data_value test flags;
  Gc.full_major ();
  is_key_value test flags;
  is_data_value test flags;
  ra := ref 12;
  Gc.full_major ();
  is_key_unset test flags;
  is_data_unset test flags;
  ignore (Sys.opaque_identity eph)
let () = (test1 [@inlined never]) ()

(** test: key dangling data dangling *)
let test2 () =
  let test = "test2" in
  Gc.minor ();
  Gc.full_major ();
  let (eph, flags) = create (ref 125) (ref 42) in
  is_key_value test flags;
  is_data_value test flags;
  ra := ref 13;
  Gc.minor ();
  is_key_unset test flags;
  is_data_unset test flags;
  ignore (Sys.opaque_identity eph)
let () = (test2 [@inlined never]) ()

(** test: key dangling data alive *)
let test3 () =
  let test = "test3" in
  Gc.minor ();
  Gc.full_major ();
  let (eph, flags) = create (ref 125) !ra in
  is_key_value test flags;
  is_data_value test flags;
  ra := ref 14;
  Gc.minor ();
  is_key_unset test flags;
  is_data_value test flags;
  ignore (Sys.opaque_identity eph)
let () = (test3 [@inlined never]) ()

(** test: key alive but one away, data dangling *)
let test4 () =
  let test = "test4" in
  Gc.minor ();
  Gc.full_major ();
  rb := ref (ref 3);
  let (eph, flags) = create !(!rb) (ref 43) in
  is_key_value test flags;
  is_data_value test flags;
  Gc.minor ();
  Gc.minor ();
  is_key_value test flags;
  is_data_value test flags;
  ignore (Sys.opaque_identity eph)
let () = (test4 [@inlined never]) ()

(** test: key dangling but one away, data dangling *)
let test5 () =
  let test = "test5" in
  Gc.minor ();
  Gc.full_major ();
  rb := ref (ref 3);
  let (eph, flags) = create !(!rb) (ref 43) in
  is_key_value test flags;
  is_data_value test flags;
  !rb := ref 4;
  Gc.minor ();
  Gc.minor ();
  is_key_unset test flags;
  is_data_unset test flags;
  ignore (Sys.opaque_identity eph)
let () = (test5 [@inlined never]) ()

(** test: key accessible from data but all dangling *)
let test6 () =
  let test = "test6" in
  Gc.minor ();
  Gc.full_major ();
  rb := ref (ref 3);
  let (eph, flags) = create !(!rb) (ref !(!rb)) in
  Gc.minor ();
  is_key_value test flags;
  !rb := ref 4;
  Gc.full_major ();
  is_key_unset test flags;
  is_data_unset test flags;
  ignore (Sys.opaque_identity eph)
let () = (test6 [@inlined never]) ()

(** test: ephemeron accessible from data but they are dangling *)
type t =
  | No
  | Ephe of (int ref, t ref) K1.t

let make_rc () = ref (ref No) [@@inline never]
let rc = make_rc ()

let test7 () =
  let test = "test7" in
  Gc.minor ();
  Gc.full_major ();
  ra := ref 42;
  let weak : t ref Weak.t = Weak.create 1 in
  let eph = ref (K1.make !ra !rc) in
  !rc := Ephe !eph;
  Weak.set weak 0 (Some !rc);
  Gc.minor ();
  is_true test "before" (Weak.check weak 0);
  eph := K1.make (ref 0) (ref No);
  rc := ref No;
  Gc.full_major ();
  Gc.full_major ();
  Gc.full_major ();
  is_false test "after" (Weak.check weak 0)
let () = (test7 [@inlined never]) ()
