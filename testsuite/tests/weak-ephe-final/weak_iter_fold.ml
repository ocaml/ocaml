(* TEST *)

let check test b =
  Printf.printf "%s: %s\n" test (if b then "OK" else "FAIL")

(* --- iter --- *)

let test_iter_empty () =
  let w : int ref Weak.t = Weak.create 5 in
  let count = ref 0 in
  Weak.iter (fun _ -> incr count) w;
  check "iter_empty" (!count = 0)
let () = (test_iter_empty [@inlined never]) ()

let test_iter_partial () =
  let w = Weak.create 5 in
  let v0 = ref 0 and v2 = ref 2 and v4 = ref 4 in
  Weak.set w 0 (Some v0);
  Weak.set w 2 (Some v2);
  Weak.set w 4 (Some v4);
  let seen = ref [] in
  Weak.iter (fun v -> seen := !v :: !seen) w;
  check "iter_partial" (List.rev !seen = [0; 2; 4]);
  ignore (Sys.opaque_identity (v0, v2, v4))
let () = (test_iter_partial [@inlined never]) ()

(* --- iteri --- *)

let test_iteri () =
  let w = Weak.create 5 in
  let v1 = ref 10 and v3 = ref 30 in
  Weak.set w 1 (Some v1);
  Weak.set w 3 (Some v3);
  let seen = ref [] in
  Weak.iteri (fun i v -> seen := (i, !v) :: !seen) w;
  check "iteri" (List.rev !seen = [(1, 10); (3, 30)]);
  ignore (Sys.opaque_identity (v1, v3))
let () = (test_iteri [@inlined never]) ()

(* --- foldi_left --- *)

let test_foldi_left () =
  let w = Weak.create 5 in
  let v0 = ref 1 and v2 = ref 2 and v4 = ref 4 in
  Weak.set w 0 (Some v0);
  Weak.set w 2 (Some v2);
  Weak.set w 4 (Some v4);
  let sum = Weak.foldi_left (fun _ acc v -> acc + !v) 0 w in
  check "foldi_left_sum" (sum = 7);
  let pairs = Weak.foldi_left (fun i acc v -> acc @ [(i, !v)]) [] w in
  check "foldi_left_indices" (pairs = [(0, 1); (2, 2); (4, 4)]);
  ignore (Sys.opaque_identity (v0, v2, v4))
let () = (test_foldi_left [@inlined never]) ()

(* --- GC erasure --- *)

let test_gc_erasure () =
  let w = Weak.create 3 in
  let v0 = ref 0 and v2 = ref 2 in
  Weak.set w 0 (Some v0);
  Weak.set w 1 (Some (ref 99)); (* no other strong reference *)
  Weak.set w 2 (Some v2);
  Gc.full_major ();
  let seen = ref [] in
  Weak.iter (fun v -> seen := !v :: !seen) w;
  check "gc_erasure" (List.rev !seen = [0; 2]);
  ignore (Sys.opaque_identity (v0, v2))
let () = (test_gc_erasure [@inlined never]) ()
