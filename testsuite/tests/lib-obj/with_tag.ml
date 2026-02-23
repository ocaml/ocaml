(* TEST *)

type t =
| A of string * float
| B of string * float

let () =
  assert (Obj.dup (Obj.repr (A ("hello", 10.))) = Obj.repr (A ("hello", 10.)));
  assert (Obj.with_tag 1 (Obj.repr (A ("hello", 10.))) = Obj.repr (B ("hello", 10.)))

let () =
  assert (Obj.tag (Obj.with_tag 42 (Obj.repr [| |])) = 42)

(* check optimisations *)
let raw_allocs f =
  let before = Gc.minor_words () in
  f ();
  let after = Gc.minor_words () in
  int_of_float (after -. before)

let allocs =
  let overhead = raw_allocs (fun () -> ()) in
  fun f -> raw_allocs f - overhead

let () =
  assert (allocs (fun () -> Obj.with_tag 1 (Obj.repr (A ("hello", 10.)))) = 0);
  assert (allocs (fun () -> Obj.with_tag 1 (Obj.repr (ref 10))) = 2)

(* check forbidden cases *)

let () =
  let [@opaque] rec f n = if n < 0 then 0 else g (n-1)
  and [@opaque]     g n = if n < 0 then 0 else f (n-1)

  in
  let non_infix_to_infix () =
    ignore (Obj.with_tag (Obj.tag (Obj.repr f)) (Obj.repr g))
  in
  let infix_to_non_infix () =
    ignore (Obj.with_tag (Obj.tag (Obj.repr g)) (Obj.repr f))
  in
  (* Other illegalities left as discipline for the caller *)
  let check_fail f m =
    match f () with
    | () -> print_endline (m ^ ": did not fail")
    | exception Failure _ -> ()
    | exception e -> print_endline (m ^ "failed with " ^ Printexc.to_string e)
  in
  (check_fail non_infix_to_infix "Duplicating non-infix block with infix tag";
   check_fail infix_to_non_infix "Duplicating infix block with non-infix tag";)

(* Check duplication of closures and infix closures *)

let () =
  let [@opaque] rec f n = if n < 0 then 0 else g (n-1)
  and [@opaque]     g n = if n < 0 then 0 else f (n-1)
  in
  (ignore (Obj.dup (Obj.repr f));
   ignore (Obj.dup (Obj.repr g)))


let () =
  print_endline "ok"
