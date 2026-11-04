(* TEST *)

(* The C-side bounds checks of caml_ephe_blit_key must reject negative
   or overflowing arguments: the stdlib wrappers check first, but the
   primitive validates its arguments by itself for direct callers. *)

external ephe_blit_key :
  Obj.Ephemeron.t -> int -> Obj.Ephemeron.t -> int -> int -> unit
  = "caml_ephe_blit_key"

let expect_invalid f =
  match f () with
  | () -> print_endline "NO EXCEPTION"
  | exception Invalid_argument _ -> ()

let () =
  let es = Obj.Ephemeron.create 4 in
  let ed = Obj.Ephemeron.create 4 in
  expect_invalid (fun () -> ephe_blit_key es 0 ed 0 (-2));
  expect_invalid (fun () -> ephe_blit_key es (-3) ed 0 2);
  expect_invalid (fun () -> ephe_blit_key es 0 ed (-3) 2);
  expect_invalid (fun () -> ephe_blit_key es 0 ed 0 max_int);
  expect_invalid (fun () -> ephe_blit_key es 3 ed 0 2);
  expect_invalid (fun () -> ephe_blit_key es 0 ed 3 2);
  ephe_blit_key es 1 ed 1 3;  (* legal blit still works *)
  print_endline "ok"
