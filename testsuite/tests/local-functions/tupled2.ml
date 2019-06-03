(* TEST
*)

(* PR#8705 *)

let test x =
  let tupled (x, y) = (); fun () -> [|x; y|] in
  match x with
  | None -> [| |]
  | Some (x, y) -> tupled (x, y) ()

let expected = "Hello "

let result = (test (Some (expected, "World!"))).(0)

let () = assert (String.equal expected result)
