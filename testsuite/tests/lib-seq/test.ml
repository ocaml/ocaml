(* TEST
*)

let filter1 x = x mod 2 = 0 ;;

(* Standard test case *)
let () =
  assert
    ([2;4] =
      (List.to_seq [1;2;3;4;5]
      |> Seq.filter (fun x -> x mod 2 = 0)
     |> List.of_seq));
  ()
;;

(* unfold *)
let () =
  let range first last =
    let step i = if i > last then None
                 else Some (i, succ i) in
    Seq.unfold step first
  in
  begin
    assert ([1;2;3] = List.of_seq (range 1 3));
    assert ([] = List.of_seq (range 1 0));
  end
;;

(* MPR 7820 *)
let () =
  assert
    ([| 1;2;3 |] =
      (Array.to_seq [| 1;2;3 |]
      |> Array.of_seq));
  ()
;;

(* delay *)
let () =
  (* Seq.concat is not (yet?) in the stdlib *)
  let rec concat xss = fun () ->
    match xss () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (xs, xss) -> Seq.append xs (concat xss) ()
  in
  let do_not_force_too_much =
    Seq.cons
      (Seq.return ())
      (Seq.delay @@ fun () -> Seq.return (assert false)) in
  match concat do_not_force_too_much () with
  | Seq.Nil -> assert false
  | Seq.Cons ((), seq) -> ignore seq

let () = print_endline "OK";;
