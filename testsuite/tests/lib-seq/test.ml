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

let () = print_endline "OK";;
