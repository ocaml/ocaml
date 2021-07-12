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

(* concat *)
let () =
  assert (
      List.concat [[1]; []; [2; 3];]
      = (let (!?) = List.to_seq in
         List.of_seq (Seq.concat !?[!?[1]; !?[]; !?[2; 3]])))

(* range *)
let () = 
  assert (List.of_seq (Seq.range ~stop:5) = [0;1;2;3;4]);
  assert (List.of_seq (Seq.range ~start:5 ~stop:0 ~step:(-1)) = [5;4;3;2;1]);
  let start = -2 and stop = 12 and step = 3 in 
  let l= List.of_seq (Seq.range ~start ~stop ~step) in
  l 
  |> List.iteri begin fun index value -> 
      assert (value = start + index * step); 
      if step > 0 then
        assert (value < stop)
      else if step < 0 then 
        assert (value > stop)
  end ;
  assert (List.length l = (abs ((stop - start) / step)) + 1) 
;;

let () = print_endline "OK";;
