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
  assert (List.of_seq (Seq.range 0 5) = [0;1;2;3;4]);
  assert (List.of_seq (Seq.range ~step:(-1) 5 0 ) = [5;4;3;2;1]);
  let start = -2 and stop = 12 and step = 3 in 
  let l= List.of_seq (Seq.range ~step start stop) in
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

(* count_from *)
let () = 
  let start = 12 and stop = 25 and step = 3 in
  let s : int Seq.t ref = ref (Seq.count_from ~step start) in 
  (Seq.range ~step start stop) 
  |> Seq.iter begin fun x -> 
    match (!s) () with 
    | Cons (y, new_s)  -> assert (x = y); s := new_s
    | Nil -> assert false 
  end


let () = print_endline "OK";;
