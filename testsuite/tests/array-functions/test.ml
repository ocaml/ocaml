let () =
  let a = [|0;1;2;3;4;5;6;7;8;9|] in
  assert (Array.exists (fun a -> a < 10) a);
  assert (Array.exists (fun a -> a > 0) a);
  assert (Array.exists (fun a -> a = 0) a);
  assert (Array.exists (fun a -> a = 1) a);
  assert (Array.exists (fun a -> a = 2) a);
  assert (Array.exists (fun a -> a = 3) a);
  assert (Array.exists (fun a -> a = 4) a);
  assert (Array.exists (fun a -> a = 5) a);
  assert (Array.exists (fun a -> a = 6) a);
  assert (Array.exists (fun a -> a = 7) a);
  assert (Array.exists (fun a -> a = 8) a);
  assert (Array.exists (fun a -> a = 9) a);
  assert (not (Array.exists (fun a -> a < 0) a));
  assert (not (Array.exists (fun a -> a > 9) a));
  assert (Array.exists (fun _ -> true) a);
;;

let () =
  let a = [|1;2;3|] in
  assert (Array.exists (fun a -> a < 3) a);
  assert (Array.exists (fun a -> a < 2) a);
  assert (not (Array.exists (fun a -> a < 1) a));
  assert (Array.exists (fun a -> a mod 2 = 0)  [|1;4;5|]);
  assert (not (Array.exists (fun a -> a mod 2 = 0)  [|1;3;5|]));
  assert (not (Array.exists (fun _ -> true) [||]));
  assert (Array.exists (fun a -> a.(9) = 1) (Array.make_matrix 10 10 1));
  let f = Array.create_float 10 in
  Array.fill f 0 10 1.0;
  assert (Array.exists (fun a -> a = 1.0) f);
;;

let () =
  let a: int array = [||] in
  assert (not (Array.exists (fun a -> a = 0) a));
  assert (not (Array.exists (fun a -> a = 1) a));
  assert (not (Array.exists (fun a -> a = 2) a));
  assert (not (Array.exists (fun a -> a = 3) a));
  assert (not (Array.exists (fun a -> a = 4) a));
  assert (not (Array.exists (fun a -> a = 5) a));
  assert (not (Array.exists (fun a -> a = 6) a));
  assert (not (Array.exists (fun a -> a = 7) a));
  assert (not (Array.exists (fun a -> a = 8) a));
  assert (not (Array.exists (fun a -> a = 9) a));
  assert (not (Array.exists (fun a -> a <> 0) a));
  assert (not (Array.exists (fun a -> a <> 1) a));
  assert (not (Array.exists (fun a -> a <> 2) a));
  assert (not (Array.exists (fun a -> a <> 3) a));
  assert (not (Array.exists (fun a -> a <> 4) a));
  assert (not (Array.exists (fun a -> a <> 5) a));
  assert (not (Array.exists (fun a -> a <> 6) a));
  assert (not (Array.exists (fun a -> a <> 7) a));
  assert (not (Array.exists (fun a -> a <> 8) a));
  assert (not (Array.exists (fun a -> a <> 9) a));
  assert (not (Array.exists (fun a -> a < 0) a));
  assert (not (Array.exists (fun a -> a < 1) a));
  assert (not (Array.exists (fun a -> a < 2) a));
  assert (not (Array.exists (fun a -> a < 3) a));
  assert (not (Array.exists (fun a -> a < 4) a));
  assert (not (Array.exists (fun a -> a < 5) a));
  assert (not (Array.exists (fun a -> a < 6) a));
  assert (not (Array.exists (fun a -> a < 7) a));
  assert (not (Array.exists (fun a -> a < 8) a));
  assert (not (Array.exists (fun a -> a < 9) a));
  assert (not (Array.exists (fun a -> a > 0) a));
  assert (not (Array.exists (fun a -> a > 1) a));
  assert (not (Array.exists (fun a -> a > 2) a));
  assert (not (Array.exists (fun a -> a > 3) a));
  assert (not (Array.exists (fun a -> a > 4) a));
  assert (not (Array.exists (fun a -> a > 5) a));
  assert (not (Array.exists (fun a -> a > 6) a));
  assert (not (Array.exists (fun a -> a > 7) a));
  assert (not (Array.exists (fun a -> a > 8) a));
  assert (not (Array.exists (fun a -> a > 9) a));
;;

let () =
  let a = [|0;1;2;3;4;5;6;7;8;9|] in
  assert (Array.for_all (fun a -> a < 10) a);
  assert (Array.for_all (fun a -> a >= 0) a);
  assert (not (Array.for_all (fun a -> a = 0) a));
  assert (not (Array.for_all (fun a -> a = 1) a));
  assert (not (Array.for_all (fun a -> a = 2) a));
  assert (not (Array.for_all (fun a -> a = 3) a));
  assert (not (Array.for_all (fun a -> a = 4) a));
  assert (not (Array.for_all (fun a -> a = 5) a));
  assert (not (Array.for_all (fun a -> a = 6) a));
  assert (not (Array.for_all (fun a -> a = 7) a));
  assert (not (Array.for_all (fun a -> a = 8) a));
  assert (not (Array.for_all (fun a -> a = 9) a));
  assert (Array.for_all (fun a -> a <> 10) a);
  assert (Array.for_all (fun a -> a <> (-1)) a);
  assert (Array.for_all (fun _ -> true) a);
;;

let () =
  assert (Array.for_all (fun x -> x mod 2 = 0) [|2;4;6|]);
  assert (not (Array.for_all (fun x -> x mod 2 = 0) [|2;3;6|]));
  assert (Array.for_all (fun _ -> false) [||]);
  assert (Array.for_all (fun a -> a.(9) = 1) (Array.make_matrix 10 10 1));
  let f = Array.create_float 10 in
  Array.fill f 0 10 1.0;
  assert (Array.for_all (fun a -> a = 1.0) f);
;;
;;

let () =
  let a = [||] in
  assert (Array.for_all (fun a -> a < 10) a);
  assert (Array.for_all (fun a -> a >= 0) a);
  assert (Array.for_all (fun a -> a = 0) a);
  assert (Array.for_all (fun a -> a = 1) a);
  assert (Array.for_all (fun a -> a = 2) a);
  assert (Array.for_all (fun a -> a = 3) a);
  assert (Array.for_all (fun a -> a = 4) a);
  assert (Array.for_all (fun a -> a = 5) a);
  assert (Array.for_all (fun a -> a = 6) a);
  assert (Array.for_all (fun a -> a = 7) a);
  assert (Array.for_all (fun a -> a = 8) a);
  assert (Array.for_all (fun a -> a = 9) a);
  assert (Array.for_all (fun a -> a <> 10) a);
  assert (Array.for_all (fun a -> a <> (-1)) a);
  assert (Array.for_all (fun _ -> true) a);
;;


let () =
  let a = [|1;2;3;4;5;6;7;8;9|] in
  assert (Array.mem 1 a);
  assert (Array.mem 2 a);
  assert (Array.mem 3 a);
  assert (Array.mem 4 a);
  assert (Array.mem 5 a);
  assert (Array.mem 6 a);
  assert (Array.mem 7 a);
  assert (Array.mem 8 a);
  assert (Array.mem 9 a);
  assert (not (Array.mem 0 a));
  assert (not (Array.mem 10 a));
;;

let () =
  assert (Array.mem 2 [|1;2;3|]);
  assert (not (Array.mem 2 [||]));
  assert (Array.mem (ref 3) [|ref 1; ref 2; ref 3|]);
  assert (Array.mem [|1;2;3|] [|[|1;2;3|];[|2;3;4|];[|0|]|]);
  assert (Array.mem 1 (Array.make 100 1));
  assert (Array.mem (ref 1) (Array.make 100 (ref 1)));
  let f = Array.create_float 10 in
  Array.fill f 0 10 1.0;
  assert (Array.mem 1.0 f);
;;

let () =
  let a = [|1;2;3;4;5;6;7;8;9|] in
  assert (Array.memq 1 a);
  assert (Array.memq 2 a);
  assert (Array.memq 3 a);
  assert (Array.memq 4 a);
  assert (Array.memq 5 a);
  assert (Array.memq 6 a);
  assert (Array.memq 7 a);
  assert (Array.memq 8 a);
  assert (Array.memq 9 a);
  assert (not (Array.memq 0 a));
  assert (not (Array.memq 10 a));
;;

let () =
  assert (Array.memq 2 [|1;2;3|]);
  assert (not (Array.memq 2 [||]));
  assert (not (Array.memq (ref 3) [|ref 1; ref 2; ref 3|]));
  assert (not (Array.memq [|1;2;3|] [|[|1;2;3|];[|2;3;4|];[|0|]|]));
  assert (Array.memq 1 (Array.make 100 1));
  assert (not (Array.memq (ref 1) (Array.make 100 (ref 1))));
  let f = Array.create_float 10 in
  Array.fill f 0 10 1.0;
  assert (not (Array.memq 1.0 f));
;;

let () =
  (* Testing Array.filter *)
  let p2 n = n mod 2 = 0
  and p3 n = n mod 3 <> 0
  and p_none n = false
  and p_all n = true in
  let rec interval n m =
    if n > m then [] else n :: interval (n+1) m in
  let open Array in
  let check p n m =
    let xs = interval n m in
    filter p (of_list xs) = of_list (List.filter p xs) in

  begin

    (* empty array *)
    assert (check p3 0 0);

    (* input has 8 elements *)
    assert (check p_none 0 7);
    assert (check p_all 0 7);
    assert (check p3 0 7);
    assert (check p2 0 7);

    (* input has 9 elements *)
    assert (check p_none 0 8);
    assert (check p3 0 8);
    assert (check p_all 0 8);

    (* output has 7 elements *)
    assert (check p2 0 12);

    (* output has 8 elements *)
    assert (check p2 0 14);
  end
;;

let () = print_endline "OK"
