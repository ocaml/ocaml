(* TEST *)

(* Copied from [test.ml], but with all the [Array.fill] tests deleted *)

(* [iarray]s don't have the [make*] functions, so we redefine them here *)
let make n x = Iarray.init n (fun _ -> x);;
let make_matrix m n x = make m (make n x);;

let () =
  let a : _ iarray = [|0;1;2;3;4;5;6;7;8;9|] in
  assert (Iarray.exists (fun a -> a < 10) a);
  assert (Iarray.exists (fun a -> a > 0) a);
  assert (Iarray.exists (fun a -> a = 0) a);
  assert (Iarray.exists (fun a -> a = 1) a);
  assert (Iarray.exists (fun a -> a = 2) a);
  assert (Iarray.exists (fun a -> a = 3) a);
  assert (Iarray.exists (fun a -> a = 4) a);
  assert (Iarray.exists (fun a -> a = 5) a);
  assert (Iarray.exists (fun a -> a = 6) a);
  assert (Iarray.exists (fun a -> a = 7) a);
  assert (Iarray.exists (fun a -> a = 8) a);
  assert (Iarray.exists (fun a -> a = 9) a);
  assert (not (Iarray.exists (fun a -> a < 0) a));
  assert (not (Iarray.exists (fun a -> a > 9) a));
  assert (Iarray.exists (fun _ -> true) a);
;;

let () =
  let a : _ iarray = [|1;2;3|] in
  assert (Iarray.exists (fun a -> a < 3) a);
  assert (Iarray.exists (fun a -> a < 2) a);
  assert (not (Iarray.exists (fun a -> a < 1) a));
  assert (Iarray.exists (fun a -> a mod 2 = 0)  [|1;4;5|]);
  assert (not (Iarray.exists (fun a -> a mod 2 = 0)  [|1;3;5|]));
  assert (not (Iarray.exists (fun _ -> true) [||]));
  assert (Iarray.exists (fun a -> Iarray.get a 9 = 1) (make_matrix 10 10 1));;
;;

let () =
  let a: int iarray = [||] in
  assert (not (Iarray.exists (fun a -> a = 0) a));
  assert (not (Iarray.exists (fun a -> a = 1) a));
  assert (not (Iarray.exists (fun a -> a = 2) a));
  assert (not (Iarray.exists (fun a -> a = 3) a));
  assert (not (Iarray.exists (fun a -> a = 4) a));
  assert (not (Iarray.exists (fun a -> a = 5) a));
  assert (not (Iarray.exists (fun a -> a = 6) a));
  assert (not (Iarray.exists (fun a -> a = 7) a));
  assert (not (Iarray.exists (fun a -> a = 8) a));
  assert (not (Iarray.exists (fun a -> a = 9) a));
  assert (not (Iarray.exists (fun a -> a <> 0) a));
  assert (not (Iarray.exists (fun a -> a <> 1) a));
  assert (not (Iarray.exists (fun a -> a <> 2) a));
  assert (not (Iarray.exists (fun a -> a <> 3) a));
  assert (not (Iarray.exists (fun a -> a <> 4) a));
  assert (not (Iarray.exists (fun a -> a <> 5) a));
  assert (not (Iarray.exists (fun a -> a <> 6) a));
  assert (not (Iarray.exists (fun a -> a <> 7) a));
  assert (not (Iarray.exists (fun a -> a <> 8) a));
  assert (not (Iarray.exists (fun a -> a <> 9) a));
  assert (not (Iarray.exists (fun a -> a < 0) a));
  assert (not (Iarray.exists (fun a -> a < 1) a));
  assert (not (Iarray.exists (fun a -> a < 2) a));
  assert (not (Iarray.exists (fun a -> a < 3) a));
  assert (not (Iarray.exists (fun a -> a < 4) a));
  assert (not (Iarray.exists (fun a -> a < 5) a));
  assert (not (Iarray.exists (fun a -> a < 6) a));
  assert (not (Iarray.exists (fun a -> a < 7) a));
  assert (not (Iarray.exists (fun a -> a < 8) a));
  assert (not (Iarray.exists (fun a -> a < 9) a));
  assert (not (Iarray.exists (fun a -> a > 0) a));
  assert (not (Iarray.exists (fun a -> a > 1) a));
  assert (not (Iarray.exists (fun a -> a > 2) a));
  assert (not (Iarray.exists (fun a -> a > 3) a));
  assert (not (Iarray.exists (fun a -> a > 4) a));
  assert (not (Iarray.exists (fun a -> a > 5) a));
  assert (not (Iarray.exists (fun a -> a > 6) a));
  assert (not (Iarray.exists (fun a -> a > 7) a));
  assert (not (Iarray.exists (fun a -> a > 8) a));
  assert (not (Iarray.exists (fun a -> a > 9) a));
;;

let () =
  let a : _ iarray = [|0;1;2;3;4;5;6;7;8;9|] in
  assert (Iarray.for_all (fun a -> a < 10) a);
  assert (Iarray.for_all (fun a -> a >= 0) a);
  assert (not (Iarray.for_all (fun a -> a = 0) a));
  assert (not (Iarray.for_all (fun a -> a = 1) a));
  assert (not (Iarray.for_all (fun a -> a = 2) a));
  assert (not (Iarray.for_all (fun a -> a = 3) a));
  assert (not (Iarray.for_all (fun a -> a = 4) a));
  assert (not (Iarray.for_all (fun a -> a = 5) a));
  assert (not (Iarray.for_all (fun a -> a = 6) a));
  assert (not (Iarray.for_all (fun a -> a = 7) a));
  assert (not (Iarray.for_all (fun a -> a = 8) a));
  assert (not (Iarray.for_all (fun a -> a = 9) a));
  assert (Iarray.for_all (fun a -> a <> 10) a);
  assert (Iarray.for_all (fun a -> a <> (-1)) a);
  assert (Iarray.for_all (fun _ -> true) a);
;;

let () =
  assert (Iarray.for_all (fun x -> x mod 2 = 0) [|2;4;6|]);
  assert (not (Iarray.for_all (fun x -> x mod 2 = 0) [|2;3;6|]));
  assert (Iarray.for_all (fun _ -> false) [||]);
  assert (Iarray.for_all (fun a -> Iarray.get a 9 = 1) (make_matrix 10 10 1));
;;
;;

let () =
  let a : _ iarray = [||] in
  assert (Iarray.for_all (fun a -> a < 10) a);
  assert (Iarray.for_all (fun a -> a >= 0) a);
  assert (Iarray.for_all (fun a -> a = 0) a);
  assert (Iarray.for_all (fun a -> a = 1) a);
  assert (Iarray.for_all (fun a -> a = 2) a);
  assert (Iarray.for_all (fun a -> a = 3) a);
  assert (Iarray.for_all (fun a -> a = 4) a);
  assert (Iarray.for_all (fun a -> a = 5) a);
  assert (Iarray.for_all (fun a -> a = 6) a);
  assert (Iarray.for_all (fun a -> a = 7) a);
  assert (Iarray.for_all (fun a -> a = 8) a);
  assert (Iarray.for_all (fun a -> a = 9) a);
  assert (Iarray.for_all (fun a -> a <> 10) a);
  assert (Iarray.for_all (fun a -> a <> (-1)) a);
  assert (Iarray.for_all (fun _ -> true) a);
;;

let does_raise3 f a b c =
  try
    ignore (f a b c);
    false
  with _ ->
    true

let () =
  let a : _ iarray = [|1;2;3;4;5;6;7;8;9|]
  and b : _ iarray = [|1;2;3;4;5;6;7;8;9|] in
  assert (Iarray.exists2 (fun a b -> a = b) a b);
  assert (Iarray.exists2 (fun a b -> a - b = 0) a b);
  assert (Iarray.exists2 (fun a b -> a = 1 && b = 1) a b);
  assert (Iarray.exists2 (fun a b -> a = 2 && b = 2) a b);
  assert (Iarray.exists2 (fun a b -> a = 3 && b = 3) a b);
  assert (Iarray.exists2 (fun a b -> a = 4 && b = 4) a b);
  assert (Iarray.exists2 (fun a b -> a = 5 && b = 5) a b);
  assert (Iarray.exists2 (fun a b -> a = 6 && b = 6) a b);
  assert (Iarray.exists2 (fun a b -> a = 7 && b = 7) a b);
  assert (Iarray.exists2 (fun a b -> a = 8 && b = 8) a b);
  assert (Iarray.exists2 (fun a b -> a = 9 && b = 9) a b);
  assert (not (Iarray.exists2 (fun a b -> a <> b) a b));
;;

let () =
  let a : _ iarray = [|1|]
  and b : _ iarray = [|1;2|] in
  assert (does_raise3 Iarray.exists2 (fun a b -> a = b) a b);
  assert (does_raise3 Iarray.exists2 (fun _ _ -> true) a b);
  assert (does_raise3 Iarray.exists2 (fun _ _ -> false) a b);
  assert (does_raise3 Iarray.exists2 (fun a b -> a = 1 && b = 1) a b);
  assert (does_raise3 Iarray.exists2 (fun a b -> a = 2 && b = 2) a b);
  assert (does_raise3 Iarray.exists2 (fun a b -> a = 3 && b = 3) a b);
  assert (does_raise3 Iarray.exists2 (fun a b -> a = 4 && b = 4) a b);
  assert (does_raise3 Iarray.exists2 (fun a b -> a = 5 && b = 5) a b);
  assert (does_raise3 Iarray.exists2 (fun a b -> a = 6 && b = 6) a b);
  assert (does_raise3 Iarray.exists2 (fun a b -> a = 7 && b = 7) a b);
  assert (does_raise3 Iarray.exists2 (fun a b -> a = 8 && b = 8) a b);
  assert (does_raise3 Iarray.exists2 (fun a b -> a = 9 && b = 9) a b);
;;

let () =
  assert (Iarray.exists2 (=) [|1;2;3|] [|3;2;1|]);
  assert (not (Iarray.exists2 (<>) [|1;2;3|] [|1;2;3|]));
  assert (does_raise3 Iarray.exists2 (=) [|1;2|] [|3|]);
;;

let () =
  let a : _ iarray = [|1;2;3;4;5;6;7;8;9|]
  and b : _ iarray = [|1;2;3;4;5;6;7;8;9|] in
  assert (Iarray.for_all2 (fun a b -> a = b) a b);
  assert (Iarray.for_all2 (fun a b -> a - b = 0) a b);
  assert (Iarray.for_all2 (fun a b -> a > 0 && b > 0) a b);
  assert (Iarray.for_all2 (fun a b -> a < 10 && b < 10) a b);
  assert (Iarray.for_all2 (fun a b -> if a = 1 then b = 1 else b <> 1) a b);
  assert (Iarray.for_all2 (fun a b -> if a = 2 then b = 2 else b <> 2) a b);
  assert (Iarray.for_all2 (fun a b -> if a = 3 then b = 3 else b <> 3) a b);
  assert (Iarray.for_all2 (fun a b -> if a = 4 then b = 4 else b <> 4) a b);
  assert (Iarray.for_all2 (fun a b -> if a = 5 then b = 5 else b <> 5) a b);
  assert (Iarray.for_all2 (fun a b -> if a = 6 then b = 6 else b <> 6) a b);
  assert (Iarray.for_all2 (fun a b -> if a = 7 then b = 7 else b <> 7) a b);
  assert (Iarray.for_all2 (fun a b -> if a = 8 then b = 8 else b <> 8) a b);
  assert (Iarray.for_all2 (fun a b -> if a = 9 then b = 9 else b <> 9) a b);
  assert (not (Iarray.for_all2 (fun a b -> a <> b) a b));
;;

let () =
  let a : _ iarray = [|1|]
  and b : _ iarray = [|1;2|] in
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = b) a b);
  assert (does_raise3 Iarray.for_all2 (fun _ _ -> true) a b);
  assert (does_raise3 Iarray.for_all2 (fun _ _ -> false) a b);
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = 1 && b = 1) a b);
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = 2 && b = 2) a b);
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = 3 && b = 3) a b);
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = 4 && b = 4) a b);
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = 5 && b = 5) a b);
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = 6 && b = 6) a b);
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = 7 && b = 7) a b);
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = 8 && b = 8) a b);
  assert (does_raise3 Iarray.for_all2 (fun a b -> a = 9 && b = 9) a b);
;;

let () =
  assert (not (Iarray.for_all2 (=) [|1;2;3|] [|3;2;1|]));
  assert (Iarray.for_all2 (=) [|1;2;3|] [|1;2;3|]);
  assert (not (Iarray.for_all2 (<>) [|1;2;3|] [|3;2;1|]));
  assert (does_raise3 Iarray.for_all2 (=) [|1;2;3|] [|1;2;3;4|]);
  assert (does_raise3 Iarray.for_all2 (=) [|1;2|] [||]);
;;

let () =
  let a : _ iarray = [|1;2;3;4;5;6;7;8;9|] in
  assert (Iarray.mem 1 a);
  assert (Iarray.mem 2 a);
  assert (Iarray.mem 3 a);
  assert (Iarray.mem 4 a);
  assert (Iarray.mem 5 a);
  assert (Iarray.mem 6 a);
  assert (Iarray.mem 7 a);
  assert (Iarray.mem 8 a);
  assert (Iarray.mem 9 a);
  assert (not (Iarray.mem 0 a));
  assert (not (Iarray.mem 10 a));
;;

let () =
  assert (Iarray.mem 2 [|1;2;3|]);
  assert (not (Iarray.mem 2 [||]));
  assert (Iarray.mem (ref 3) [|ref 1; ref 2; ref 3|]);
  assert (Iarray.mem [|1;2;3|] [|[|1;2;3|];[|2;3;4|];[|0|]|]);
  assert (Iarray.mem 1 (make 100 1));
  assert (Iarray.mem (ref 1) (make 100 (ref 1)));
;;

let () =
  let a : _ iarray = [|1;2;3;4;5;6;7;8;9|] in
  assert (Iarray.memq 1 a);
  assert (Iarray.memq 2 a);
  assert (Iarray.memq 3 a);
  assert (Iarray.memq 4 a);
  assert (Iarray.memq 5 a);
  assert (Iarray.memq 6 a);
  assert (Iarray.memq 7 a);
  assert (Iarray.memq 8 a);
  assert (Iarray.memq 9 a);
  assert (not (Iarray.memq 0 a));
  assert (not (Iarray.memq 10 a));
;;

let () =
  assert (Iarray.memq 2 [|1;2;3|]);
  assert (not (Iarray.memq 2 [||]));
  assert (not (Iarray.memq (ref 3) [|ref 1; ref 2; ref 3|]));
  (* The below tests immutable arrays of *mutable* arrays because physical
     equality is only guaranteed to be distinct for mutable values *)
  assert (not (Iarray.memq [|1;2;3|] [|[|1;2;3|];[|2;3;4|];[|0|]|]));
  assert (Iarray.memq 1 (make 100 1));
  assert (not (Iarray.memq (ref 1) (make 100 (ref 1))));
;;

let () = print_endline "OK"
