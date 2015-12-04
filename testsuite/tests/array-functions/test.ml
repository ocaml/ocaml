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

let does_raise3 f a b c =
  try
    ignore (f a b c);
    false
  with _ ->
    true

let () =
  let a = [|1;2;3;4;5;6;7;8;9|]  
  and b = [|1;2;3;4;5;6;7;8;9|] in
  assert (Array.exists2 (fun a b -> a = b) a b);
  assert (Array.exists2 (fun a b -> a - b = 0) a b);
  assert (Array.exists2 (fun a b -> a = 1 && b = 1) a b);
  assert (Array.exists2 (fun a b -> a = 2 && b = 2) a b);
  assert (Array.exists2 (fun a b -> a = 3 && b = 3) a b);
  assert (Array.exists2 (fun a b -> a = 4 && b = 4) a b);
  assert (Array.exists2 (fun a b -> a = 5 && b = 5) a b);
  assert (Array.exists2 (fun a b -> a = 6 && b = 6) a b);
  assert (Array.exists2 (fun a b -> a = 7 && b = 7) a b);
  assert (Array.exists2 (fun a b -> a = 8 && b = 8) a b);
  assert (Array.exists2 (fun a b -> a = 9 && b = 9) a b);
  assert (not (Array.exists2 (fun a b -> a <> b) a b));
;;

let () =
  let a = [|1|]
  and b = [|1;2|] in
  assert (does_raise3 Array.exists2 (fun a b -> a = b) a b);
  assert (does_raise3 Array.exists2 (fun _ _ -> true) a b);
  assert (does_raise3 Array.exists2 (fun _ _ -> false) a b);
  assert (does_raise3 Array.exists2 (fun a b -> a = 1 && b = 1) a b);
  assert (does_raise3 Array.exists2 (fun a b -> a = 2 && b = 2) a b);
  assert (does_raise3 Array.exists2 (fun a b -> a = 3 && b = 3) a b);
  assert (does_raise3 Array.exists2 (fun a b -> a = 4 && b = 4) a b);
  assert (does_raise3 Array.exists2 (fun a b -> a = 5 && b = 5) a b);
  assert (does_raise3 Array.exists2 (fun a b -> a = 6 && b = 6) a b);
  assert (does_raise3 Array.exists2 (fun a b -> a = 7 && b = 7) a b);
  assert (does_raise3 Array.exists2 (fun a b -> a = 8 && b = 8) a b);
  assert (does_raise3 Array.exists2 (fun a b -> a = 9 && b = 9) a b);
;;

let () =
  let a = [|1;2;3;4;5;6;7;8;9|]  
  and b = [|1;2;3;4;5;6;7;8;9|] in
  assert (Array.for_all2 (fun a b -> a = b) a b);
  assert (Array.for_all2 (fun a b -> a - b = 0) a b);
  assert (Array.for_all2 (fun a b -> a > 0 && b > 0) a b);
  assert (Array.for_all2 (fun a b -> a < 10 && b < 10) a b);
  assert (Array.for_all2 (fun a b -> if a = 1 then b = 1 else b <> 1) a b);
  assert (Array.for_all2 (fun a b -> if a = 2 then b = 2 else b <> 2) a b);
  assert (Array.for_all2 (fun a b -> if a = 3 then b = 3 else b <> 3) a b);
  assert (Array.for_all2 (fun a b -> if a = 4 then b = 4 else b <> 4) a b);
  assert (Array.for_all2 (fun a b -> if a = 5 then b = 5 else b <> 5) a b);
  assert (Array.for_all2 (fun a b -> if a = 6 then b = 6 else b <> 6) a b);
  assert (Array.for_all2 (fun a b -> if a = 7 then b = 7 else b <> 7) a b);
  assert (Array.for_all2 (fun a b -> if a = 8 then b = 8 else b <> 8) a b);
  assert (Array.for_all2 (fun a b -> if a = 9 then b = 9 else b <> 9) a b);
  assert (not (Array.for_all2 (fun a b -> a <> b) a b));
;;

let () =
  let a = [|1|]
  and b = [|1;2|] in
  assert (does_raise3 Array.for_all2 (fun a b -> a = b) a b);
  assert (does_raise3 Array.for_all2 (fun _ _ -> true) a b);
  assert (does_raise3 Array.for_all2 (fun _ _ -> false) a b);
  assert (does_raise3 Array.for_all2 (fun a b -> a = 1 && b = 1) a b);
  assert (does_raise3 Array.for_all2 (fun a b -> a = 2 && b = 2) a b);
  assert (does_raise3 Array.for_all2 (fun a b -> a = 3 && b = 3) a b);
  assert (does_raise3 Array.for_all2 (fun a b -> a = 4 && b = 4) a b);
  assert (does_raise3 Array.for_all2 (fun a b -> a = 5 && b = 5) a b);
  assert (does_raise3 Array.for_all2 (fun a b -> a = 6 && b = 6) a b);
  assert (does_raise3 Array.for_all2 (fun a b -> a = 7 && b = 7) a b);
  assert (does_raise3 Array.for_all2 (fun a b -> a = 8 && b = 8) a b);
  assert (does_raise3 Array.for_all2 (fun a b -> a = 9 && b = 9) a b);
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

let does_raise2 f a b =
  try
    ignore (f a b);
    false
  with _ ->
    true

let () =
  let a = [|1;2;3;4;5;6;7;8;9|] in
  assert (Array.find (fun a -> a = 1) a = 1);
  assert (Array.find (fun a -> a = 2) a = 2);
  assert (Array.find (fun a -> a = 3) a = 3);
  assert (Array.find (fun a -> a = 4) a = 4);
  assert (Array.find (fun a -> a = 5) a = 5);
  assert (Array.find (fun a -> a = 6) a = 6);
  assert (Array.find (fun a -> a = 7) a = 7);
  assert (Array.find (fun a -> a = 8) a = 8);
  assert (Array.find (fun a -> a = 9) a = 9);
  assert (Array.find (fun a -> a > 1) a = 2);
  assert (Array.find (fun a -> a > 2) a = 3);
  assert (Array.find (fun a -> a > 3) a = 4);
  assert (Array.find (fun a -> a > 4) a = 5);
  assert (Array.find (fun a -> a > 5) a = 6);
  assert (Array.find (fun a -> a > 6) a = 7);
  assert (Array.find (fun a -> a > 7) a = 8);
  assert (Array.find (fun a -> a > 8) a = 9);
  assert (does_raise2 Array.find (fun a -> a < 0) a);
  assert (does_raise2 Array.find (fun a -> a > 9) a);
;;

let () = print_endline "OK"
