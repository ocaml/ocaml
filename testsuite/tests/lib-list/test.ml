(* Standard test case *)
let () =
  let l = List.init 10 (fun x -> x) in
  assert (List.exists (fun a -> a < 10) l);
  assert (List.exists (fun a -> a > 0) l);
  assert (List.exists (fun a -> a = 0) l);
  assert (List.exists (fun a -> a = 1) l);
  assert (List.exists (fun a -> a = 2) l);
  assert (List.exists (fun a -> a = 3) l);
  assert (List.exists (fun a -> a = 4) l);
  assert (List.exists (fun a -> a = 5) l);
  assert (List.exists (fun a -> a = 6) l);
  assert (List.exists (fun a -> a = 7) l);
  assert (List.exists (fun a -> a = 8) l);
  assert (List.exists (fun a -> a = 9) l);
  assert (not (List.exists (fun a -> a < 0) l));
  assert (not (List.exists (fun a -> a > 9) l));
  assert (List.exists (fun _ -> true) l);
;;

(* Empty test case *)
let () =
  assert ((List.init 0 (fun x -> x)) = []);
;;

(* Erroneous test case *)

let () =
  let result = try
      let _ = List.init (-1) (fun x -> x) in false
  with Invalid_argument e -> true (* Exception caught *)
  in assert result;
;;

(* Evaluation order (non tail-recursive) *)

let () =
  let result = ref false in
  let _ = List.init 2 (fun x ->
      if x = 0 then result := false
      else if x = 1 then result := true
    )
  in assert !result
;;

(* Evaluation order (tail-recursive) *)
let () =
  let result = ref false in
  let _ = List.init 10_001 (fun x ->
      if x = 9999 then result := false
      else if x = 10000 then result := true
    )
  in assert !result
;;

let () = print_endline "OK";;
