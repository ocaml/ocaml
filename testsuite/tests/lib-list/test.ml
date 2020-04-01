(* TEST
*)

let string_of_even_opt x =
  if x mod 2 = 0 then
    Some (string_of_int x)
  else
    None

(* Standard test case *)
let () =
  let l = List.init 10 (fun x -> x) in
  let sl = List.init 10 string_of_int in
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

  begin
    let f ~limit a = if a >= limit then Some (a, limit) else None in
    assert (List.find_map (f ~limit:3) [] = None);
    assert (List.find_map (f ~limit:3) l = Some (3, 3));
    assert (List.find_map (f ~limit:30) l = None);
  end;

  assert (List.filteri (fun i _ -> i < 2) (List.rev l) = [9; 8]);

  assert (List.compare_lengths [] [] = 0);
  assert (List.compare_lengths [1;2] ['a';'b'] = 0);
  assert (List.compare_lengths [] [1;2] < 0);
  assert (List.compare_lengths ['a'] [1;2] < 0);
  assert (List.compare_lengths [1;2] [] > 0);
  assert (List.compare_lengths [1;2] ['a'] > 0);

  assert (List.compare_length_with [] 0 = 0);
  assert (List.compare_length_with [] 1 < 0);
  assert (List.compare_length_with [] (-1) > 0);
  assert (List.compare_length_with [] max_int < 0);
  assert (List.compare_length_with [] min_int > 0);
  assert (List.compare_length_with [1] 0 > 0);
  assert (List.compare_length_with ['1'] 1 = 0);
  assert (List.compare_length_with ['1'] 2 < 0);
  assert (List.filter_map string_of_even_opt l = ["0";"2";"4";"6";"8"]);
  assert (List.concat_map (fun i -> [i; i+1]) [1; 5] = [1; 2; 5; 6]);
  assert (
    let count = ref 0 in
    List.concat_map (fun i -> incr count; [i; !count]) [1; 5] = [1; 1; 5; 2]);
  assert (List.fold_left_map (fun a b -> a + b, b) 0 l = (45, l));
  assert (List.fold_left_map (fun a b -> assert false) 0 [] = (0, []));
  assert (
    let f a b = a + b, string_of_int b in
    List.fold_left_map f 0 l = (45, sl));
  ()
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

(* Evaluation order *)
let () =
  let test n =
    let result = ref false in
    let _ = List.init n (fun x -> result := (x = n - 1)) in
    assert !result
  in
  (* Threshold must equal the value in stdlib/list.ml *)
  let threshold = 10_000 in
  test threshold; (* Non tail-recursive case *)
  test (threshold + 1) (* Tail-recursive case *)
;;

let () = print_endline "OK";;
