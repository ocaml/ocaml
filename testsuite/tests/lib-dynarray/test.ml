(* TEST
*)

module A = Dyn_array

let () =
  let a = A.create() in
  A.push a 1;
  A.push a 2;
  assert (A.to_list a = [1;2]);;

let () =
  let a = A.create() in
  A.push a 1;
  A.push a 2;
  A.push a 3;
  assert (A.length a = 3);;

let () =
  let a = A.make 1 5 in
  A.push a 6;
  assert (A.to_list a = [5;6]);;

let () =
  List.iter
    (fun l ->
      let a = A.of_list l in
      assert (A.to_list a = l))
    [
      [];
      [1];
      [1;2];
      [1;2;3];
      [1;2;3;4];
      [1;2;3;4;5;6;7;8;9;10];
    ]
;;

let () =
  let a = A.create() in
  A.push a 0.; A.push a 1.;
  A.clear a;
  A.push a 0.; A.push a 1.; A.push a 7.; A.push a 10.; A.push a 12.;
  A.truncate a 2;
  assert (1. = A.fold_left (+.) 0. a);
  A.clear a;
  assert (0 = A.length a);
  A.push a 0.; A.push a 1.; A.push a 7.; A.push a 10.; A.push a 12.;
  assert (1. +. 7. +. 10. +. 12. = A.fold_left (+.) 0. a);;

let () =
  let seq = Seq.(ints 0 |> take 10_000) in
  let a = A.of_seq seq in
  assert (Some 9999 = A.pop a);
  assert (Some 9998 = A.pop a);
  assert (Some 9997 = A.pop a);
  assert (9997 = A.length a);
  ();;

let () =
  let a = A.of_list [1;2] in
  assert (Some 2 = A.pop a);
  assert (Some 1 = A.pop a);
  assert (None = A.pop a);
  assert (None = A.pop a);
  ();;

let () =
  let a = A.of_list [1;2;3] in
  A.push a 4;
  assert (A.to_list a = [1;2;3;4]);;

let list_range start len : _ list =
  Seq.ints start |> Seq.take len |> List.of_seq
;;

let () =
  let a1 = A.init 5 (fun i->i)
  and a2 = A.init 5 (fun i->i+5) in
  A.append a1 a2;
  assert (A.to_list a1 = list_range 0 10);;

let () =
  let empty = A.create ()
  and a2 = A.init 5 (fun i->i) in
  A.append empty a2;
  assert (A.to_list empty = list_range 0 5);;

let () =
  let a1 = A.init 5 (fun i->i) and empty = A.create () in
  A.append a1 empty;
  assert (A.to_list a1 = list_range 0 5);;

let () =
  let a = A.init 3 (fun i->i) in
  A.append a a;
  assert (A.to_list a = [0; 1; 2; 0; 1; 2]);;

let() =
  let empty = A.create () in
  A.append empty empty;
  assert (A.to_list empty = []);;

let () =
  assert (A.of_list [1;2;3] |> A.copy |> A.to_list = [1;2;3]);;

let () =
  let a = A.create() in
  for i=0 to 20 do A.push a i; done;
  assert (A.to_list (A.copy a) = list_range 0 21);;

let () =
  assert (A.create() |> A.copy |> A.is_empty);;


let () =
  let a = A.create() in
  for i=0 to 20_000 do A.push a i; done;
  List.iter
    (fun size ->
      A.truncate a size;
      assert (A.to_list a = list_range 0 size))
    [ 19_999; 2000; 100; 50; 4; 4; 3; 2; 1; 0];;

let () =
  let a = A.create() in
  for i = 0 to 200 do
    A.push a i;
  done;
  A.shrink_capacity a;
  assert (A.length a = 201);;

let () =
  let a = A.of_list [1;2;3] in
  assert (A.to_list @@ A.map string_of_int a = ["1"; "2"; "3"]);;

let () =
  let a = A.of_list [1;2;3] in
  let a = A.mapi (fun i e -> Printf.sprintf "%i %i" i e) a in
  assert (A.to_list a = ["0 1"; "1 2"; "2 3"]);;

let () =
  let a = A.of_list [1;2;3;4;5] in
  assert (A.fold_left (+) 0 a = 15);;

let () =
  let l = list_range 0 300_000 in
  let a = A.of_list l in
  assert (A.to_list a = l);;

let () =
  let a = A.create() in
  A.ensure_capacity_with ~filler:42 a 200;
  for i=1 to 200 do
    A.unsafe_push a i
  done;
  assert (A.length a = 200);
  assert (A.to_list a = list_range 1 200);;

let () =
  let a = A.create() in
  A.push a 1;
  A.ensure_capacity_nonempty a 200;
  for i=2 to 200 do
    A.unsafe_push a i
  done;
  assert (A.length a = 200);
  assert (A.to_list a = list_range 1 200);;

let () = print_endline "OK";;
