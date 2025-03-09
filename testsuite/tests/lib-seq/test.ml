(* TEST *)

let (!?) = List.to_seq
let (!!) = List.of_seq
let cmp = compare

let head s = match s() with Seq.Cons(x,_) -> x | _ -> assert false

let poison : _ Seq.t =
  fun () ->
    failwith "Poisoned"

(* Standard test case *)
let () =
  assert
    ([2;4] =
      (List.to_seq [1;2;3;4;5]
      |> Seq.filter (fun x -> x mod 2 = 0)
     |> List.of_seq));
  ()
;;

(* filteri *)
let () =
  assert
    ([5;4] =
      (List.to_seq [5;4;3;2;1;]
      |> Seq.filteri (fun i _ -> i < 2)
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
    assert ([1;2;3] = !!(range 1 3));
    assert ([] = !!(range 1 0));
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
      = !!(Seq.concat !?[!?[1]; !?[]; !?[2; 3]])
    )

(* [cycle empty] is empty. *)
let () =
  let xs = Seq.(cycle empty) in
  assert (Seq.length xs = 0)

(* [cycle] of a singleton. *)
let () =
  let xs = Seq.(take 7 (cycle !?[1])) in
  assert (!!xs = [1;1;1;1;1;1;1])

(* [cycle] of a longer sequence. *)
let () =
  let xs = Seq.(take 7 (cycle !?[1;2;3])) in
  assert (!!xs = [1;2;3;1;2;3;1])

(* [iterate] *)
let () =
  let f x = x + 7 in
  let xs = Seq.(take 4 (iterate f 0)) in
  assert (!!xs = [0; 7; 14; 21])

(* [iterate] must not invoke [f] too early. (An easy trap to fall into.)
   The function [f] does not tolerate being invoked 4 times. Indeed, in
   this example, it should be called 3 times only. *)
let () =
  let c = ref 0 in
  let f x = incr c; assert (!c < 4); x + 7 in
  let xs = Seq.(take 4 (iterate f 0)) in
  assert (!!xs = [0; 7; 14; 21])

(* [init] *)
let () =
  let xs = Seq.(init 4 (fun i -> i+10)) in
  assert (!!xs = [10;11;12;13])

(* [fold_lefti] *)
let () =
  let xs = !?["a"; "b"] in
  assert (
    Seq.fold_lefti (fun acc i x -> (i, x) :: acc) [] xs = [ 1, "b"; 0, "a" ]
  )

(* [scan] *)
let () =
  let xs = Seq.(scan (+) 0 !?[1;2;3;4;5]) in
  assert (!!xs = [0; 1; 3; 6; 10; 15])

(* [scan] *)
let () =
  let xs = Seq.(scan (fun acc x -> x+1::acc) [] !?[1;2;3;4;5]) in
  assert (!!xs = [[]; [2]; [3;2]; [4;3;2]; [5;4;3;2]; [6;5;4;3;2]])

(* [is_empty] *)
let () =
  assert (Seq.is_empty Seq.empty);
  assert (not @@ Seq.is_empty (List.to_seq [1;2;3]))

(* [uncons] *)
let () =
  assert (match Seq.uncons (List.to_seq [1;2;3]) with
      | None -> false
      | Some (x,tl) -> x = 1 && List.of_seq tl = [2;3])

(* [repeat] *)
let () =
  let seq = Seq.repeat 1 in
  assert (Seq.length (Seq.take 1000 seq) = 1000);
  assert (head seq = 1);
  assert (head (Seq.drop 100_000 seq) = 1);
  ()

(* [forever] *)
let () =
  let r = ref 0 in
  let seq = Seq.forever (fun () ->
      let x = !r in incr r; x)
  in
  assert (List.of_seq (Seq.take 10 seq) = [0;1;2;3;4;5;6;7;8;9]);
  assert (head seq = 10);
  assert (Seq.length (Seq.take 1_000_000 seq) = 1_000_000);
  ()

(* [scan] must not invoke [f] too early. (An easy trap to fall into.)
   The function [f] does not tolerate being invoked 4 times. Indeed, in
   this example, it should be called 3 times only. *)
let () =
  let c = ref 0 in
  let f x y = incr c; assert (!c < 4); x + y in
  let xs = Seq.(take 4 (scan f 0 !?[1;2;3;4;5])) in
  assert (!!xs = [0; 1; 3; 6])

(* [take] *)
let () =
  let xs = Seq.take 0 poison in
  assert (!!xs = [])

(* [take_while] *)
let () =
  let xs = Seq.iterate succ 0 |> Seq.take_while (fun x->x<10) in
  assert (!!xs = [0;1;2;3;4;5;6;7;8;9])

(* [take_while] *)
let () =
  let xs = Seq.append (List.to_seq [1;2;3]) poison |> Seq.take_while (fun x -> x<3) in
  assert (!!xs = [1;2])

(* [drop] *)
let () =
  let xs = !?[1;2;3] in
  assert (Seq.drop 0 xs == xs);
  assert (!!(Seq.drop 1 xs) = [2;3]);
  assert (!!(Seq.drop 2 xs) = [3]);
  assert (!!(Seq.drop 3 xs) = []);
  assert (!!(Seq.drop 4 xs) = []);
  ()

(* [sorted_merge] *)
let () =
  let xs = !?[1;3;4;7]
  and ys = !?[2;2;5;7;16] in
  assert (!!(Seq.sorted_merge cmp xs ys) = [1;2;2;3;4;5;7;7;16])

(* [sorted_merge] should not consume its arguments too far. *)
let () =
  let (_ : int Seq.t) = Seq.sorted_merge cmp poison poison in
  assert true;
  let xs = Seq.(cons 1 (cons 3 poison))
  and ys = Seq.(cons 2 poison) in
  assert (!!(Seq.(take 2 (sorted_merge cmp xs ys))) = [1;2]);
  assert (!!(Seq.(take 2 (sorted_merge cmp ys xs))) = [1;2]);
  ()

(* [interleave] *)
let () =
  let xs = !?[1;2;3]
  and ys = !?[4;5] in
  assert (!!(Seq.interleave xs ys) = [1;4;2;5;3]);
  let xs = Seq.repeat 0 in
  assert (!!(Seq.(take 6 (interleave xs ys))) = [0;4;0;5;0;0]);
  let ys = Seq.repeat 1 in
  assert (!!(Seq.(take 6 (interleave xs ys))) = [0;1;0;1;0;1]);
  ()

(* [once] *)
let () =
  let xs = Seq.once (!?[1;2;3]) in
  let (n : int) = Seq.length xs in
  assert (n = 3);
  try
    let (_ : int) = Seq.length xs in
    print_endline "Oops"
  with Seq.Forced_twice ->
    ()

(* [memoize] *)
let () =
  let xs = Seq.(memoize (once (!?[1;2;3]))) in
  assert (Seq.length xs = 3);
  assert (Seq.fold_left (+) 0 xs = 6);
  ()

(* [of_dispenser] *)
let () =
  let c = ref 0 in
  let it () = let x = !c in c := x + 1; Some x in
  let xs = Seq.of_dispenser it in
  assert (!!(Seq.take 5 xs) = [0;1;2;3;4]);
  assert (!!(Seq.take 5 xs) = [5;6;7;8;9]);
  ()

(* [memoize] and [of_dispenser] *)
let () =
  let c = ref 0 in
  let it () = let x = !c in c := x + 1; Some x in
  let xs = Seq.(memoize (of_dispenser it)) in
  assert (!!(Seq.take 5 xs) = [0;1;2;3;4]);
  assert (!!(Seq.take 5 xs) = [0;1;2;3;4]);
  ()

(* [mapi] *)
let() =
  let seq = List.to_seq [0;1;2;3] |> Seq.mapi (fun i x -> i, x) in
  assert (Seq.length seq = 4);
  assert (Seq.for_all (fun (x,y) -> x=y) seq)

(* [product] *)
let () =
  (* test it works on infinite sequences *)
  let s = Seq.(product (repeat 1) (repeat true)) in
  assert ([1,true; 1,true; 1,true] = List.of_seq (Seq.take 3 s));
  (* basic functionality test *)
  let s = Seq.product (List.to_seq [1;2;3]) (List.to_seq [true;false]) in
  assert ([1,false; 1,true; 2,false; 2,true; 3,false; 3,true]
          = (List.of_seq s |> List.sort compare));
  ()

(* [find_index] *)
let () =
  assert (Seq.find_index (fun x -> x=1) !?[] = None);
  let xs = !?[1;2;3;4;5] in
  assert (Seq.find_index (fun x -> x=1) xs = Some 0);
  assert (Seq.find_index (fun x -> x=3) xs = Some 2);
  assert (Seq.find_index (fun x -> x=5) xs = Some 4);
  assert (Seq.find_index (fun x -> x=6) xs = None);
  ()

(* [find_mapi] *)
let () =
  assert (Seq.find_mapi
    (fun i x -> if x+i=3 then Some(i, x) else None) !?[] = None);
  let xs = !?[3;3;3;42;42] in
  assert (Seq.find_mapi
    (fun i x -> if x+i=3 then Some(i, x) else None) xs = Some (0, 3));
  assert (Seq.find_mapi
    (fun i x -> if x+i=4 then Some(i, x) else None) xs = Some (1, 3));
  assert (Seq.find_mapi
    (fun i x -> if x+i=5 then Some(i, x) else None) xs = Some (2, 3));
  assert (Seq.find_mapi
    (fun i x -> if x+i=7 then Some(i, x) else None) xs = None);
  ()

(* Auxiliary definitions of 2d matrices. *)
let square n f =
  Seq.(init n (fun i -> init n (fun j -> f i j)))

let rec infinite i () =
  Seq.(Cons (
    map (fun j -> (i, j)) (ints 0),
    infinite (i+1)
  ))

(* [transpose] of a finite square matrix. *)
let () =
  let matrix = square 3 (fun i j -> (i, j)) in
  (* Check the first line of our square matrix. *)
  assert (!!(head matrix) = [(0, 0); (0, 1); (0, 2)]);
  (* Check the first column of our square matrix. *)
  assert (!!(Seq.map head matrix) = [(0, 0); (1, 0); (2, 0)]);
  (* Transpose the matrix. *)
  let matrix = Seq.transpose matrix in
  (* Check the first line of the transposed matrix. *)
  assert (!!(head matrix) = [(0, 0); (1, 0); (2, 0)]);
  (* Check the first column of the transposed matrix. *)
  assert (!!(Seq.map head matrix) = [(0, 0); (0, 1); (0, 2)]);
  ()

(* [transpose] of a doubly-infinite matrix. *)
let () =
  let matrix = infinite 0 in
  (* Check the first line. *)
  assert (!!(Seq.(take 3 (head matrix))) = [(0, 0); (0, 1); (0, 2)]);
  (* Check the first column. *)
  assert (!!(Seq.(take 3 (map head matrix))) = [(0, 0); (1, 0); (2, 0)]);
  (* Transpose the matrix. *)
  let matrix = Seq.transpose matrix in
  (* Check the first line of the transposed matrix. *)
  assert (!!(Seq.(take 3 (head matrix))) = [(0, 0); (1, 0); (2, 0)]);
  (* Check the first column of the transposed matrix. *)
  assert (!!(Seq.(take 3 (map head matrix))) = [(0, 0); (0, 1); (0, 2)]);
  ()

let () = print_endline "OK";;
