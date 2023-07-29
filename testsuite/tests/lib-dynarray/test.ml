(* TEST *)

let list_range start len : _ list =
  Seq.ints start |> Seq.take len |> List.of_seq

module A = Dynarray

(** {1:dynarrays Dynamic arrays} *)

(** create, add_last *)

let () =
  let a = A.create() in
  A.add_last a 1;
  A.add_last a 2;
  assert (A.length a = 2);
  assert (A.to_list a = [1;2]);;


(** make *)

let () =
  let a = A.make 3 5 in
  A.add_last a 6;
  assert (A.to_list a = [5; 5; 5; 6]);;


(** init *)

let () =
  let test_init n f =
    assert (A.init n f |> A.to_array = Array.init n f) in
  for i = 0 to 1024 do
    test_init i Fun.id
  done;;


(** is_empty *)

let () =
  let a = A.create () in
  assert (A.is_empty a);
  A.ensure_capacity a 256;
  assert (A.is_empty a);;

(** length is tested below *)

(** get_last, find_last *)
let () =
  let a = A.of_list [1; 2] in
  assert (A.get_last a = 2);
  assert (A.find_last a = Some 2);

  A.remove_last a;
  assert (A.to_list a = [1]);
  assert (A.get_last a = 1);
  assert (A.find_last a = Some 1);

  A.remove_last a;
  assert (A.to_list a = []);
  assert (match A.get_last a with exception _ -> true | _ -> false);
  assert (A.find_last a = None)

(** copy, add_last *)

let () =
  assert (A.of_list [1;2;3] |> A.copy |> A.to_list = [1;2;3]);;

let () =
  let a = A.create() in
  for i=0 to 20 do A.add_last a i; done;
  assert (A.to_list (A.copy a) = list_range 0 21);;

let () =
  assert (A.create() |> A.copy |> A.is_empty);;

let () =
  let a = A.of_list [1; 2; 3] in
  let b = A.copy a in
  for i = 4 to 1024 do
    A.add_last b i
  done;
  assert (A.fold_left (+) 0 a = (1 + 2 + 3));
  assert (A.fold_left (+) 0 b = (1024 * 1025) / 2);;

let () =
  let a = A.of_list [1; 2; 3] in
  assert (A.fold_right List.cons a [] = [1; 2; 3]);;

(** {1:adding Adding elements} *)

(** add_last was tested above *)

(** append *)

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
  A.append a (A.copy a);
  (** Note: [A.append a a] is unspecified, and in particular it
     loops infinitely with the following natural implementation:
{[
     let append a b =
       append_iter a iter b

     let iter f a =
       let i = ref 0 in
       while !i < length a do
         f (get a !i);
         incr i
       done
]}
  *)
  assert (A.to_list a = [0; 1; 2; 0; 1; 2]);;

let() =
  let empty = A.create () in
  A.append empty empty;
  assert (A.to_list empty = []);;


(** dynarrays with floats *)

let () =
  let a = A.create() in
  A.add_last a 0.; A.add_last a 1.;
  assert (0. = A.get a 0);
  assert (1. = A.get a 1);
  assert (1. = A.fold_left (+.) 0. a);
  A.clear a;
  A.add_last a 0.; A.add_last a 1.; A.add_last a 7.; A.add_last a 10.; A.add_last a 12.;
  A.truncate a 2;
  assert (1. = A.fold_left (+.) 0. a);
  A.clear a;
  assert (0 = A.length a);
  A.add_last a 0.; A.add_last a 1.; A.add_last a 7.; A.add_last a 10.; A.add_last a 12.;
  A.set a 2 8.;
  assert (0. +. 1. +. 8. +. 10. +. 12. = A.fold_left (+.) 0. a);;


(** {1:removing Removing elements} *)


(** pop_last_opt, length *)

let () =
  let seq = Seq.(ints 0 |> take 10_000) in
  let a = A.of_seq seq in
  assert (Some 9999 = A.pop_last_opt a);
  assert (Some 9998 = A.pop_last_opt a);
  assert (Some 9997 = A.pop_last_opt a);
  assert (9997 = A.length a);
  ();;

let () =
  let a = A.of_list [1;2] in
  assert (Some 2 = A.pop_last_opt a);
  assert (Some 1 = A.pop_last_opt a);
  assert (None = A.pop_last_opt a);
  assert (None = A.pop_last_opt a);
  ();;


(** truncate *)

let () =
  let a = A.create() in
  let max_length = 20_000 in
  for i = 0 to max_length - 1 do A.add_last a i; done;
  List.iter
    (fun size ->
      A.truncate a size;
      let result_size = min max_length size in
      assert (A.to_list a = list_range 0 result_size))
    [ 30_000; 20_000; 19_999; 2000; 100; 50; 4; 4; 3; 2; 1; 0];;



(** {1:iteration Iteration} *)

(** map *)

let () =
  let a = A.of_list [1;2;3] in
  assert (A.to_list @@ A.map string_of_int a = ["1"; "2"; "3"]);;


(** mapi *)

let () =
  let a = A.of_list [1;2;3] in
  let a = A.mapi (fun i e -> Printf.sprintf "%i %i" i e) a in
  assert (A.to_list a = ["0 1"; "1 2"; "2 3"]);;


(** Iterator invalidation *)

let raises_invalid_argument f =
  match f () with
  | exception Invalid_argument _ -> true
  | exception _ | _ -> false

let () =
  let a = A.of_list [1; 2; 3] in
  assert (raises_invalid_argument (fun () ->
    A.append a a
  ))

let () =
  let a = A.of_list [1; 2; 3] in
  assert (raises_invalid_argument (fun () ->
    a |> A.iter (fun i ->
      A.add_last a (10 + i)
    )
  ))

let () =
  let a = A.of_list [1; 2; 3] in
  assert (raises_invalid_argument (fun () ->
    a |> A.iter (fun i ->
      if i >= 2 then A.remove_last a
    )
  ))

let does_not_raise_invalid_argument f =
  not (raises_invalid_argument f)

(* The spec says that this is a programming error, but currently we accept
   the following without an error. *)
let () =
  let a = A.of_list [1; 2; 3] in
  A.ensure_capacity a 10;
  assert (does_not_raise_invalid_argument (fun () ->
    a |> A.iter (fun i ->
      A.add_last a i;
      A.remove_last a
    )
  ))

(* Even with a capacity increase in the middle,
   we still accept this although the spec would let us reject. *)
let () =
  let a = A.of_list [1; 2; 3] in
  A.fit_capacity a;
  assert (does_not_raise_invalid_argument (fun () ->
    a |> A.iter (fun i ->
      A.add_last a i;
      A.remove_last a
    )
  ))


(** {1:conversions Conversions to other data structures} *)

(** {of,to}_{list,array,seq{,_rev}{,_rentrant}} *)

let () =
  for i = 0 to 1024 do
    let ints = List.init i Fun.id in
    assert ((ints |> A.of_list |> A.to_list) = ints);
    let arr = Array.of_list ints in
    assert ((arr |> A.of_array |> A.to_array) = arr);
    let seq = Array.to_seq arr in
    [A.to_seq; A.to_seq_reentrant] |> List.iter (fun dynarray_to_seq ->
      assert ((seq |> A.of_seq |> dynarray_to_seq) |> Array.of_seq = arr)
    );
    [A.to_seq_rev; A.to_seq_rev_reentrant] |> List.iter (fun dynarray_to_seq_rev ->
      assert ((seq |> A.of_seq |> dynarray_to_seq_rev)
                |> List.of_seq |> List.rev
              = ints)
    );
  done;;

(** reentrancy for to_seq{,_rev}_reentrant *)
let () =
  let a = A.of_list [1; 2; 3; 4] in
  let seq = A.to_seq a in
  let srq = A.to_seq_reentrant a in
  let elems_a = A.to_seq_reentrant a in

  let (i, seq) = Option.get (Seq.uncons seq) in assert (i = 1);
  let (i, srq) = Option.get (Seq.uncons srq) in assert (i = 1);

  (* setting an element in the middle is observed by both versions *)
  A.set a 1 12;
  assert (List.of_seq elems_a = [1; 12; 3; 4]);
  let (i, seq) = Option.get (Seq.uncons seq) in assert (i = 12);
  let (i, srq) = Option.get (Seq.uncons srq) in assert (i = 12);

  (* adding or removing elements invalidates [seq] but works with [srq] *)
  A.remove_last a;
  assert (List.of_seq elems_a = [1; 12; 3]);
  assert (match Seq.uncons seq with
    | exception (Invalid_argument _) -> true
    | _ -> false
  );
  let (i, srq) = Option.get (Seq.uncons srq) in assert (i = 3);

  A.add_last a 4;
  assert (List.of_seq elems_a = [1; 12; 3; 4]);
  let (i, srq) = Option.get (Seq.uncons srq) in assert (i = 4);
  assert (Seq.is_empty srq)

let () =
  let a = A.of_list [1; 2; 3; 4; 5] in
  let seq = A.to_seq_rev a in
  let srq = A.to_seq_rev_reentrant a in

  let (i, seq) = Option.get (Seq.uncons seq) in assert (i = 5);
  let (i, srq) = Option.get (Seq.uncons srq) in assert (i = 5);

  (* setting an element in the middle is observed by both versions *)
  A.set a 3 14;
  assert (A.to_list a = [1; 2; 3; 14; 5]);
  let (i, seq) = Option.get (Seq.uncons seq) in assert (i = 14);
  let (i, srq) = Option.get (Seq.uncons srq) in assert (i = 14);

  (* adding elements invalidates [seq] but is ignored by [srq] *)
  A.add_last a 6;
  assert (A.to_list a = [1; 2; 3; 14; 5; 6]);
  assert (match Seq.uncons seq with
    | exception (Invalid_argument _) -> true
    | _ -> false
  );
  (* just check the head, no popping *)
  let (i, _) = Option.get (Seq.uncons srq) in assert (i = 3);
  let (i, _) = Option.get (Seq.uncons srq) in assert (i = 3);

  (* [srq] skips removed elements *)
  A.truncate a 1;
  assert (A.to_list a = [1]);
  let (i, srq) = Option.get (Seq.uncons srq) in assert (i = 1);
  assert (Seq.is_empty srq)


(** {1:advanced Advanced topics for performance} *)

(** set_capacity *)

let () =
  let a = A.create() in
  let max_length = 20_000 in
  for i = 0 to max_length - 1 do A.add_last a i; done;
  List.iter
    (fun size ->
      A.set_capacity a size;
      let result_size = min max_length size in
      assert (A.to_list a = list_range 0 result_size))
    [ 30_000; 20_000; 19_999; 2000; 100; 50; 4; 4; 3; 2; 1; 0];;


(** fit_capacity, capacity *)

let () =
  let a = A.create() in
  for i = 0 to 200 do
    A.add_last a i;
  done;
  A.fit_capacity a;
  assert (A.length a = 201);
  assert (A.length a = A.capacity a);
