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


(** blit *)
let () =
  let () =
    (* normal blit works ok *)
    let a = A.of_list [1; 2; 3; 4; 5; 6] in
    let b = A.of_list [7; 8; 9; 10; 11] in
    A.blit ~src:b ~src_pos:1 ~dst:a ~dst_pos:2 ~len:3;
    assert (A.to_list a = [1; 2; 8; 9; 10; 6])
  in
  let () =
    (* source range overflows source array: error *)
    let a = A.of_list [1; 2] in
    let b = A.of_list [3; 4] in
    assert (match
              A.blit ~src:b ~src_pos:2 ~dst:a ~dst_pos:0 ~len:2
            with exception _ -> true | _ -> false)
  in
  let () =
    (* target range overflows target array: extend the array *)
    let a = A.of_list [1; 2] in
    let b = A.of_list [3; 4; 5] in
    A.blit ~src:b ~src_pos:0 ~dst:a ~dst_pos:1 ~len:3;
    assert (A.to_list a = [1; 3; 4; 5]);
    (* call [fit_capacity] to test the resize logic later on. *)
    A.fit_capacity a;
    (* this works even at the end *)
    A.blit ~src:b ~src_pos:0 ~dst:a ~dst_pos:4 ~len:2;
    assert (A.to_list a = [1; 3; 4; 5; 3; 4]);
    (* ... but it fails if the extension would leave a gap *)
    assert (A.length a = 6);
    assert (match
              A.blit ~src:b ~src_pos:0 ~dst:a ~dst_pos:7 ~len:2
            with exception _ -> true | _ -> false)
  in
  let () =
    (* self-blitting scenarios *)
    (* src_pos > dst_pos *)
    let a = A.of_list [1; 2; 3] in
    A.blit ~src:a ~src_pos:1 ~dst:a ~dst_pos:0 ~len:2;
    assert (A.to_list a = [2; 3; 3]);
    A.blit ~src:a ~src_pos:0 ~dst:a ~dst_pos:2 ~len:3;
    assert (A.to_list a = [2; 3; 2; 3; 3]);
    let b = A.of_list [1; 2; 3; 4] in
    (* src_pos = dst_pos *)
    A.blit ~src:b ~src_pos:1 ~dst:b ~dst_pos:1 ~len:2;
    assert (A.to_list b = [1; 2; 3; 4]);
    (* src_pos < dst_pos *)
    A.blit ~src:b ~src_pos:0 ~dst:b ~dst_pos:2 ~len:2;
    assert (A.to_list b = [1; 2; 1; 2]);
  in
  ()

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

(** mem *)
let () =
  let a = A.of_list [1;2;3;4;5] in
  assert (A.mem 1 a = true);
  assert (A.mem 7 a = false)

(** memq *)
let () =
  let five = 5 in
  let a = A.of_list [five; 6; 7] in
  assert (A.memq five a = true)

(** find_opt *)
let () =
  let a = A.of_list [1;4;9] in
  assert (A.find_opt (fun x -> x / 2 = 2) a = Some 4);
  assert (A.find_opt (fun x -> x = 5) a = None)

(** find_index *)
let () =
  let a = A.of_list [1;2;3] in
  assert (A.find_index (fun x -> x = 1) a = Some 0);
  assert (A.find_index (fun x -> x = 5) a = None)

(** find_map *)
let () =
  let a = A.of_list [1;2;3;4;5] in
  let b = A.of_list [1;2;3] in
  let go x = if x > 3 then Some x else None in
  assert (A.find_map go a = Some 4);
  assert (A.find_map go b = None)

(** find_mapi *)
let () =
  let a = A.of_list [1;1;3] in
  let b = A.of_list [3;2;1] in
  let go i x = if i = x then Some (i, x) else None in
  assert (A.find_mapi go a = Some (1,1));
  assert (A.find_mapi go b = None)

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


(** {1:comparison Comparison functions} *)

let () =
  let a = A.of_list [1; 2; 3] in
  A.ensure_capacity a 1000;
  let b = A.of_list [1; 2; 3] in
  assert (A.equal (=) a a);
  assert (A.compare Int.compare a a = 0);
  assert (A.equal (=) a b);
  assert (A.compare Int.compare a b = 0);
  ()

let () =
  let same eq l1 l2 = A.equal eq (A.of_list l1) (A.of_list l2) in
  assert (not (same (=) [1; 2; 3] [1; 3; 2]));
  assert (not (same (=) [1; 2; 3] [1; 2]));
  assert (not (same (=) [1] [1; 2]));
  assert (not (same (=) [] [1; 2]));
  assert (same (fun _ _ -> true) [1; 2] [3; 4]);
  assert (not (same (fun _ _ -> true) [1; 2] [3]));
  ()

let () =
  let compare cmp l1 l2 = A.compare cmp (A.of_list l1) (A.of_list l2) in
  assert (compare Int.compare [] [] = 0);
  assert (compare Int.compare [1; 2] [1; 2] = 0);
  assert (compare Int.compare [min_int] [max_int] < 0);
  assert (compare Int.compare [10] [0; 1] < 0);
  assert (compare Int.compare [10] [0] > 0);
  assert (compare (Fun.flip Int.compare) [10] [0] < 0);
  ()

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
  assert (A.length a = A.capacity a);;


(** check that comparisons and marshalling-with-sharing work as
    expected. *)

let () =
  (** Comparison.

      We expect physically-equal dynarrays to be found equal,
      and structurally-distinct dynarrays to be found distinct.
  *)
  let a = A.of_list [42] in
  let b = A.of_list [21] in
  assert (Stdlib.compare a a = 0);
  assert (Stdlib.compare a b <> 0);
  assert (a = a);
  assert (a <> b);

  (** On the other hand, we do not specify that comparison is fully
      structural, it may find structurally-equal values distinct, and
      in fact it does.

      This is not part of our specification, but we document the
      current behavior through tests below. *)
  let a' = A.create () in
  A.ensure_capacity a' 10000;
  A.append_list a' [42];
  assert (A.to_list a = A.to_list a');
  assert (a <> a');
  assert (Stdlib.compare a a' <> 0);
  ();;

let () =
  (** Marshalling. *)
  let a = A.of_list [42] in
  let buf = Marshal.to_string a [] in
  let c = Marshal.from_string buf 0 in
  (* Note: currently the equality of dynarrays is *not* stable by
     marshalling-unmarshalling. *)
  assert (Stdlib.compare a c <> 0);
  assert (a <> c);
  ();;
