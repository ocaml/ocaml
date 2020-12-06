(* TEST
*)

open Printf

(* This is the module type of [Float.Array] except type [t] is abstract. *)
module type S = sig
  type t
  val length : t -> int
  val get : t -> int -> float
  val set : t -> int -> float -> unit
  val make : int -> float -> t
  val create : int -> t
  val init : int -> (int -> float) -> t
  val append : t -> t -> t
  val concat : t list -> t
  val sub : t -> int -> int -> t
  val copy : t -> t
  val fill : t -> int -> int -> float -> unit
  val blit : t -> int -> t -> int -> int -> unit
  val to_list : t -> float list
  val of_list : float list -> t
  val iter : (float -> unit) -> t -> unit
  val iteri : (int -> float -> unit) -> t -> unit
  val map : (float -> float) -> t -> t
  val mapi : (int -> float -> float) -> t -> t
  val fold_left : ('a -> float -> 'a) -> 'a -> t -> 'a
  val fold_right : (float -> 'a -> 'a) -> t -> 'a -> 'a
  val iter2 : (float -> float -> unit) -> t -> t -> unit
  val map2 : (float -> float -> float) -> t -> t -> t
  val for_all : (float -> bool) -> t -> bool
  val exists : (float -> bool) -> t -> bool
  val mem : float -> t -> bool
  val mem_ieee : float -> t -> bool
  val sort : (float -> float -> int) -> t -> unit
  val stable_sort : (float -> float -> int) -> t -> unit
  val fast_sort : (float -> float -> int) -> t -> unit
  val to_seq : t -> float Seq.t
  val to_seqi : t -> (int * float) Seq.t
  val of_seq : float Seq.t -> t
  val map_to_array : (float -> 'a) -> t -> 'a array
  val map_from_array : ('a -> float) -> 'a array -> t
  val unsafe_get : t -> int -> float
  val unsafe_set : t -> int -> float -> unit

  (* From Sys, rather than Float.Array *)
  val max_length : int
end

module Flat_float_array : S = struct
  include Stdlib.Float.Array
  let max_length = Sys.max_floatarray_length
end

(* module [Array] specialized to [float] and with a few changes,
   satisfies signature S *)
module Float_array : S = struct
  include Stdlib.Array
  let create = create_float
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let mem_ieee x a = exists ((=) x) a
  type t = float array
  let max_length = Sys.max_array_length
end

module Test (A : S) : sig end = struct

  (* auxiliary functions *)

  let neg_zero = 1.0 /. neg_infinity in

  let rec check_i_upto a i =
    if i >= 0 then begin
      assert (A.get a i = Float.of_int i);
      check_i_upto a (i - 1);
    end
  in

  let check_i a = check_i_upto a (A.length a - 1) in

  let check_inval f arg =
    match f arg with
    | _ -> assert false
    | exception (Invalid_argument _) -> ()
    | exception _ -> assert false
  in

  (* [make] [set] [get] *)
  let a = A.make 1000 1.0 in
  for i = 0 to 499 do A.set a i (Float.of_int i) done;
  let rec loop i =
    if i >= 0 then begin
      assert (A.get a i = (if i < 500 then Float.of_int i else 1.0));
      loop (i - 1);
    end
  in loop 999;
  check_inval (A.get a) (-1);
  check_inval (A.get a) (1000);
  check_inval (fun i -> A.set a i 1.0) (-1);
  check_inval (fun i -> A.set a i 1.0) 1000;
  check_inval A.create (-1);
  check_inval A.create (A.max_length + 1);
  check_inval (fun i -> A.make i 1.0) (-1);
  check_inval (fun i -> A.make i 1.0) (A.max_length + 1);

  (* [length] *)
  let test_length l = assert (l = (A.length (A.create l))) in
  test_length 0;
  test_length 10;
  test_length 25;
  test_length 255;
  test_length 256;
  test_length 1000;
  test_length 123456;

  (* [init] *)
  let a = A.init 1000 Float.of_int in
  check_i a;
  check_inval (fun i -> A.init i Float.of_int) (-1);
  check_inval (fun i -> A.init i Float.of_int) (A.max_length + 1);

  (* [append] *)
  let check m n =
    let a = A.init m Float.of_int in
    let b = A.init n (fun x -> Float.of_int (x + m)) in
    let c = A.append a b in
    assert (A.length c = (m + n));
    check_i c;
  in
  check 0 0;
  check 0 100;
  check 1 100;
  check 100 0;
  check 100 1;
  check 100 100;
  check 1000 1000;
  (* check_inval omitted *)

  (* [concat] *)
  let check l =
    let f (len, acc) n =
      (len + n, A.init n (fun i -> Float.of_int (len + i)) :: acc)
    in
    let (total, ll) = List.fold_left f (0, []) l in
    let b = A.concat (List.rev ll) in
    assert (A.length b = total);
    check_i b;
  in
  check [0; 0; 0];
  check [1; 10; 100];
  check [10; 0];
  check [0];
  check [1000; 1000; 1000];
  check [];
  (* check_inval omitted *)

  (* [sub] *)
  let a = A.init 1000 (fun i -> Float.of_int (i - 100)) in
  let b = A.sub a 100 200 in
  check_i b;
  assert (A.length b = 200);
  let b = A.sub a 1000 0 in
  check_i (A.sub a 1000 0);
  assert  (A.length b = 0);
  check_inval (A.sub a (-1)) 0;
  check_inval (A.sub a 0) (-1);
  check_inval (A.sub a 0) 1001;
  check_inval (A.sub a 1000) 1;

  (* [copy] *)
  let check len =
    let a = A.init len Float.of_int in
    let b = A.copy a in
    check_i b;
    assert (A.length b = len);
  in
  check 0;
  check 1;
  check 128;
  check 1023;

  (* [blit] [fill] *)
  let test_blit_fill data initval ofs len =
    let a = A.of_list data in
    let b = A.create (List.length data) in
    A.blit a 0 b 0 (A.length b);
    assert (a = b);
    A.fill b ofs len initval;
    let rec check i = function
      | [] -> ()
      | hd :: tl ->
          assert (A.get b i = (if i >= ofs && i < ofs + len
                               then initval else hd));
          check (i + 1) tl;
    in
    check 0 data
  in
  test_blit_fill [1.0;2.0;5.0;8.123;-100.456;212e19] 3.1415 3 2;
  let a = A.create 100 in
  check_inval (A.fill a (-1) 0) 1.0;
  check_inval (A.fill a 0 (-1)) 1.0;
  check_inval (A.fill a 0 101) 1.0;
  check_inval (A.fill a 100 1) 1.0;
  check_inval (A.fill a 101 0) 1.0;
  check_inval (A.blit a (-1) a 0) 0;
  check_inval (A.blit a 0 a 0) (-1);
  check_inval (A.blit a 0 a 0) 101;
  check_inval (A.blit a 100 a 0) 1;
  check_inval (A.blit a 101 a 0) 0;
  check_inval (A.blit a 0 a (-1)) 0;
  check_inval (A.blit a 0 a 100) 1;
  check_inval (A.blit a 0 a 101) 0;
  let test_blit_overlap a ofs1 ofs2 len =
    let a = A.of_list a in
    let b = A.copy a in
    A.blit a ofs1 a ofs2 len;
    for i = 0 to len - 1 do
      assert (A.get b (ofs1 + i) = A.get a (ofs2 + i))
    done
  in
  test_blit_overlap [1.; 2.; 3.; 4.] 1 2 2;

  (* [to_list] [of_list] *)
  let a = A.init 1000 Float.of_int in
  assert (compare a (A.of_list (A.to_list a)) = 0);
  let a = A.init 0 Float.of_int in
  assert (compare a (A.of_list (A.to_list a)) = 0);
  (* check_inval omitted *)

  (* [iter] *)
  let a = A.init 300 (Float.of_int) in
  let r = ref 0.0 in
  A.iter (fun x -> assert (x = !r); r := x +. 1.0) a;
  A.iter (fun _ -> assert false) (A.create 0);
  assert (!r = 300.0);

  (* [iteri] *)
  let a = A.init 300 Float.of_int in
  let r = ref 0 in
  let f i x =
    assert (i = !r);
    assert (x = Float.of_int i);
    r := i + 1
  in
  A.iteri f a;
  A.iteri (fun _ _ -> assert false) (A.create 0);
  assert (!r = 300);

  (* [map], test result and order of evaluation *)
  let a = A.init 500 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := !r +. 1.0;
    x -. 1.0
  in
  let b = A.map f a in
  check_i (A.sub b 1 499);

  (* [mapi], test result and order of evaluation *)
  let a = A.init 500 Float.of_int in
  let r = ref 0.0 in
  let f i x =
    assert (x = Float.of_int i);
    assert (x = !r);
    r := !r +. 1.0;
    x -. 1.0
  in
  let b = A.mapi f a in
  check_i (A.sub b 1 499);

  (* [fold_left], test result and order of evaluation *)
  let a = A.init 500 Float.of_int in
  let f acc x =
    assert (acc = x);
    x +. 1.0
  in
  let acc = A.fold_left f 0.0 a in
  assert (acc = 500.0);

  (* [fold_right], test result and order of evaluation *)
  let a = A.init 500 Float.of_int in
  let f x acc =
    assert (x = acc -. 1.0);
    x
  in
  let acc = A.fold_right f a 500.0 in
  assert (acc = 0.0);

  (* [iter2], test result and order of evaluation *)
  let a = A.init 123 Float.of_int in
  let b = A.init 123 Float.of_int in
  let r = ref 0.0 in
  let f x y =
    assert (x = !r);
    assert (y = !r);
    r := !r +. 1.0;
  in
  A.iter2 f a b;
  let c = A.create 456 in
  check_inval (A.iter2 (fun _ _ -> assert false) a) c;
  check_inval (A.iter2 (fun _ _ -> assert false) c) a;

  (* [map2], test result and order of evaluation *)
  let a = A.init 456 Float.of_int in
  let b = A.init 456 (fun i -> Float.of_int i /. 2.0) in
  let r = ref 0.0 in
  let f x y =
    assert (x = !r);
    assert (y = !r /. 2.0);
    r := !r +. 1.0;
    2.0 *. (x -. y)
  in
  let c = A.map2 f a b in
  check_i c;
  let d = A.create 455 in
  check_inval (A.map2 (fun _ _ -> assert false) a) d;
  check_inval (A.map2 (fun _ _ -> assert false) d) a;

  (* [for_all], test result and order of evaluation *)
  let a = A.init 777 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := x +. 1.0;
    true
  in
  assert (A.for_all f a);
  let f x = assert (x = 0.0); false in
  assert (not (A.for_all f a));

  (* [exists], test result and order of evaluation *)
  let a = A.init 777 Float.of_int in
  let r = ref 0.0 in
  let f x =
    assert (x = !r);
    r := x +. 1.0;
    false
  in
  assert (not (A.exists f a));
  let f x = assert (x = 0.0); true in
  assert (A.exists f a);

  (* [mem] *)
  let a = A.init 7777 Float.of_int in
  assert (A.mem 0.0 a);
  assert (A.mem 7776.0 a);
  assert (not (A.mem (-1.0) a));
  assert (not (A.mem 7777.0 a));
  let check v =
    A.set a 1000 v;
    assert (A.mem v a);
  in
  List.iter check [infinity; neg_infinity; neg_zero; nan];

  (* [mem_ieee] *)
  let a = A.init 7777 Float.of_int in
  assert (A.mem_ieee 0.0 a);
  assert (A.mem_ieee 7776.0 a);
  assert (not (A.mem_ieee (-1.0) a));
  assert (not (A.mem_ieee 7777.0 a));
  let check v =
    A.set a 1000 v;
    assert (A.mem_ieee v a);
  in
  List.iter check [infinity; neg_infinity; neg_zero];
  A.set a 0 nan;
  assert (not (A.mem_ieee nan a));

  (* [sort] [fast_sort] [stable_sort] *)
  let check_sort sort cmp a =
    let rec check_sorted a i =
      if i + 1 < A.length a then begin
        assert (cmp (A.get a i) (A.get a (i + 1)) <= 0);
        check_sorted a (i + 1);
      end
    in
    let rec check_permutation a b i =
      let p = Array.make (A.length a) true in
      let rec find lo hi x =
        assert (lo < hi);
        if hi = lo + 1 then begin
          assert (cmp (A.get a lo) x = 0);
          assert (p.(lo));
          p.(lo) <- false;
        end else begin
          let mid = (lo + hi) / 2 in
          assert (lo < mid && mid < hi);
          match cmp (A.get a (mid - 1)) x with
          | 0 when p.(mid - 1) -> find lo mid x
          | 0 -> find mid hi x
          | c when c < 0 -> find mid hi x
          | c when c > 0 -> find lo mid x
          | _ -> assert false
        end
      in
      A.iter (find 0 (A.length a)) b
    in
    let b = A.copy a in
    sort cmp a;
    check_sorted a 0;
    check_permutation a b 0;
  in
  Random.init 123;
  let rand_float _ =
    match Random.int 1004 with
    | 1000 -> nan
    | 1001 -> infinity
    | 1002 -> neg_infinity
    | 1003 -> neg_zero
    | n when n < 500 -> Random.float 1.0
    | _ -> -. Random.float 1.0
  in
  let check s =
    let a = A.init 5 Float.of_int in
    check_sort s Stdlib.compare a; (* already sorted *)
    check_sort s (fun x y -> Stdlib.compare y x) a; (* reverse-sorted *)

    let a = A.of_list [nan; neg_infinity; neg_zero; 0.; infinity] in
    check_sort s Stdlib.compare a; (* already sorted *)
    check_sort s (fun x y -> Stdlib.compare y x) a; (* reverse-sorted *)

    let a = A.init 50000 rand_float in
    check_sort s Stdlib.compare a;
    let a = A.make 1000 1.0 in
    check_sort s Stdlib.compare a;
    let a = A.append (A.make 1000 1.0) (A.make 1000 2.0) in
    check_sort s Stdlib.compare a;
  in
  check A.sort;
  check A.stable_sort;
  check A.fast_sort;

  (* [to_seq] *)
  let check_seq a =
    let r = ref 0 in
    let f x =
      assert (A.get a !r = x);
      r := !r + 1;
    in
    let s = A.to_seq a in
    Seq.iter f s;
  in
  check_seq (A.init 999 Float.of_int);
  check_seq (A.create 0);

  (* [to_seqi] *)
  let check_seqi a =
    let r = ref 0 in
    let f (i, x) =
      assert (i = !r);
      assert (A.get a !r = x);
      r := !r + 1;
    in
    let s = A.to_seqi a in
    Seq.iter f s;
  in
  check_seqi (A.init 999 Float.of_int);
  check_seqi (A.create 0);

  (* [of_seq] *)
  let r = ref 0 in
  let rec f () =
    if !r = 100 then Seq.Nil else begin
      let res = Seq.Cons (Float.of_int !r, f) in
      r := !r + 1;
      res
    end
  in
  let a = A.of_seq f in
  assert (a = A.init 100 Float.of_int);
  assert (A.of_seq Seq.empty = A.create 0);

  (* [map_to_array] *)
  let r = ref 0 in
  let f x =
    assert (x = Float.of_int !r);
    r := !r + 1;
    x *. 2.0
  in
  let a = A.init 876 Float.of_int in
  let ar1 = A.map_to_array f a in
  let ar2 = Array.init 876 (fun x -> Float.of_int (2 * x)) in
  assert (ar1 = ar2);
  let ar = A.map_to_array (fun _ -> assert false) (A.create 0) in
  assert (ar = [| |]);

  (* [map_from_array] *)
  let r = ref 0 in
  let f x =
    assert (x = Float.of_int !r);
    r := !r + 1;
    x *. 2.0
  in
  let ar = Array.init 876 Float.of_int in
  let a1 = A.map_from_array f ar in
  let a2 = A.init 876 (fun x -> Float.of_int (2 * x)) in
  assert (a1 = a2);
  let a = A.map_from_array (fun _ -> assert false) [| |] in
  assert (a = A.create 0);

  (* comparisons *)
  let normalize_comparison n =
    if n = 0 then 0 else if n < 0 then -1 else 1
  in
  let check c l1 l2 =
    assert (c = (normalize_comparison (compare (A.of_list l1) (A.of_list l2))))
  in
  check 0    [0.0; 0.25; -4.0; 3.141592654; nan]
             [0.0; 0.25; -4.0; 3.141592654; nan];
  check (-1) [0.0; 0.25; nan]
             [0.0; 0.25; 3.14];
  check (-1) [0.0; 0.25; -4.0]
             [0.0; 0.25; 3.14159];
  check 1    [0.0; 2.718; -4.0]
             [0.0; 0.25; 3.14159];
  check 1    [0.0; 2.718; -4.0]
             [nan; 0.25; 3.14159];

  (* [unsafe_get] [unsafe_set] *)
  let a = A.create 3 in
  for i = 0 to 2 do A.unsafe_set a i (float i) done;
  for i = 0 to 2 do assert (A.unsafe_get a i = float i) done;

  (* I/O *)
  let test_structured_io value =
    let (tmp, oc) =
      Filename.open_temp_file ~mode:[Open_binary] "floatarray" ".data"
    in
    Marshal.to_channel oc value [];
    close_out oc;
    let ic = open_in_bin tmp in
    let value' = Marshal.from_channel ic in
    close_in ic;
    Sys.remove tmp;
    assert (compare value value' = 0)
  in
  let l = [0.; 0.25; -4.; 3.14159265; nan; infinity; neg_infinity; neg_zero] in
  test_structured_io (A.of_list l);

end

(* We run the same tests on [Float.Array] and [Array]. *)
module T1 = Test (Flat_float_array)
module T2 = Test (Float_array)
