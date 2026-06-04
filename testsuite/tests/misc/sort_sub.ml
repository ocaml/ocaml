(* TEST *)

open Printf

let () =
   Random.init 42

module type SigArray = sig
   type 'a t
   val init : int -> (int -> 'a) -> 'a t
   val length : 'a t -> int
   val get : 'a t -> int -> 'a
   val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
   val sort : ('a -> 'a -> int) -> 'a t -> unit
   val stable_sort_sub : ('a -> 'a -> int) -> 'a t -> int -> int -> unit
   val copy : 'a t -> 'a t
end

module Tests (A : SigArray) = struct

   (* [generate n] generates an array of length [n],
      containing random integer data. *)
   let generate n =
     A.init n @@ fun i -> Random.int n

   (* Our comparison function. *)
   let cmp =
      Int.compare

   (* Comparing the content of two arrays. *)
   let equal a1 a2 =
     assert (A.length a1 = A.length a2);
     A.for_all2 (=) a1 a2

   (* [test a ofs len] tests [A.stable_sort_sub a ofs len].
      [A.sort] is used as a reference sorting algorithm. *)
   let test a ofs len =
     let segment = A.init len (fun i -> A.get a (ofs + i)) in
     A.sort cmp segment;
     let expected = A.init (A.length a)
      (fun i -> if i < ofs || i >= (ofs + len) then A.get a i
                                              else A.get segment (i - ofs)) in
     A.stable_sort_sub cmp a ofs len;
     if not (equal a expected) then
       printf "A.stable_sort_sub: FAILURE (array length %d, offset %d,
           segment length %d)\n%!"
         (A.length a) ofs len

   (* One set of tests, with random segments of a random array. *)

   let number_of_tests = 1000
   let max_length = 128
   let test1 () =
     for _ = 1 to number_of_tests do
       let n = Random.int (max_length+1) in
       let a = generate n in
       let ofs = Random.int (n+1) in
       let len = Random.int (n+1-ofs) in
       test a ofs len
     done

   (* A second set of tests, enumerating all segments of a short array. *)

   let number_of_tests = 10
   let length = 10
   let test2 () =
     for _ = 1 to number_of_tests do
       let a = generate length in
       for i = 0 to length do
         for j = i to length do
           test (A.copy a) i (j - i)
         done
       done
     done

   (* Done. *)

   let run () =
     test1 (); test2 ();

end

module Small_Impl_Array = struct
   type 'a t = 'a Array.t
   let init = Array.init
   let length = Array.length
   let get = Array.get
   let for_all2 = Array.for_all2
   let sort = Array.sort
   let stable_sort_sub = Array.stable_sort_sub
   let copy = Array.copy
end

module Small_Impl_Dynarray = struct
   type 'a t = 'a Dynarray.t
   let init = Dynarray.init
   let length = Dynarray.length
   let get = Dynarray.get
   let for_all2 = Dynarray.for_all2
   let sort = Dynarray.sort
   let stable_sort_sub = Dynarray.stable_sort_sub
   let copy = Dynarray.copy
end

module Array_Tests = Tests (Small_Impl_Array)
module Dynarray_Tests = Tests (Small_Impl_Dynarray)

let () =
   Array_Tests.run () ;
   Dynarray_Tests.run ();
   print_endline "OK"
