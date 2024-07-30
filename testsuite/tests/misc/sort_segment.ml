(* TEST *)

open Printf

let () =
  Random.init 42

(* [generate n] generates an array of length [n],
   containing random integer data. *)
let generate n =
  Array.init n @@ fun i -> Random.int n

(* Our comparison function. *)
let cmp =
  Int.compare

(* Comparing the content of two arrays. *)
let equal a1 a2 =
  assert (Array.length a1 = Array.length a2);
  Array.for_all2 (=) a1 a2

(* [test a ofs len] tests [Array.stable_sort_segment a ofs len].
   [Array.sort] is used as a reference sorting algorithm. *)
let test a ofs len =
  let segment = Array.sub a ofs len in
  Array.sort cmp segment;
  let expected =
    Array.concat [
      Array.sub a 0 ofs;
      segment;
      Array.sub a (ofs+len) (Array.length a - (ofs+len))
    ]
  in
  Array.stable_sort_segment cmp a ofs len;
  if not (equal a expected) then
    printf "Array.stable_sort_segment: FAILURE (array length %d, offset %d, segment length %d)\n%!"
      (Array.length a) ofs len

(* One set of tests, with random segments of a random array. *)

let number_of_tests = 1000
let max_length = 128
let () =
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
let () =
  for _ = 1 to number_of_tests do
    let a = generate length in
    for i = 0 to length do
      for j = i to length do
        test (Array.copy a) i (j - i)
      done
    done
  done

(* Done. *)

let () =
  printf "OK\n%!"
