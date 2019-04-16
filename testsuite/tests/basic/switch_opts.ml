(* TEST *)

(* Test for optimisation of jump tables to arrays of constants *)

let p = Printf.printf

type test =
  Test : 'b * 'a * ('b -> 'a) -> test

type t = A | B | C

(* These test functions need to have at least three cases.
   Functions with fewer cases don't trigger the optimisation,
   as they are compiled to if-then-else, not switch *)

let passes = ref 0

let full_test line ~f ~results () =
  let f = Sys.opaque_identity f in
  List.iter
    (fun (input, output) ->
       let result = f input in
       if result <> output
       then raise (Assert_failure (__FILE__,line,0))
    )
    results;
  incr passes

let test_int_match =
  full_test __LINE__
    ~f:(function
      | 1 -> 1
      | 2 -> 2
      | 3 -> 3
      | _ -> 0
    )
    ~results:
      [ 1,1; 2,2; 3,3; 4,0; 0,0 ]

let test_int_match_reverse =
  full_test __LINE__
    ~f:(function
      | 1 -> 3
      | 2 -> 2
      | 3 -> 1
      | _ -> 0
    )
    ~results:
      [ 1,3; 2,2; 3,1; 4,0; 0,0 ]

let test_int_match_negative =
  full_test __LINE__
    ~f:(function
      | 1 -> -1
      | 2 -> -2
      | 3 -> -3
      | _ -> 0
    )
    ~results:
      [ 1,-1; 2,-2; 3,-3; 4,0; 0,0 ]

let test_int_match_negative_reverse =
  full_test __LINE__
    ~f:(function
      | 1 -> -3
      | 2 -> -2
      | 3 -> -1
      | _ -> 0
    )
    ~results:
      [ 1,-3; 2,-2; 3,-1; 4,0; 0,0 ]

let test_int_min_int =
  full_test __LINE__
    ~f:(function
      | 1 -> 1
      | 2 -> 2
      | 3 -> min_int
      | _ -> 0
    )
    ~results:
      [ 1,1; 2,2; 3,min_int; 4,0; 0,0 ]

let test_int_max_int =
  full_test __LINE__
    ~f:(function
      | 1 -> 1
      | 2 -> 2
      | 3 -> max_int
      | _ -> 0
    )
    ~results:
      [ 1,1; 2,2; 3,max_int; 4,0; 0,0 ]

let test_float =
  full_test __LINE__
    ~f:(function
      | 1 -> 1.0
      | 2 -> 2.0
      | 3 -> 3.0
      | _ -> 0.0
    )
    ~results:
      [ 1,1.0; 2,2.0; 3,3.0; 4,0.0; 0,0.0 ]

let test_string =
  full_test __LINE__
    ~f:(function
      | 1 -> "a"
      | 2 -> "b"
      | 3 -> "cc"
      | _ -> ""
    )
    ~results:
      [ 1,"a"; 2, "b"
      ; 3, Sys.opaque_identity "c" ^ Sys.opaque_identity "c"; 4, ""; 0, "" ]

let test_list =
  full_test __LINE__
    ~f:(function
      | 1 -> []
      | 2 -> [ 42 ]
      | 3 -> [ 1; 2; 3 ]
      | _ -> [ 415 ]
    )
    ~results:
      [ 1, []; 2, [ 42 ]; 3, List.rev [3;2;1]; 4, [ 415 ]; 0, [ 415 ] ]

let test_abc =
  full_test __LINE__
    ~f:(function
      | A -> 1
      | B -> 2
      | C -> 3
    )
    ~results:
      [ A, 1; B, 2; C, 3]

let test_abc_unsorted =
  full_test __LINE__
    ~f:(function
      | C -> 3
      | A -> 1
      | B -> 2
    )
    ~results:
      [ A, 1; B, 2; C, 3]

let test_abc_neg3 =
  full_test __LINE__
    ~f:(function
      | A -> 1
      | B -> 2
      | C -> -3
    )
    ~results:
      [ A, 1; B, 2; C, -3]

let test_abc_min_int =
  full_test __LINE__
    ~f:(function
      | A -> 1
      | B -> 2
      | C -> min_int
    )
    ~results:
      [ A, 1; B, 2; C, min_int ]

let test_abc_max_int =
  full_test __LINE__
    ~f:(function
      | A -> 1
      | B -> 2
      | C -> max_int
    )
    ~results:
      [ A, 1; B, 2; C, max_int ]

let test_abc_float =
  full_test __LINE__
    ~f:(function
      | A -> 1.
      | B -> 2.
      | C -> 3.
    )
    ~results:
      [ A, 1.; B, 2.; C, 3. ]

let test_abc_string =
  full_test __LINE__
    ~f:(function
      | A -> "a"
      | B -> "b"
      | C -> "c"
    )
    ~results:
      [ A, "a"; B, "b"; C, "c" ]

let test_abc_list =
  full_test __LINE__
    ~f:(function
      | A -> []
      | B -> [42]
      | C -> [1;2;3]
    )
    ~results:
      [ A, []; B, [42]; C, List.rev [3;2;1] ]

let test_f99 =
  full_test __LINE__
    ~f:(function
      | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4 | 5 -> 5 | 6 -> 6 | 7 -> 7 | 8 -> 8
      | 9 -> 9 | 10 -> 10 | 11 -> 11 | 12 -> 12 | 13 -> 13 | 14 -> 14 | 15 -> 15
      | 16 -> 16 | 17 -> 17 | 18 -> 18 | 19 -> 19 | 20 -> 20 | 21 -> 21 | 22 -> 22
      | 23 -> 23 | 24 -> 24 | 25 -> 25 | 26 -> 26 | 27 -> 27 | 28 -> 28 | 29 -> 29
      | 30 -> 30 | 31 -> 31 | 32 -> 32 | 33 -> 33 | 34 -> 34 | 35 -> 35 | 36 -> 36
      | 37 -> 37 | 38 -> 38 | 39 -> 39 | 40 -> 40 | 41 -> 41 | 42 -> 42 | 43 -> 43
      | 44 -> 44 | 45 -> 45 | 46 -> 46 | 47 -> 47 | 48 -> 48 | 49 -> 49 | 50 -> 50
      | 51 -> 51 | 52 -> 52 | 53 -> 53 | 54 -> 54 | 55 -> 55 | 56 -> 56 | 57 -> 57
      | 58 -> 58 | 59 -> 59 | 60 -> 60 | 61 -> 61 | 62 -> 62 | 63 -> 63 | 64 -> 64
      | 65 -> 65 | 66 -> 66 | 67 -> 67 | 68 -> 68 | 69 -> 69 | 70 -> 70 | 71 -> 71
      | 72 -> 72 | 73 -> 73 | 74 -> 74 | 75 -> 75 | 76 -> 76 | 77 -> 77 | 78 -> 78
      | 79 -> 79 | 80 -> 80 | 81 -> 81 | 82 -> 82 | 83 -> 83 | 84 -> 84 | 85 -> 85
      | 86 -> 86 | 87 -> 87 | 88 -> 88 | 89 -> 89 | 90 -> 90 | 91 -> 91 | 92 -> 92
      | 93 -> 93 | 94 -> 94 | 95 -> 95 | 96 -> 96 | 97 -> 97 | 98 -> 98 | 99 -> 99
      | _ -> 0
    )
    ~results:
      [ 1,1; 42,42; 98, 98; 99,99; 100, 0 ]

let test_poly =
  full_test __LINE__
    ~f:(function
      | 1 -> `Primary
      | 2 -> `Secondary
      | 3 -> `Tertiary
      | n -> invalid_arg "test"
    )
    ~results:
      [ 1, `Primary; 2, `Secondary; 3, `Tertiary ]

let test_or =
  full_test __LINE__
    ~f:(function
      | 1 | 2 | 3 -> 0
      | 4 | 5 | 6 -> 1
      | 7 -> 2
      | _ -> 0
    )
    ~results:
      [ 1,0; 2,0; 3,0; 4,1; 5,1; 6,1; 7,2; 8,0; 0,0 ]

type t' = E | F | G | H

let test_or_efgh =
  full_test __LINE__
    ~f:(function
      | E | H -> 0
      | F -> 1
      | G -> 2
    )
    ~results:
      [ E,0; H,0; F,1; G,2 ]

type 'a gadt =
  | Ag : int gadt
  | Bg : string gadt
  | Cg : int gadt
  | Dg : int gadt
  | Eg : int gadt

let test_gadt =
  full_test __LINE__
    ~f:(function
      | Ag -> 1
      | Cg -> 2
      | Dg -> 3
      | Eg -> 4
    )
    ~results:
      [ Ag,1; Cg,2; Dg,3; Eg,4 ]

let () =
  test_int_match ();
  test_int_match_reverse ();
  test_int_match_negative ();
  test_int_match_negative_reverse ();
  test_int_min_int ();
  test_int_max_int ();
  test_float ();
  test_string ();
  test_list ();
  test_abc ();
  test_abc_unsorted ();
  test_abc_neg3 ();
  test_abc_min_int ();
  test_abc_max_int ();
  test_abc_float ();
  test_abc_string ();
  test_abc_list ();
  test_f99 ();
  test_poly ();
  test_or ();
  test_or_efgh ();
  test_gadt ();
  ()

let () =
  Printf.printf "%d tests passed\n" !passes
