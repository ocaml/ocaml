open Bigarray
open Printf

(* Test harness *)

let error_occurred = ref false

let function_tested = ref ""

let testing_function s =
    function_tested := s;
    print_newline();
    print_string s;
    print_newline()

let test test_number answer correct_answer =
 flush stdout;
 flush stderr;
 if answer <> correct_answer then begin
   eprintf "*** Bad result (%s, test %d)\n" !function_tested test_number;
   flush stderr;
   error_occurred := true
 end else begin
   printf " %d..." test_number
 end

(* One-dimensional arrays *)

let _ =
  testing_function "------ Array1 --------";
  testing_function "create/set/get";
  let test_setget kind vals =
    let rec set a i = function
        [] -> ()
      | (v1, v2) :: tl -> Array1.set a i v1; set a (i+1) tl in
    let rec test a i = function
        [] -> true
      | (v1, v2) :: tl -> Array1.get a i = v2 && test a (i+1) tl in
    let ca = Array1.create kind c_layout (List.length vals) in
    let fa = Array1.create kind fortran_layout (List.length vals) in
    set ca 0 vals;
    set fa 1 vals;
    test ca 0 vals && test fa 1 vals in
  test 1 true
    (test_setget int8_signed
                 [0, 0;
                  123, 123;
                  -123, -123;
                  456, -56;
                  0x101, 1]);
  test 2 true
    (test_setget int8_unsigned
                 [0, 0;
                  123, 123;
                  -123, 133;
                  456, 0xc8;
                  0x101, 1]);
  test 3 true
    (test_setget int16_signed
                 [0, 0;
                  123, 123;
                  -123, -123;
                  31456, 31456;
                  -31456, -31456;
                  65432, -104;
                  0x10001, 1]);
  test 4 true
    (test_setget int16_unsigned
                 [0, 0;
                  123, 123;
                  -123, 65413;
                  31456, 31456;
                  -31456, 34080;
                  65432, 65432;
                  0x10001, 1]);
  test 5 true
    (test_setget int
                 [0, 0;
                  123, 123;
                  -456, -456;
                  max_int, max_int;
                  min_int, min_int;
                  0x12345678, 0x12345678;
                  -0x12345678, -0x12345678]);
  test 6 true
    (test_setget int32
                 [Int32.zero, Int32.zero;
                  Int32.of_int 123, Int32.of_int 123;
                  Int32.of_int (-456), Int32.of_int (-456);
                  Int32.max, Int32.max;
                  Int32.min, Int32.min;
                  Int32.of_string "0x12345678", Int32.of_string "0x12345678"]);
  test 7 true
    (test_setget int64
                 [Int64.zero, Int64.zero;
                  Int64.of_int 123, Int64.of_int 123;
                  Int64.of_int (-456), Int64.of_int (-456);
                  Int64.max, Int64.max;
                  Int64.min, Int64.min;
                  Int64.of_string "0x123456789ABCDEF0",
                     Int64.of_string "0x123456789ABCDEF0"]);
  test 8 true
    (test_setget nativeint
                 [Nativeint.zero, Nativeint.zero;
                  Nativeint.of_int 123, Nativeint.of_int 123;
                  Nativeint.of_int (-456), Nativeint.of_int (-456);
                  Nativeint.max, Nativeint.max;
                  Nativeint.min, Nativeint.min;
                  Nativeint.of_string "0x12345678",
                    Nativeint.of_string "0x12345678"]);
  test 9 true
    (test_setget float4
                 [0.0, 0.0;
                  4.0, 4.0;
                  -0.5, -0.5;
                  655360.0, 655360.0]);
  test 10 true
    (test_setget float8
                 [0.0, 0.0;
                  4.0, 4.0;
                  -0.5, -0.5;
                  1.2345678, 1.2345678;
                  3.1415e10, 3.1415e10]);

  testing_function "comparisons";
  let from_list kind vals =
    let a = Array1.create kind c_layout (List.length vals) in
    let rec set i = function
        [] -> () 
      | hd :: tl -> Array1.set a i hd; set (i+1) tl in
    set 0 vals;
    a in
  let from_list_fortran kind vals =
    let a = Array1.create kind fortran_layout (List.length vals) in
    let rec set i = function
        [] -> () 
      | hd :: tl -> Array1.set a i hd; set (i+1) tl in
    set 1 vals;
    a in

  let normalize_comparison n =
    if n = 0 then 0 else if n < 0 then -1 else 1 in
  test 1 0 (normalize_comparison (compare
     (from_list int8_signed [1;2;3;-4;127;-128])
     (from_list int8_signed [1;2;3;-4;127;-128])));
  test 2 (-1) (normalize_comparison (compare
     (from_list int8_signed [1;2;3;-4;127;-128])
     (from_list int8_signed [1;2;3;4;127;-128])));
  test 3 1 (normalize_comparison (compare
     (from_list int8_signed [1;2;3;-4;127;-128])
     (from_list int8_signed [1;2;3;-4;42;-128])));
  test 4 (-1) (normalize_comparison (compare
     (from_list int8_signed [1;2;3;-4])
     (from_list int8_signed [1;2;3;4;127;-128])));
  test 5 1 (normalize_comparison (compare
     (from_list int8_signed [1;2;3;4;127;-128])
     (from_list int8_signed [1;2;3;-4])));

  test 6 0 (normalize_comparison (compare
     (from_list int8_unsigned [1;2;3;-4;127;-128])
     (from_list int8_unsigned [1;2;3;-4;127;-128])));
  test 7 1 (normalize_comparison (compare
     (from_list int8_unsigned [1;2;3;-4;127;-128])
     (from_list int8_unsigned [1;2;3;4;127;-128])));
  test 8 1 (normalize_comparison (compare
     (from_list int8_unsigned [1;2;3;-4;127;-128])
     (from_list int8_unsigned [1;2;3;-4;42;-128])));

  test 9 0 (normalize_comparison (compare
     (from_list int16_signed [1;2;3;-4;127;-128])
     (from_list int16_signed [1;2;3;-4;127;-128])));
  test 10 (-1) (normalize_comparison (compare
     (from_list int16_signed [1;2;3;-4;127;-128])
     (from_list int16_signed [1;2;3;4;127;-128])));
  test 11 1 (normalize_comparison (compare
     (from_list int16_signed [1;2;3;-4;127;-128])
     (from_list int16_signed [1;2;3;-4;42;-128])));

  test 12 0 (normalize_comparison (compare
     (from_list int16_unsigned [1;2;3;-4;127;-128])
     (from_list int16_unsigned [1;2;3;-4;127;-128])));
  test 13 (-1) (normalize_comparison (compare
     (from_list int16_unsigned [1;2;3;4;127;-128])
     (from_list int16_unsigned [1;2;3;0xFFFF;127;-128])));
  test 14 1 (normalize_comparison (compare
     (from_list int16_unsigned [1;2;3;-4;127;-128])
     (from_list int16_unsigned [1;2;3;-4;42;-128])));

  test 15 0 (normalize_comparison (compare
     (from_list int [1;2;3;-4;127;-128])
     (from_list int [1;2;3;-4;127;-128])));
  test 16 (-1) (normalize_comparison (compare
     (from_list int [1;2;3;-4;127;-128])
     (from_list int [1;2;3;4;127;-128])));
  test 17 1 (normalize_comparison (compare
     (from_list int [1;2;3;-4;127;-128])
     (from_list int [1;2;3;-4;42;-128])));

  test 18 0 (normalize_comparison (compare
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;127;-128]))
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;127;-128]))));
  test 19 (-1) (normalize_comparison (compare
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;127;-128]))
     (from_list int32 (List.map Int32.of_int [1;2;3;4;127;-128]))));
  test 20 1 (normalize_comparison (compare
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;127;-128]))
     (from_list int32 (List.map Int32.of_int [1;2;3;-4;42;-128]))));

  test 21 0 (normalize_comparison (compare
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;127;-128]))
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;127;-128]))));
  test 22 (-1) (normalize_comparison (compare
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;127;-128]))
     (from_list int64 (List.map Int64.of_int [1;2;3;4;127;-128]))));
  test 23 1 (normalize_comparison (compare
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;127;-128]))
     (from_list int64 (List.map Int64.of_int [1;2;3;-4;42;-128]))));

  test 24 0 (normalize_comparison (compare
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;127;-128]))
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;127;-128]))));
  test 25 (-1) (normalize_comparison (compare
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;127;-128]))
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;4;127;-128]))));
  test 26 1 (normalize_comparison (compare
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;127;-128]))
     (from_list nativeint (List.map Nativeint.of_int [1;2;3;-4;42;-128]))));

  test 27 0 (normalize_comparison (compare
     (from_list float4 [0.0; 0.25; -4.0; 3.141592654])
     (from_list float4 [0.0; 0.25; -4.0; 3.141592654])));
  test 28 (-1) (normalize_comparison (compare
     (from_list float4 [0.0; 0.25; -4.0])
     (from_list float4 [0.0; 0.25; 3.14159])));
  test 29 1 (normalize_comparison (compare
     (from_list float4 [0.0; 2.718; -4.0])
     (from_list float4 [0.0; 0.25; 3.14159])));

  test 30 0 (normalize_comparison (compare
     (from_list float8 [0.0; 0.25; -4.0; 3.141592654])
     (from_list float8 [0.0; 0.25; -4.0; 3.141592654])));
  test 31 (-1) (normalize_comparison (compare
     (from_list float8 [0.0; 0.25; -4.0])
     (from_list float8 [0.0; 0.25; 3.14159])));
  test 32 1 (normalize_comparison (compare
     (from_list float8 [0.0; 2.718; -4.0])
     (from_list float8 [0.0; 0.25; 3.14159])));

  testing_function "dim";
  test 1 (Array1.dim (from_list int [1;2;3;4;5])) 5;
  test 2 (Array1.dim (from_list_fortran int [1;2;3])) 3;

  testing_function "sub";
  let a = from_list int [1;2;3;4;5;6;7;8] in
  test 1 (Array1.sub a 2 5)
         (from_list int [3;4;5;6;7]);
  test 2 (Array1.sub a 0 2)
         (from_list int [1;2]);
  test 3 (Array1.sub a 0 8)
         (from_list int [1;2;3;4;5;6;7;8]);
  let a = from_list float8 [1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0] in
  test 4 (Array1.sub a 2 5)
         (from_list float8 [3.0;4.0;5.0;6.0;7.0]);
  test 5 (Array1.sub a 0 2)
         (from_list float8 [1.0;2.0]);
  test 6 (Array1.sub a 0 8)
         (from_list float8 [1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0]);
  let a = from_list_fortran float8 [1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0] in
  test 7 (Array1.sub a 2 5)
         (from_list_fortran float8 [2.0;3.0;4.0;5.0;6.0]);
  test 8 (Array1.sub a 1 2)
         (from_list_fortran float8 [1.0;2.0]);
  test 9 (Array1.sub a 1 8)
         (from_list_fortran float8 [1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0]);
  Gc.full_major();  (* test GC of proxies *)

  testing_function "blit, fill";
  let test_blit_fill kind data initval ofs len =
    let a = from_list kind data in
    let b = Array1.create kind c_layout (List.length data) in
    Array1.blit a b;
    (a = b) &&
    (Array1.fill (Array1.sub b ofs len) initval;
     let rec check i = function
         [] -> true
       | hd :: tl -> Array1.get b i = (if i >= ofs && i < ofs + len
                                       then initval else hd)
                     && check (i+1) tl
     in check 0 data) in
  test 1 true (test_blit_fill int8_signed [1;2;5;8;-100;127] 7 3 2);
  test 2 true (test_blit_fill int8_unsigned [1;2;5;8;-100;212] 7 3 2);
  test 3 true (test_blit_fill int16_signed [1;2;5;8;-100;212] 7 3 2);
  test 4 true (test_blit_fill int16_unsigned [1;2;5;8;-100;212] 7 3 2);
  test 5 true (test_blit_fill int [1;2;5;8;-100;212] 7 3 2);
  test 6 true (test_blit_fill int32 (List.map Int32.of_int [1;2;5;8;-100;212])
                                    (Int32.of_int 7) 3 2);
  test 7 true (test_blit_fill int64 (List.map Int64.of_int [1;2;5;8;-100;212])
                                    (Int64.of_int 7) 3 2);
  test 8 true (test_blit_fill nativeint
                             (List.map Nativeint.of_int [1;2;5;8;-100;212])
                             (Nativeint.of_int 7) 3 2);
  test 9 true (test_blit_fill float4 [1.0;2.0;0.5;0.125;256.0;512.0]
                             0.25 3 2);
  test 10 true (test_blit_fill float8 [1.0;2.0;5.0;8.123;-100.456;212e19]
                             3.1415 3 2);

(* Bi-dimensional arrays *)

  print_newline();
  testing_function "------ Array2 --------";
  testing_function "create/set/get";
  let make_array2 kind layout ind0 dim1 dim2 fromint =
    let a = Array2.create kind layout dim1 dim2 in
    for i = ind0 to dim1 - 1 + ind0 do
      for j = ind0 to dim2 - 1 + ind0 do
        Array2.set a i j (fromint (i * 1000 + j))
      done
    done;
    a in
  let check_array2 a ind0 dim1 dim2 fromint =
    try
      for i = ind0 to dim1 - 1 + ind0 do
        for j = ind0 to dim2 - 1 + ind0 do
          if Array2.get a i j <> (fromint (i * 1000 + j)) then raise Exit
        done
      done;
      true
    with Exit -> false in
  let id x = x in
  test 1 true
    (check_array2 (make_array2 int16_signed c_layout 0 10 20 id) 0 10 20 id);
  test 2 true
    (check_array2 (make_array2 int c_layout 0 10 20 id) 0 10 20 id);
  test 3 true
    (check_array2 (make_array2 int32 c_layout 0 10 20 Int32.of_int)
                  0 10 20 Int32.of_int);
  test 4 true
    (check_array2 (make_array2 float4 c_layout 0 10 20 float)
                  0 10 20 float);
  test 5 true
    (check_array2 (make_array2 float8 c_layout 0 10 20 float)
                  0 10 20 float);
  test 6 true
    (check_array2 (make_array2 int16_signed fortran_layout 1 10 20 id) 1 10 20 id);
  test 7 true
    (check_array2 (make_array2 int fortran_layout 1 10 20 id) 1 10 20 id);
  test 8 true
    (check_array2 (make_array2 int32 fortran_layout 1 10 20 Int32.of_int)
                  1 10 20 Int32.of_int);
  test 9 true
    (check_array2 (make_array2 float4 fortran_layout 1 10 20 float)
                  1 10 20 float);
  test 10 true
    (check_array2 (make_array2 float8 fortran_layout 1 10 20 float)
                  1 10 20 float);

  testing_function "dim";
  let a = (make_array2 int c_layout 0 4 6 id) in
  test 1 (Array2.dim1 a) 4;
  test 2 (Array2.dim2 a) 6;
  let b =  (make_array2 int fortran_layout 1 4 6 id) in
  test 3 (Array2.dim1 b) 4;
  test 4 (Array2.dim2 b) 6;

  testing_function "sub";
  let a = make_array2 int c_layout 0 5 3 id in
  let b = Array2.sub_left a 2 2 in
  test 1 true
         (Array2.get b 0 0 = 2000 &&
          Array2.get b 0 1 = 2001 &&
          Array2.get b 0 2 = 2002 &&
          Array2.get b 1 0 = 3000 &&
          Array2.get b 1 1 = 3001 &&
          Array2.get b 1 2 = 3002);
  let a = make_array2 int fortran_layout 1 5 3 id in
  let b = Array2.sub_right a 2 2 in
  test 2 true
         (Array2.get b 1 1 = 1002 &&
          Array2.get b 1 2 = 1003 &&
          Array2.get b 2 1 = 2002 &&
          Array2.get b 2 2 = 2003 &&
          Array2.get b 3 1 = 3002 &&
          Array2.get b 3 2 = 3003 &&
          Array2.get b 4 1 = 4002 &&
          Array2.get b 4 2 = 4003 &&
          Array2.get b 5 1 = 5002 &&
          Array2.get b 5 2 = 5003);

  testing_function "slice";
  let a = make_array2 int c_layout 0 5 3 id in
  test 1 (Array2.slice_left a 0) (from_list int [0;1;2]);
  test 2 (Array2.slice_left a 1) (from_list int [1000;1001;1002]);
  test 3 (Array2.slice_left a 2) (from_list int [2000;2001;2002]);
  test 4 (Array2.slice_left a 3) (from_list int [3000;3001;3002]);
  test 5 (Array2.slice_left a 4) (from_list int [4000;4001;4002]);
  let a = make_array2 int fortran_layout 1 5 3 id in
  test 6 (Array2.slice_right a 1) (from_list_fortran int [1001;2001;3001;4001;5001]);
  test 7 (Array2.slice_right a 2) (from_list_fortran int [1002;2002;3002;4002;5002]);
  test 8 (Array2.slice_right a 3) (from_list_fortran int [1003;2003;3003;4003;5003]);

(* Tri-dimensional arrays *)

  print_newline();
  testing_function "------ Array3 --------";
  testing_function "create/set/get";
  let make_array3 kind layout ind0 dim1 dim2 dim3 fromint =
    let a = Array3.create kind layout dim1 dim2 dim3 in
    for i = ind0 to dim1 - 1 + ind0 do
      for j = ind0 to dim2 - 1 + ind0 do
        for k = ind0 to dim3 - 1 + ind0 do
          Array3.set a i j k (fromint (i * 100 + j * 10 + k))
        done
      done
    done;
    a in
  let check_array3 a ind0 dim1 dim2 dim3 fromint =
    try
      for i = ind0 to dim1 - 1 + ind0 do
        for j = ind0 to dim2 - 1 + ind0 do
          for k = ind0 to dim3 - 1 + ind0 do
            if Array3.get a i j k <> (fromint (i * 100 + j * 10 + k))
            then raise Exit
          done
        done
      done;
      true
    with Exit -> false in
  let id x = x in
  test 1 true
    (check_array3 (make_array3 int16_signed c_layout 0 4 5 6 id) 0 4 5 6 id);
  test 2 true
    (check_array3 (make_array3 int c_layout 0 4 5 6 id) 0 4 5 6 id);
  test 3 true
    (check_array3 (make_array3 int32 c_layout 0 4 5 6 Int32.of_int)
                  0 4 5 6 Int32.of_int);
  test 4 true
    (check_array3 (make_array3 float4 c_layout 0 4 5 6 float)
                  0 4 5 6 float);
  test 5 true
    (check_array3 (make_array3 float8 c_layout 0 4 5 6 float)
                  0 4 5 6 float);
  test 6 true
    (check_array3 (make_array3 int16_signed fortran_layout 1 4 5 6 id) 1 4 5 6 id);
  test 7 true
    (check_array3 (make_array3 int fortran_layout 1 4 5 6 id) 1 4 5 6 id);
  test 8 true
    (check_array3 (make_array3 int32 fortran_layout 1 4 5 6 Int32.of_int)
                  1 4 5 6 Int32.of_int);
  test 9 true
    (check_array3 (make_array3 float4 fortran_layout 1 4 5 6 float)
                  1 4 5 6 float);
  test 10 true
    (check_array3 (make_array3 float8 fortran_layout 1 4 5 6 float)
                  1 4 5 6 float);

  testing_function "dim";
  let a = (make_array3 int c_layout 0 4 5 6 id) in
  test 1 (Array3.dim1 a) 4;
  test 2 (Array3.dim2 a) 5;
  test 3 (Array3.dim3 a) 6;
  let b =  (make_array3 int fortran_layout 1 4 5 6 id) in
  test 4 (Array3.dim1 b) 4;
  test 5 (Array3.dim2 b) 5;
  test 6 (Array3.dim3 b) 6;

  testing_function "slice1";
  let a = make_array3 int c_layout 0 3 3 3 id in
  test 1 (Array3.slice_left_1 a 0 0) (from_list int [0;1;2]);
  test 2 (Array3.slice_left_1 a 0 1) (from_list int [10;11;12]);
  test 3 (Array3.slice_left_1 a 0 2) (from_list int [20;21;22]);
  test 4 (Array3.slice_left_1 a 1 1) (from_list int [110;111;112]);
  test 5 (Array3.slice_left_1 a 2 1) (from_list int [210;211;212]);
  let a = make_array3 int fortran_layout 1 3 3 3 id in
  test 6 (Array3.slice_right_1 a 1 2) (from_list_fortran int [112;212;312]);
  test 7 (Array3.slice_right_1 a 3 1) (from_list_fortran int [131;231;331]);

  ()
                  
(********* End of test *********)

let _ =
  print_newline();
  if !error_occurred then begin
    prerr_endline "************* TEST FAILED ****************"; exit 2
  end else
    exit 0
