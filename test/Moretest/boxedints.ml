(* Test the types nativeint, int32, int64 *)

open Printf

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

(***** Tests on 32 bit arithmetic *****)

module type TESTSIG = sig
  type t
  module Ops : sig
    val neg: t -> t 
    val add: t -> t -> t 
    val sub: t -> t -> t 
    val mul: t -> t -> t 
    val div: t -> t -> t 
    val rem: t -> t -> t 
    val logand: t -> t -> t 
    val logor: t -> t -> t 
    val logxor: t -> t -> t 
    val shift_left: t -> int -> t 
    val shift_right: t -> int -> t 
    val shift_right_logical: t -> int -> t 
    val of_int: int -> t 
    val to_int: t -> int 
    val zero: t
    val one: t
    val minus_one: t
    val min: t
    val max: t
    val format : string -> t -> string 
    val to_string: t -> string
    val of_string: string -> t 
  end
  val testcomp: t -> t -> bool*bool*bool*bool*bool*bool
end

module Test32(M: TESTSIG) =
struct
  open M
  open Ops

  let _ =
    testing_function "of_int, to_int";
    test 1 (to_int (of_int 0)) 0;
    test 2 (to_int (of_int 123)) 123;
    test 3 (to_int (of_int (-456))) (-456);
    test 4 (to_int (of_int 0x3FFFFFFF)) 0x3FFFFFFF;
    test 5 (to_int (of_int (-0x40000000))) (-0x40000000);

    testing_function "of_string";
    test 1 (of_string "0") (of_int 0);
    test 2 (of_string "123") (of_int 123);
    test 3 (of_string "-456") (of_int (-456));
    test 4 (of_string "123456789") (of_int 123456789);
    test 5 (of_string "0xABCDEF") (of_int 0xABCDEF);
    test 6 (of_string "-0o1234567012") (of_int (- 0o1234567012));
    test 7 (of_string "0b01010111111000001100")
           (of_int 0b01010111111000001100);
    test 8 (of_string "0x7FFFFFFF") max;
    test 9 (of_string "-0x80000000") min;
    test 10 (of_string "0x80000000") min;
    test 11 (of_string "0xFFFFFFFF") minus_one;

    testing_function "to_string, format";
    List.iter (fun (n, s) -> test n (to_string (of_string s)) s)
      [1, "0"; 2, "123"; 3, "-456"; 4, "1234567890";
       5, "2147483647"; 6, "-2147483648"];
    List.iter (fun (n, s) -> test n (format "0x%X" (of_string s)) s)
      [7, "0x0"; 8, "0x123"; 9, "0xABCDEF"; 10, "0x12345678";
       11, "0x7FFFFFFF"; 12, "0x80000000"; 13, "0xFFFFFFFF"];
    test 14 (to_string max) "2147483647";
    test 15 (to_string min) "-2147483648";
    test 16 (to_string zero) "0";
    test 17 (to_string one) "1";
    test 18 (to_string minus_one) "-1";

    testing_function "neg";
    test 1 (neg (of_int 0)) (of_int 0);
    test 2 (neg (of_int 123)) (of_int (-123));
    test 3 (neg (of_int (-456))) (of_int 456);
    test 4 (neg (of_int 123456789)) (of_int (-123456789));
    test 5 (neg max) (of_string "-0x7FFFFFFF");
    test 6 (neg min) min;

    testing_function "add";
    test 1 (add (of_int 0) (of_int 0)) (of_int 0);
    test 2 (add (of_int 123) (of_int 0)) (of_int 123);
    test 3 (add (of_int 0) (of_int 456)) (of_int 456);
    test 4 (add (of_int 123) (of_int 456)) (of_int 579);
    test 5 (add (of_int (-123)) (of_int 456)) (of_int 333);
    test 6 (add (of_int 123) (of_int (-456))) (of_int (-333));
    test 7 (add (of_int (-123)) (of_int (-456))) (of_int (-579));
    test 8 (add (of_string "0x12345678") (of_string "0x9ABCDEF"))
           (of_string "0x1be02467");
    test 9 (add max max) (of_int (-2));
    test 10 (add min min) zero;
    test 11 (add max one) min;
    test 12 (add min minus_one) max;
    test 13 (add max min) minus_one;

    testing_function "sub";
    test 1 (sub (of_int 0) (of_int 0)) (of_int 0);
    test 2 (sub (of_int 123) (of_int 0)) (of_int 123);
    test 3 (sub (of_int 0) (of_int 456)) (of_int (-456));
    test 4 (sub (of_int 123) (of_int 456)) (of_int (-333));
    test 5 (sub (of_int (-123)) (of_int 456)) (of_int (-579));
    test 6 (sub (of_int 123) (of_int (-456))) (of_int 579);
    test 7 (sub (of_int (-123)) (of_int (-456))) (of_int 333);
    test 8 (sub (of_string "0x12345678") (of_string "0x9ABCDEF"))
           (of_string "0x8888889");
    test 9 (sub max min) minus_one;
    test 10 (sub min max) one;
    test 11 (sub min one) max;
    test 12 (sub max minus_one) min;

    testing_function "mul";
    test 1 (mul (of_int 0) (of_int 0)) (of_int 0);
    test 1 (mul (of_int 123) (of_int 0)) (of_int 0);
    test 1 (mul (of_int 0) (of_int (-456))) (of_int 0);
    test 2 (mul (of_int 123) (of_int 1)) (of_int 123);
    test 3 (mul (of_int 1) (of_int (-456))) (of_int (-456));
    test 2 (mul (of_int 123) (of_int (-1))) (of_int (-123));
    test 3 (mul (of_int (-1)) (of_int (-456))) (of_int 456);
    test 4 (mul (of_int 123) (of_int 456)) (of_int 56088);
    test 5 (mul (of_int (-123)) (of_int 456)) (of_int (-56088));
    test 6 (mul (of_int 123) (of_int (-456))) (of_int (-56088));
    test 7 (mul (of_int (-123)) (of_int (-456))) (of_int 56088);
    test 8 (mul (of_string "0x12345678") (of_string "0x9ABCDEF"))
           (of_string "0xe242d208");
    test 9 (mul max max) one;

    testing_function "div";
    List.iter
      (fun (n, a, b) -> test n (div (of_int a) (of_int b)) (of_int (a / b)))
      [1, 0, 2;
       2, 123, 1;
       3, -123, 1;
       4, 123, -1;
       5, -123, -1;
       6, 1275312364, 365;
       7, 16384, 256;
       8, -1275312364, 365;
       9, 1275312364, -365;
       10, 1234567, 12345678;
       11, 1234567, -12345678];

    testing_function "mod";
    List.iter
      (fun (n, a, b) -> test n (rem (of_int a) (of_int b)) (of_int (a mod b)))
      [1, 0, 2;
       2, 123, 1;
       3, -123, 1;
       4, 123, -1;
       5, -123, -1;
       6, 1275312364, 365;
       7, 16384, 256;
       8, -1275312364, 365;
       9, 1275312364, -365;
       10, 1234567, 12345678;
       11, 1234567, -12345678];

    testing_function "and";
    List.iter
      (fun (n, a, b, c) -> test n (logand (of_string a) (of_string b))
                                  (of_string c))
      [1, "0x12345678", "0x9abcdef0", "0x12345670";
       2, "0x12345678", "0x0fedcba9", "0x2244228";
       3, "0xFFFFFFFF", "0x12345678", "0x12345678";
       4, "0", "0x12345678", "0";
       5, "0x55555555", "0xAAAAAAAA", "0"];

    testing_function "or";
    List.iter
      (fun (n, a, b, c) -> test n (logor (of_string a) (of_string b))
                                  (of_string c))
      [1, "0x12345678", "0x9abcdef0", "0x9abcdef8";
       2, "0x12345678", "0x0fedcba9", "0x1ffddff9";
       3, "0xFFFFFFFF", "0x12345678", "0xFFFFFFFF";
       4, "0", "0x12345678", "0x12345678";
       5, "0x55555555", "0xAAAAAAAA", "0xFFFFFFFF"];

    testing_function "xor";
    List.iter
      (fun (n, a, b, c) -> test n (logxor (of_string a) (of_string b))
                                  (of_string c))
      [1, "0x12345678", "0x9abcdef0", "0x88888888";
       2, "0x12345678", "0x0fedcba9", "0x1dd99dd1";
       3, "0xFFFFFFFF", "0x12345678", "0xedcba987";
       4, "0", "0x12345678", "0x12345678";
       5, "0x55555555", "0xAAAAAAAA", "0xFFFFFFFF"];

    testing_function "shift_left";
    List.iter
      (fun (n, a, b, c) -> test n (shift_left (of_string a) b) (of_string c))
      [1, "1", 1, "2";
       2, "1", 2, "4";
       3, "1", 4, "0x10";
       4, "1", 30, "0x40000000";
       5, "1", 31, "0x80000000";
       6, "0x16236", 7, "0xb11b00";
       7, "0x10", 27, "0x80000000";
       8, "0x10", 28, "0"];

    testing_function "shift_right";
    List.iter
      (fun (n, a, b, c) -> test n (shift_right (of_string a) b) (of_string c))
      [1, "2", 1, "1";
       2, "4", 2, "1";
       3, "0x10", 4, "1";
       4, "0x40000000", 10, "0x100000";
       5, "0x80000000", 31, "-1";
       6, "0xb11b00", 7, "0x16236";
       7, "-0xb11b00", 7, "-90678"];

    testing_function "shift_right_logical";
    List.iter
      (fun (n, a, b, c) -> test n (shift_right_logical (of_string a) b)
                                  (of_string c))
      [1, "2", 1, "1";
       2, "4", 2, "1";
       3, "0x10", 4, "1";
       4, "0x40000000", 10, "0x100000";
       5, "0x80000000", 31, "1";
       6, "0xb11b00", 7, "0x16236";
       7, "-0xb11b00", 7, "0x1fe9dca"];

    testing_function "Comparisons";
    test 1 (testcomp (of_int 0) (of_int 0))
           (true,false,false,false,true,true);
    test 2 (testcomp (of_int 1234567) (of_int 1234567))
           (true,false,false,false,true,true);
    test 3 (testcomp (of_int 0) (of_int 1))
           (false,true,true,false,true,false);
    test 4 (testcomp (of_int (-1)) (of_int 0))
           (false,true,true,false,true,false);
    test 5 (testcomp (of_int 1) (of_int 0))
           (false,true,false,true,false,true);
    test 6 (testcomp (of_int 0) (of_int (-1)))
           (false,true,false,true,false,true);
    test 7 (testcomp max min)
           (false,true,false,true,false,true);

    ()
end

(********* Tests on 64-bit arithmetic ***********)

module Test64(M: TESTSIG) =
struct
  open M
  open Ops

  let _ =
    testing_function "of_int, to_int";
    test 1 (to_int (of_int 0)) 0;
    test 2 (to_int (of_int 123)) 123;
    test 3 (to_int (of_int (-456))) (-456);
    test 4 (to_int (of_int 0x3FFFFFFF)) 0x3FFFFFFF;
    test 5 (to_int (of_int (-0x40000000))) (-0x40000000);

    testing_function "of_string";
    test 1 (of_string "0") (of_int 0);
    test 2 (of_string "123") (of_int 123);
    test 3 (of_string "-456") (of_int (-456));
    test 4 (of_string "123456789") (of_int 123456789);
    test 5 (of_string "0xABCDEF") (of_int 0xABCDEF);
    test 6 (of_string "-0o1234567012") (of_int (- 0o1234567012));
    test 7 (of_string "0b01010111111000001100")
           (of_int 0b01010111111000001100);
    test 8 (of_string "0x7FFFFFFFFFFFFFFF") max;
    test 9 (of_string "-0x8000000000000000") min;
    test 10 (of_string "0x8000000000000000") min;
    test 11 (of_string "0xFFFFFFFFFFFFFFFF") minus_one;

    testing_function "to_string, format";
    List.iter (fun (n, s) -> test n (to_string (of_string s)) s)
      [1, "0"; 2, "123"; 3, "-456"; 4, "1234567890";
       5, "1234567890123456789";
       6, "9223372036854775807";
       7, "-9223372036854775808"];
    List.iter (fun (n, s) -> test n (format "0x%X" (of_string s)) s)
      [7, "0x0"; 8, "0x123"; 9, "0xABCDEF"; 10, "0x1234567812345678";
       11, "0x7FFFFFFFFFFFFFFF"; 12, "0x8000000000000000";
       13, "0xFFFFFFFFFFFFFFFF"];
    test 14 (to_string max) "9223372036854775807";
    test 15 (to_string min) "-9223372036854775808";
    test 16 (to_string zero) "0";
    test 17 (to_string one) "1";
    test 18 (to_string minus_one) "-1";

    testing_function "neg";
    test 1 (neg (of_int 0)) (of_int 0);
    test 2 (neg (of_int 123)) (of_int (-123));
    test 3 (neg (of_int (-456))) (of_int 456);
    test 4 (neg (of_int 123456789)) (of_int (-123456789));
    test 5 (neg max) (of_string "-0x7FFFFFFFFFFFFFFF");
    test 6 (neg min) min;

    testing_function "add";
    test 1 (add (of_int 0) (of_int 0)) (of_int 0);
    test 2 (add (of_int 123) (of_int 0)) (of_int 123);
    test 3 (add (of_int 0) (of_int 456)) (of_int 456);
    test 4 (add (of_int 123) (of_int 456)) (of_int 579);
    test 5 (add (of_int (-123)) (of_int 456)) (of_int 333);
    test 6 (add (of_int 123) (of_int (-456))) (of_int (-333));
    test 7 (add (of_int (-123)) (of_int (-456))) (of_int (-579));
    test 8 (add (of_string "0x1234567812345678") 
                (of_string "0x9ABCDEF09ABCDEF"))
           (of_string "0x1be024671be02467");
    test 9 (add max max) (of_int (-2));
    test 10 (add min min) zero;
    test 11 (add max one) min;
    test 12 (add min minus_one) max;
    test 13 (add max min) minus_one;

    testing_function "sub";
    test 1 (sub (of_int 0) (of_int 0)) (of_int 0);
    test 2 (sub (of_int 123) (of_int 0)) (of_int 123);
    test 3 (sub (of_int 0) (of_int 456)) (of_int (-456));
    test 4 (sub (of_int 123) (of_int 456)) (of_int (-333));
    test 5 (sub (of_int (-123)) (of_int 456)) (of_int (-579));
    test 6 (sub (of_int 123) (of_int (-456))) (of_int 579);
    test 7 (sub (of_int (-123)) (of_int (-456))) (of_int 333);
    test 8 (sub (of_string "0x1234567812345678") 
                (of_string "0x9ABCDEF09ABCDEF"))
           (of_string "0x888888908888889");
    test 9 (sub max min) minus_one;
    test 10 (sub min max) one;
    test 11 (sub min one) max;
    test 12 (sub max minus_one) min;

    testing_function "mul";
    test 1 (mul (of_int 0) (of_int 0)) (of_int 0);
    test 1 (mul (of_int 123) (of_int 0)) (of_int 0);
    test 1 (mul (of_int 0) (of_int (-456))) (of_int 0);
    test 2 (mul (of_int 123) (of_int 1)) (of_int 123);
    test 3 (mul (of_int 1) (of_int (-456))) (of_int (-456));
    test 2 (mul (of_int 123) (of_int (-1))) (of_int (-123));
    test 3 (mul (of_int (-1)) (of_int (-456))) (of_int 456);
    test 4 (mul (of_int 123) (of_int 456)) (of_int 56088);
    test 5 (mul (of_int (-123)) (of_int 456)) (of_int (-56088));
    test 6 (mul (of_int 123) (of_int (-456))) (of_int (-56088));
    test 7 (mul (of_int (-123)) (of_int (-456))) (of_int 56088);
    test 8 (mul (of_string "0x12345678") (of_string "0x9ABCDEF"))
           (of_string "0xb00ea4e242d208");
    test 9 (mul max max) one;

    testing_function "div";
    List.iter
      (fun (n, a, b) -> test n (div (of_int a) (of_int b)) (of_int (a / b)))
      [1, 0, 2;
       2, 123, 1;
       3, -123, 1;
       4, 123, -1;
       5, -123, -1;
       6, 1275312364, 365;
       7, 16384, 256;
       8, -1275312364, 365;
       9, 1275312364, -365;
       10, 1234567, 12345678;
       11, 1234567, -12345678];

    testing_function "mod";
    List.iter
      (fun (n, a, b) -> test n (rem (of_int a) (of_int b)) (of_int (a mod b)))
      [1, 0, 2;
       2, 123, 1;
       3, -123, 1;
       4, 123, -1;
       5, -123, -1;
       6, 1275312364, 365;
       7, 16384, 256;
       8, -1275312364, 365;
       9, 1275312364, -365;
       10, 1234567, 12345678;
       11, 1234567, -12345678];

    testing_function "and";
    List.iter
      (fun (n, a, b, c) -> test n (logand (of_string a) (of_string b))
                                  (of_string c))
      [1, "0x1234567812345678", "0x9abcdef09abcdef0", "0x1234567012345670";
       2, "0x1234567812345678", "0x0fedcba90fedcba9", "0x224422802244228";
       3, "0xFFFFFFFFFFFFFFFF", "0x1234000012345678", "0x1234000012345678";
       4, "0", "0x1234567812345678", "0";
       5, "0x5555555555555555", "0xAAAAAAAAAAAAAAAA", "0"];

    testing_function "or";
    List.iter
      (fun (n, a, b, c) -> test n (logor (of_string a) (of_string b))
                                  (of_string c))
      [1, "0x1234567812345678", "0x9abcdef09abcdef0", "0x9abcdef89abcdef8";
       2, "0x1234567812345678", "0x0fedcba90fedcba9", "0x1ffddff91ffddff9";
       3, "0xFFFFFFFFFFFFFFFF", "0x12345678", "0xFFFFFFFFFFFFFFFF";
       4, "0", "0x1234567812340000", "0x1234567812340000";
       5, "0x5555555555555555", "0xAAAAAAAAAAAAAAAA", "0xFFFFFFFFFFFFFFFF"];

    testing_function "xor";
    List.iter
      (fun (n, a, b, c) -> test n (logxor (of_string a) (of_string b))
                                  (of_string c))
      [1, "0x1234567812345678", "0x9abcdef09abcdef0", "0x8888888888888888";
       2, "0x1234567812345678", "0x0fedcba90fedcba9", "0x1dd99dd11dd99dd1";
       3, "0xFFFFFFFFFFFFFFFF", "0x123456789ABCDEF", "0xfedcba9876543210";
       4, "0", "0x1234567812340000", "0x1234567812340000";
       5, "0x5555555555555555", "0xAAAAAAAAAAAAAAAA", "0xFFFFFFFFFFFFFFFF"];

    testing_function "shift_left";
    List.iter
      (fun (n, a, b, c) -> test n (shift_left (of_string a) b) (of_string c))
      [1, "1", 1, "2";
       2, "1", 2, "4";
       3, "1", 4, "0x10";
       4, "1", 62, "0x4000000000000000";
       5, "1", 63, "0x8000000000000000";
       6, "0x16236ABD45673", 7, "0xb11b55ea2b3980";
       7, "0x10", 59, "0x8000000000000000";
       8, "0x10", 60, "0"];

    testing_function "shift_right";
    List.iter
      (fun (n, a, b, c) -> test n (shift_right (of_string a) b) (of_string c))
      [1, "2", 1, "1";
       2, "4", 2, "1";
       3, "0x10", 4, "1";
       4, "0x40000000", 10, "0x100000";
       5, "0x8000000000000000", 63, "-1";
       6, "0xb11b55ea2b3980", 7, "0x16236ABD45673";
       7, "-0xb11b55ea2b3980", 7, "-389461927286387"];

    testing_function "shift_right_logical";
    List.iter
      (fun (n, a, b, c) -> test n (shift_right_logical (of_string a) b)
                                  (of_string c))
      [1, "2", 1, "1";
       2, "4", 2, "1";
       3, "0x10", 4, "1";
       4, "0x40000000", 10, "0x100000";
       5, "0x8000000000000000", 63, "1";
       6, "0xb11b55ea2b3980", 7, "0x16236ABD45673";
       7, "-0xb11b55ea2b3980", 7, "0x1fe9dc9542ba98d"];

    testing_function "Comparisons";
    test 1 (testcomp (of_int 0) (of_int 0))
           (true,false,false,false,true,true);
    test 2 (testcomp (of_int 1234567) (of_int 1234567))
           (true,false,false,false,true,true);
    test 3 (testcomp (of_int 0) (of_int 1))
           (false,true,true,false,true,false);
    test 4 (testcomp (of_int (-1)) (of_int 0))
           (false,true,true,false,true,false);
    test 5 (testcomp (of_int 1) (of_int 0))
           (false,true,false,true,false,true);
    test 6 (testcomp (of_int 0) (of_int (-1)))
           (false,true,false,true,false,true);
    test 7 (testcomp max min)
           (false,true,false,true,false,true);

    ()
end

(******** The test proper **********)

let testcomp_int32 (a : int32) (b : int32) =
  (a = b, a <> b, a < b, a > b, a <= b, a >= b)
let testcomp_int64 (a : int64) (b : int64) =
  (a = b, a <> b, a < b, a > b, a <= b, a >= b)
let testcomp_nativeint (a : nativeint) (b : nativeint) =
  (a = b, a <> b, a < b, a > b, a <= b, a >= b)

let _ =
  testing_function "-------- Int32 --------";
  let module A = Test32(struct type t = int32
                               module Ops = Int32
                               let testcomp = testcomp_int32 end) in
  print_newline(); testing_function "-------- Int64 --------";
  let module B = Test64(struct type t = int64
                               module Ops = Int64
                               let testcomp = testcomp_int64 end) in
  print_newline(); testing_function "-------- Nativeint --------";
  match Sys.word_size with
    32 ->
      let module C =
        Test32(struct type t = nativeint 
                      module Ops = Nativeint
                      let testcomp = testcomp_nativeint end)
      in ()
  | 64 ->
      let module C =
        Test64(struct type t = nativeint
                      module Ops = Nativeint
                      let testcomp = testcomp_nativeint end)
      in ()
  | _ ->
      assert false

(********* End of test *********)

let _ =
  print_newline();
  if !error_occurred then begin
    prerr_endline "************* TEST FAILED ****************"; exit 2
  end else
    exit 0
