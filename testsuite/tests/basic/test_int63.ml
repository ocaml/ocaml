(* Test the types nativeint, int32, int64 *)

open Printf

let error_occurred = ref false

let function_tested = ref ""

let testing_function s =
    function_tested := s;
    print_newline();
    print_string s;
    print_newline()

let test_strings test_number answer correct_answer =
 flush stdout;
 flush stderr;
 if answer <> correct_answer then begin
   eprintf "*** Bad result (%s, test %d): expected \"%s\", got \"%s\"\n" !function_tested test_number correct_answer answer;
   flush stderr;
   error_occurred := true
 end else begin
   printf " %d..." test_number
 end

let test_int63 test_number answer correct_answer =
 flush stdout;
 flush stderr;
 if answer <> correct_answer then begin
   eprintf "*** Bad result (%s, test %d): expected %s, got %s\n" !function_tested test_number
     (Int63.to_string correct_answer) (Int63.to_string answer);
   flush stderr;
   error_occurred := true
 end else begin
   printf " %d..." test_number
 end

let test_ints test_number answer correct_answer =
 flush stdout;
 flush stderr;
 if answer <> correct_answer then begin
   eprintf "*** Bad result (%s, test %d): expected %d, got %d\n" !function_tested test_number
     correct_answer answer;
   flush stderr;
   error_occurred := true
 end else begin
   printf " %d..." test_number
 end


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

(***** Tests on 63 bit arithmetic *****)

let testcomp_int63 (a : Int63.t) (b : Int63.t) =
  (a = b, a <> b, a < b, a > b, a <= b, a >= b, compare a b)

module Test63 =
struct
  module M = struct type t = Int63.t
    module Ops = Int63
    let testcomp = testcomp_int63 end
  open M
  open Ops
  open Int63

  let _ =
    testing_function "of_int, to_int";
    test_ints 1 (to_int (of_int 0)) 0;
    test_ints 2 (to_int (of_int 123)) 123;
    test_ints 3 (to_int (of_int (-456))) (-456);
    test_ints 4 (to_int (of_int 0x3FFFFFFF)) 0x3FFFFFFF;
    test_ints 5 (to_int (of_int (-0x40000000))) (-0x40000000);

    testing_function "of_string";
    test_int63 1 (of_string "0") (of_int 0);
    test_int63 2 (of_string "123") (of_int 123);
    test_int63 3 (of_string "-456") (of_int (-456));
    test_int63 4 (of_string "123456789") (of_int 123456789);
    test_int63 5 (of_string "0xABCDEF") (of_int 0xABCDEF);
    test_int63 6 (of_string "-0o1234567012") (of_int (- 0o1234567012));
    test_int63 7 (of_string "0b01010111111000001100")
      (of_int 0b01010111111000001100);
    test_int63 8 (of_string "0x3FFFFFFFFFFFFFFF") max_int;
    test_int63 9 (of_string "-0x4000000000000000") min_int;
    test_int63 10 (of_string "0x4000000000000000") min_int;
    test_int63 11 (of_string "0x7FFFFFFFFFFFFFFF") minus_one;

    testing_function "to_string";
    List.iter (fun (n, s) -> test_strings n (to_string (of_string s)) s)
      [1, "0"; 2, "123"; 3, "-456"; 4, "1234567890";
       5, "1234567890123456789";
       6, "4611686018427387903";
       7, "-4611686018427387904"];
    test_strings 15 (to_string max_int) "4611686018427387903";
    test_strings 16 (to_string min_int) "-4611686018427387904";
    test_strings 17 (to_string zero) "0";
    test_strings 18 (to_string one) "1";
    test_strings 19 (to_string minus_one) "-1";

    testing_function "neg";
    test_int63 1 (neg (of_int 0)) (of_int 0);
    test_int63 2 (neg (of_int 123)) (of_int (-123));
    test_int63 3 (neg (of_int (-456))) (of_int 456);
    test_int63 4 (neg (of_int 123456789)) (of_int (-123456789));
    test_int63 5 (neg max_int) (of_string "-0x3FFFFFFFFFFFFFFF");
    test_int63 6 (neg min_int) min_int;

    testing_function "add";
    test_int63 1 (add (of_int 0) (of_int 0)) (of_int 0);
    test_int63 2 (add (of_int 123) (of_int 0)) (of_int 123);
    test_int63 3 (add (of_int 0) (of_int 456)) (of_int 456);
    test_int63 4 (add (of_int 123) (of_int 456)) (of_int 579);
    test_int63 5 (add (of_int (-123)) (of_int 456)) (of_int 333);
    test_int63 6 (add (of_int 123) (of_int (-456))) (of_int (-333));
    test_int63 7 (add (of_int (-123)) (of_int (-456))) (of_int (-579));
    test_int63 8 (add (of_string "0x1234567812345678")
                   (of_string "0x9ABCDEF09ABCDEF"))
                (of_string "0x1be024671be02467");
    test_int63 9 (add max_int max_int) (of_int (-2));
    test_int63 10 (add min_int min_int) zero;
    test_int63 11 (add max_int one) min_int;
    test_int63 12 (add min_int minus_one) max_int;
    test_int63 13 (add max_int min_int) minus_one;

    testing_function "sub";
    test_int63 1 (sub (of_int 0) (of_int 0)) (of_int 0);
    test_int63 2 (sub (of_int 123) (of_int 0)) (of_int 123);
    test_int63 3 (sub (of_int 0) (of_int 456)) (of_int (-456));
    test_int63 4 (sub (of_int 123) (of_int 456)) (of_int (-333));
    test_int63 5 (sub (of_int (-123)) (of_int 456)) (of_int (-579));
    test_int63 6 (sub (of_int 123) (of_int (-456))) (of_int 579);
    test_int63 7 (sub (of_int (-123)) (of_int (-456))) (of_int 333);
    test_int63 8 (sub (of_string "0x1234567812345678")
                (of_string "0x9ABCDEF09ABCDEF"))
           (of_string "0x888888908888889");
    test_int63 9 (sub max_int min_int) minus_one;
    test_int63 10 (sub min_int max_int) one;
    test_int63 11 (sub min_int one) max_int;
    test_int63 12 (sub max_int minus_one) min_int;

    testing_function "mul";
    test_int63 1 (mul (of_int 0) (of_int 0)) (of_int 0);
    test_int63 2 (mul (of_int 123) (of_int 0)) (of_int 0);
    test_int63 3 (mul (of_int 0) (of_int (-456))) (of_int 0);
    test_int63 4 (mul (of_int 123) (of_int 1)) (of_int 123);
    test_int63 5 (mul (of_int 1) (of_int (-456))) (of_int (-456));
    test_int63 6 (mul (of_int 123) (of_int (-1))) (of_int (-123));
    test_int63 7 (mul (of_int (-1)) (of_int (-456))) (of_int 456);
    test_int63 8 (mul (of_int 123) (of_int 456)) (of_int 56088);
    test_int63 9 (mul (of_int (-123)) (of_int 456)) (of_int (-56088));
    test_int63 10 (mul (of_int 123) (of_int (-456))) (of_int (-56088));
    test_int63 11 (mul (of_int (-123)) (of_int (-456))) (of_int 56088);
    test_int63 12 (mul (of_string "0x12345678") (of_string "0x9ABCDEF"))
      (of_string "0xb00ea4e242d208");
    test_int63 13 (mul max_int max_int) one;

    testing_function "div";
    List.iter
      (fun (n, a, b) -> test_int63 n (div (of_int a) (of_int b)) (of_int (a / b)))
      [1, 0, 2;
       2, 123, 1;
       3, -123, 1;
       4, 123, -1;
       5, -123, -1;
       6, 127531236, 365;
       7, 16384, 256;
       8, -127531236, 365;
       9, 127531236, -365;
       10, 1234567, 12345678;
       11, 1234567, -12345678];
    test_int63 12 (div min_int (of_int (-1))) min_int;

    testing_function "mod";
    List.iter
      (fun (n, a, b) -> test_int63 n (rem (of_int a) (of_int b)) (of_int (a mod b)))
      [1, 0, 2;
       2, 123, 1;
       3, -123, 1;
       4, 123, -1;
       5, -123, -1;
       6, 127531236, 365;
       7, 16384, 256;
       8, -127531236, 365;
       9, 127531236, -365;
       10, 1234567, 12345678;
       11, 1234567, -12345678];
    test_int63 12 (rem min_int (of_int (-1))) (of_int 0);

    testing_function "and";
    List.iter
      (fun (n, a, b, c) -> test_int63 n (logand (of_string a) (of_string b))
                                  (of_string c))
      [1, "0x1234567812345678", "0x1abcdef09abcdef0", "0x1234567012345670";
       2, "0x1234567812345678", "0x0fedcba90fedcba9", "0x224422802244228";
       3, "0x7FFFFFFFFFFFFFFF", "0x1234000012345678", "0x1234000012345678";
       4, "0", "0x1234567812345678", "0";
       5, "0x5555555555555555", "0x2AAAAAAAAAAAAAAA", "0"];

    testing_function "or";
    List.iter
      (fun (n, a, b, c) -> test_int63 n (logor (of_string a) (of_string b))
                                  (of_string c))
      [1, "0x1234567812345678", "0x1abcdef09abcdef0", "0x1abcdef89abcdef8";
       2, "0x1234567812345678", "0x0fedcba90fedcba9", "0x1ffddff91ffddff9";
       3, "0x7FFFFFFFFFFFFFFF", "0x12345678", "0x7FFFFFFFFFFFFFFF";
       4, "0", "0x1234567812340000", "0x1234567812340000";
       5, "0x5555555555555555", "0x2AAAAAAAAAAAAAAA", "0x7FFFFFFFFFFFFFFF"];

    testing_function "xor";
    List.iter
      (fun (n, a, b, c) -> test_int63 n (logxor (of_string a) (of_string b))
                                  (of_string c))
      [1, "0x1234567812345678", "0x1abcdef09abcdef0", "0x0888888888888888";
       2, "0x1234567812345678", "0x0fedcba90fedcba9", "0x1dd99dd11dd99dd1";
       3, "0x7FFFFFFFFFFFFFFF", "0x123456789ABCDEF", "0x7edcba9876543210";
       4, "0", "0x1234567812340000", "0x1234567812340000";
       5, "0x5555555555555555", "0x2AAAAAAAAAAAAAAA", "0x7FFFFFFFFFFFFFFF"];

    testing_function "shift_left";
    List.iter
      (fun (n, a, b, c) -> test_int63 n (shift_left (of_string a) b) (of_string c))
      [1, "1", 1, "2";
       2, "1", 2, "4";
       3, "1", 4, "0x10";
       4, "1", 62, "0x4000000000000000";
       5, "0x16236ABD45673", 7, "0xb11b55ea2b3980";
       6, "0x10", 58, "0x4000000000000000";
       7, "0x10", 60, "0"];

    testing_function "shift_right";
    List.iter
      (fun (n, a, b, c) -> test_int63 n (shift_right (of_string a) b) (of_string c))
      [1, "2", 1, "1";
       2, "4", 2, "1";
       3, "0x10", 4, "1";
       4, "0x40000000", 10, "0x100000";
       5, "0x4000000000000000", 62, "-1";
       6, "0xb11b55ea2b3980", 7, "0x16236ABD45673";
       7, "-0xb11b55ea2b3980", 7, "-389461927286387"];

    testing_function "shift_right_logical";
    List.iter
      (fun (n, a, b, c) -> test_int63 n (shift_right_logical (of_string a) b)
                                  (of_string c))
      [1, "2", 1, "1";
       2, "4", 2, "1";
       3, "0x10", 4, "1";
       4, "0x40000000", 10, "0x100000";
       5, "0x4000000000000000", 62, "1";
       6, "0xb11b55ea2b3980", 7, "0x16236ABD45673";
       7, "-0xb11b55ea2b3980", 7, "0xfe9dc9542ba98d"];

    testing_function "Comparisons";
    test 1 (testcomp (of_int 0) (of_int 0))
           (true,false,false,false,true,true,0);
    test 2 (testcomp (of_int 1234567) (of_int 1234567))
           (true,false,false,false,true,true,0);
    test 3 (testcomp (of_int 0) (of_int 1))
           (false,true,true,false,true,false,-1);
    test 4 (testcomp (of_int (-1)) (of_int 0))
           (false,true,true,false,true,false,-1);
    test 5 (testcomp (of_int 1) (of_int 0))
           (false,true,false,true,false,true,1);
    test 6 (testcomp (of_int 0) (of_int (-1)))
           (false,true,false,true,false,true,1);
    test 7 (testcomp max_int min_int)
           (false,true,false,true,false,true,1);

    print_newline(); testing_function "--------- Conversions -----------";
    testing_function "Int32 of/to Int63.t";
    test 1 (Int63.to_int32 (Int63.of_string "0x12345678"))
      (Int32.of_string "0x12345678");
    test 2 (Int63.of_int32 (Int32.of_string "0x12345678"))
      (Int63.of_string "0x12345678");
    test 3 (Int63.to_int32 (Int63.of_string "0x123456789ABCDEF0"))
      (Int32.of_string "0x9ABCDEF0");

    testing_function "int64 of/to Int63.t";
    test 1 (Int63.to_int64 (Int63.of_string "-0x12345678"))
      (Int64.of_string "-0x12345678");
    test 2 (Int63.to_int64 (Int63.of_string "0x1234567812345678"))
      (Int64.of_string "0x1234567812345678");

    testing_function "Int63.t of/to nativeint";
    test 1 (Int63.of_nativeint (Nativeint.of_string "0x12345678"))
      (Int63.of_string "0x12345678");
    test 2 (Int63.to_nativeint (Int63.of_string "-0x12345678"))
      (Nativeint.of_string "-0x12345678");
    test 3 (Int63.to_nativeint (Int63.of_string "0x123456789ABCDEF0"))
      (if Sys.word_size = 64
       then Nativeint.of_string "0x123456789ABCDEF0"
       else Nativeint.of_string "0x9ABCDEF0");

    ()
end

let _ =
  print_newline();
  if !error_occurred then begin
    prerr_endline "************* TEST FAILED ****************"; exit 2
  end else
    exit 0
