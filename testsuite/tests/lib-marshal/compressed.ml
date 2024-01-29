(* TEST
   include ocamlcommon;
*)

(* Test for compressed marshaling / unmarshaling *)

open Compression

let max_data_depth = 500000

type t = A | B of int | C of float | D of string | E of char
       | F of t | G of t * t | H of int * t | I of t * float | J

let longstring =
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
let verylongstring =
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

let bigint = Int64.to_int 0x123456789ABCDEF0L

let test_out ?(flags = []) filename =
  let oc = open_out_bin filename in
  output_value oc 1;
  output_value oc (-1);
  output_value oc 258;
  output_value oc 20000;
  output_value oc 0x12345678;
  output_value oc bigint;
  output_value oc "foobargeebuz";
  output_value oc longstring;
  output_value oc verylongstring;
  output_value oc 3.141592654;
  output_value oc ();
  output_value oc A;
  output_value oc (B 1);
  output_value oc (C 2.718);
  output_value oc (D "hello, world!");
  output_value oc (E 'l');
  output_value oc (F(B 1));
  output_value oc (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e')))));
  output_value oc (H(1, A));
  output_value oc (I(B 2, 1e-6));
  let x = D "sharing" in
  let y = G(x, x) in
  let z = G(y, G(x, y)) in
  output_value oc z;
  output_value oc [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|];
  let rec big n = if n <= 0 then A else H(n, big(n-1)) in
  output_value oc (big 1000);
  output_value oc (Int32.of_string "0");
  output_value oc (Int32.of_string "123456");
  output_value oc (Int32.of_string "-123456");
  output_value oc (Int64.of_string "0");
  output_value oc (Int64.of_string "123456789123456");
  output_value oc (Int64.of_string "-123456789123456");
  output_value oc (Nativeint.of_string "0");
  output_value oc (Nativeint.of_string "123456");
  output_value oc (Nativeint.of_string "-123456");
  output_value oc
    (Nativeint.shift_left (Nativeint.of_string "123456789") 32);
  output_value oc
    (Nativeint.shift_left (Nativeint.of_string "-123456789") 32);
  let i = Int64.of_string "123456789123456" in
    output_value oc (i,i);
  close_out oc


let test n b =
  print_string "Test "; print_int n;
  if b then print_string " passed.\n" else print_string " FAILED.\n";
  flush stderr

let test_in filename =
  let ic = open_in_bin filename in
  test 1 (input_value ic = 1);
  test 2 (input_value ic = (-1));
  test 3 (input_value ic = 258);
  test 4 (input_value ic = 20000);
  test 5 (input_value ic = 0x12345678);
  test 6 (input_value ic = bigint);
  test 7 (input_value ic = "foobargeebuz");
  test 8 (input_value ic = longstring);
  test 9 (input_value ic = verylongstring);
  test 10 (input_value ic = 3.141592654);
  test 11 (input_value ic = ());
  test 12 (match input_value ic with
    A -> true
  | _ -> false);
  test 13 (match input_value ic with
    (B 1) -> true
  | _ -> false);
  test 14 (match input_value ic with
    (C f) -> f = 2.718
  | _ -> false);
  test 15 (match input_value ic with
    (D "hello, world!") -> true
  | _ -> false);
  test 16 (match input_value ic with
    (E 'l') -> true
  | _ -> false);
  test 17 (match input_value ic with
    (F(B 1)) -> true
  | _ -> false);
  test 18 (match input_value ic with
    (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) -> true
  | _ -> false);
  test 19 (match input_value ic with
    (H(1, A)) -> true
  | _ -> false);
  test 20 (match input_value ic with
    (I(B 2, 1e-6)) -> true
  | _ -> false);
  test 21 (match input_value ic with
    G((G((D "sharing" as t1), t2) as t3), G(t4, t5)) ->
      t1 == t2 && t3 == t5 && t4 == t1
  | _ -> false);
  test 22 (input_value ic = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|]);
  let rec check_big n t =
    if n <= 0 then
      test 23 (match t with A -> true | _ -> false)
    else
      match t with H(m, s) -> if m = n then check_big (n-1) s
                                       else test 23 false
                 | _ -> test 23 false
  in
    check_big 1000 (input_value ic);
  test 26 (input_value ic = Int32.of_string "0");
  test 27 (input_value ic = Int32.of_string "123456");
  test 28 (input_value ic = Int32.of_string "-123456");
  test 29 (input_value ic = Int64.of_string "0");
  test 30 (input_value ic = Int64.of_string "123456789123456");
  test 31 (input_value ic = Int64.of_string "-123456789123456");
  test 32 (input_value ic = Nativeint.of_string "0");
  test 33 (input_value ic = Nativeint.of_string "123456");
  test 34 (input_value ic = Nativeint.of_string "-123456");
  test 35 (input_value ic =
             Nativeint.shift_left (Nativeint.of_string "123456789") 32);
  test 36 (input_value ic =
             Nativeint.shift_left (Nativeint.of_string "-123456789") 32);
  let ((i, j) : int64 * int64) = input_value ic in
  test 37 (i = Int64.of_string "123456789123456");
  test 38 (j = Int64.of_string "123456789123456");
  test 39 (i == j);
  close_in ic

let test_supported filename =
  Out_channel.(with_open_bin filename (fun oc -> output_value oc ()));
  let s = In_channel.(with_open_bin filename input_all) in
  let actually_supported =
    match s.[3] with
    | '\xBD' -> true
    | '\xBE' -> false
    | _ -> assert false in
  test 100 (actually_supported = compression_supported)

let main () =
  test_out "intext.data"; test_in "intext.data";
  test_supported "intext.data";
  Sys.remove "intext.data"

let _ = main ()
