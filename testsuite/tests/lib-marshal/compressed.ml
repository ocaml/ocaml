(* TEST
   include ocamlcommon;
*)

(* Test for compressed marshaling / unmarshaling *)

open Compression

let from_channel = Marshal.from_channel

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

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let test_out ?(flags = []) filename =
  let oc = open_out_bin filename in
  to_channel oc 1 flags;
  to_channel oc (-1) flags;
  to_channel oc 258 flags;
  to_channel oc 20000 flags;
  to_channel oc 0x12345678 flags;
  to_channel oc bigint flags;
  to_channel oc "foobargeebuz" flags;
  to_channel oc longstring flags;
  to_channel oc verylongstring flags;
  to_channel oc 3.141592654 flags;
  to_channel oc () flags;
  to_channel oc A flags;
  to_channel oc (B 1) flags;
  to_channel oc (C 2.718) flags;
  to_channel oc (D "hello, world!") flags;
  to_channel oc (E 'l') flags;
  to_channel oc (F(B 1)) flags;
  to_channel oc (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) flags;
  to_channel oc (H(1, A)) flags;
  to_channel oc (I(B 2, 1e-6)) flags;
  let x = D "sharing" in
  let y = G(x, x) in
  let z = G(y, G(x, y)) in
  to_channel oc z flags;
  to_channel oc [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|] flags;
  let rec big n = if n <= 0 then A else H(n, big(n-1)) in
  to_channel oc (big 1000) flags;
  to_channel oc y (Marshal.No_sharing :: flags);
  to_channel oc fib (Marshal.Closures :: flags);
  to_channel oc (Int32.of_string "0") flags;
  to_channel oc (Int32.of_string "123456") flags;
  to_channel oc (Int32.of_string "-123456") flags;
  to_channel oc (Int64.of_string "0") flags;
  to_channel oc (Int64.of_string "123456789123456") flags;
  to_channel oc (Int64.of_string "-123456789123456") flags;
  to_channel oc (Nativeint.of_string "0") flags;
  to_channel oc (Nativeint.of_string "123456") flags;
  to_channel oc (Nativeint.of_string "-123456") flags;
  to_channel oc
    (Nativeint.shift_left (Nativeint.of_string "123456789") 32) flags;
  to_channel oc
    (Nativeint.shift_left (Nativeint.of_string "-123456789") 32) flags;
  let i = Int64.of_string "123456789123456" in
    to_channel oc (i,i) flags;
  close_out oc


let test n b =
  print_string "Test "; print_int n;
  if b then print_string " passed.\n" else print_string " FAILED.\n";
  flush stderr

let test_in filename =
  let ic = open_in_bin filename in
  test 1 (from_channel ic = 1);
  test 2 (from_channel ic = (-1));
  test 3 (from_channel ic = 258);
  test 4 (from_channel ic = 20000);
  test 5 (from_channel ic = 0x12345678);
  test 6 (from_channel ic = bigint);
  test 7 (from_channel ic = "foobargeebuz");
  test 8 (from_channel ic = longstring);
  test 9 (from_channel ic = verylongstring);
  test 10 (from_channel ic = 3.141592654);
  test 11 (from_channel ic = ());
  test 12 (match from_channel ic with
    A -> true
  | _ -> false);
  test 13 (match from_channel ic with
    (B 1) -> true
  | _ -> false);
  test 14 (match from_channel ic with
    (C f) -> f = 2.718
  | _ -> false);
  test 15 (match from_channel ic with
    (D "hello, world!") -> true
  | _ -> false);
  test 16 (match from_channel ic with
    (E 'l') -> true
  | _ -> false);
  test 17 (match from_channel ic with
    (F(B 1)) -> true
  | _ -> false);
  test 18 (match from_channel ic with
    (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) -> true
  | _ -> false);
  test 19 (match from_channel ic with
    (H(1, A)) -> true
  | _ -> false);
  test 20 (match from_channel ic with
    (I(B 2, 1e-6)) -> true
  | _ -> false);
  test 21 (match from_channel ic with
    G((G((D "sharing" as t1), t2) as t3), G(t4, t5)) ->
      t1 == t2 && t3 == t5 && t4 == t1
  | _ -> false);
  test 22 (from_channel ic = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|]);
  let rec check_big n t =
    if n <= 0 then
      test 23 (match t with A -> true | _ -> false)
    else
      match t with H(m, s) -> if m = n then check_big (n-1) s
                                       else test 23 false
                 | _ -> test 23 false
  in
    check_big 1000 (from_channel ic);
  test 24 (match from_channel ic with
    G((D "sharing" as t1), (D "sharing" as t2)) -> t1 != t2
  | _ -> false);
  test 25 (let fib = (from_channel ic : int -> int) in fib 5 = 8 && fib 10 = 89);
  test 26 (from_channel ic = Int32.of_string "0");
  test 27 (from_channel ic = Int32.of_string "123456");
  test 28 (from_channel ic = Int32.of_string "-123456");
  test 29 (from_channel ic = Int64.of_string "0");
  test 30 (from_channel ic = Int64.of_string "123456789123456");
  test 31 (from_channel ic = Int64.of_string "-123456789123456");
  test 32 (from_channel ic = Nativeint.of_string "0");
  test 33 (from_channel ic = Nativeint.of_string "123456");
  test 34 (from_channel ic = Nativeint.of_string "-123456");
  test 35 (from_channel ic =
             Nativeint.shift_left (Nativeint.of_string "123456789") 32);
  test 36 (from_channel ic =
             Nativeint.shift_left (Nativeint.of_string "-123456789") 32);
  let ((i, j) : int64 * int64) = from_channel ic in
  test 37 (i = Int64.of_string "123456789123456");
  test 38 (j = Int64.of_string "123456789123456");
  test 39 (i == j);
  close_in ic

let main () =
  test_out "intext.data"; test_in "intext.data";
  Sys.remove "intext.data"

let _ = main ()
