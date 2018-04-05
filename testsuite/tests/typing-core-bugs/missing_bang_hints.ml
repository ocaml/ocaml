(* TEST
   * expect
*)


let r = ref 1 in
print_int r        (* should be [!r] *)
;;

[%%expect{|
Line _, characters 10-11:
  print_int r        (* should be [!r] *)
            ^
Error: This expression has type int ref
       but an expression was expected of type int
       Hint: Did you forget to use `!' to get the content of a reference somewhere?
|}];;


let r = ref 1 in
r := r + 1         (* should be [!r + 1] *)
;;

[%%expect{|
Line _, characters 5-6:
  r := r + 1         (* should be [!r + 1] *)
       ^
Error: This expression has type int ref
       but an expression was expected of type int
       Hint: Did you forget to use `!' to get the content of a reference somewhere?
|}];;



(* The missing `!' does not necessarily have to be inserted at the reported
   location in order to fix the typing error. This is illustrated by the
   following test, and is why the hint says "somewhere". *)

let f x y =
  let z = ref 0 in
  z := !z + x;
  z := !z + y;
  z                 (* missing `!' here? *)

let _ =
  print_int (f 3 4) (* or maybe there? *)
;;

[%%expect{|
val f : int -> int -> int ref = <fun>
Line _, characters 12-19:
    print_int (f 3 4) (* or maybe there? *)
              ^^^^^^^
Error: This expression has type int ref
       but an expression was expected of type int
       Hint: Did you forget to use `!' to get the content of a reference somewhere?
|}];;
