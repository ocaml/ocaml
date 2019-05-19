(* TEST
   * expect
*)
let rec h =
  let rec f n = if n >= 0 then g (n - 1)
  and g n = h n; f n in
  f;;
[%%expect{|
Lines 2-4, characters 2-3:
2 | ..let rec f n = if n >= 0 then g (n - 1)
3 |   and g n = h n; f n in
4 |   f..
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

(* (* if this was accepted, we could segfault by doing: *)
let () = Gc.minor ()
let () = ignore (h 10)
*)
