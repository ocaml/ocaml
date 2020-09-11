(* TEST
   * expect
*)

(* Test a success case *)
type 'a t = T of 'a
;;
T 123
[%%expect {|
type 'a t = T of 'a
- : int t = T 123
|}]

(* no <poly> after fix *)
type _ t = ..
type 'a t += T of 'a
;;
T 123
[%%expect {|
type _ t = ..
type 'a t += T of 'a
- : int t = T 123
|}]


(* GADT with fixed arg type *)
type _ t += T: char -> int t
;;
T 'x'
[%%expect {|
type _ t += T : char -> int t
- : int t = T 'x'
|}]


(* GADT with poly arg type.... and the expected T <poly> *)
type _ t += T: 'a -> int t
;;
T 'x'
[%%expect {|
type _ t += T : 'a -> int t
- : int t = T <poly>
|}]

(* the rest are expected without <poly> *)
type _ t += T: 'a * bool -> 'a t
;;
T ('x',true)
[%%expect {|
type _ t += T : 'a * bool -> 'a t
- : char t = T ('x', true)
|}]

type _ t += T: 'a -> ('a * bool) t
;;
T 'x'
[%%expect {|
type _ t += T : 'a -> ('a * bool) t
- : (char * bool) t = T 'x'
|}]
