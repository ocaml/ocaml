(* TEST
   * expect
*)

(* This is a known-bug file for use of 'rec' by the '#show' command,
   to record known regressions from #7453 and #9094 *)

type t = T of t;;
[%%expect{|
type t = T of t
|}]
#show t;;
(* this output is CORRECT, it should not use nonrec *)
[%%expect{|
type t = T of t
|}];;

type nonrec s = Foo of t;;
[%%expect{|
type nonrec s = Foo of t
|}];;
#show s;;
(* this output is CORRECT, it elides the unnecessary nonrec keyword *)
[%%expect{|
type s = Foo of t
|}];;



module M : sig type t val x : t end = struct type t = int let x = 0 end;;
[%%expect{|
module M : sig type t val x : t end
|}];;
(* this output is CORRECT, it does not use 'rec' *)

module rec M : sig type t val x : M.t end = struct type t = int let x = 0 end;;
(* this output is CORRECT . *)
[%%expect{|
module rec M : sig type t val x : M.t end
|}];;
#show_module M;;
(* this output is CORRECT *)
[%%expect{|
module rec M : sig type t val x : M.t end
|}];;


(* Indirect recursion *)

type t
type f = [ `A of t ]
type t = X of u | Y of [ f | `B ]  and u = Y of t;;

[%%expect{|
type t
type f = [ `A of t ]
type t = X of u | Y of [ `A of t/1 | `B ]
and u = Y of t/2
|}];;

#show t;;
(* this output is PARTIAL: t is mutually recursive with u *)
[%%expect{|
type nonrec t = X of u | Y of [ `A of t/2 | `B ]
|}];;


module rec M: sig type t = X of N.t end = M
and N: sig type t = X of M.t end = N

[%%expect{|
module rec M : sig type t = X of N.t end
and N : sig type t = X of M.t end
|}];;

(* this output is PARTIAL: M is mutually recursive with N *)
#show M;;
[%%expect{|
module M : sig type t = X of N.t end
|}];;
