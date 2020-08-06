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
(* this output is INCORRECT, it should not use nonrec *)
[%%expect{|
type nonrec t = T of t
|}];;

type nonrec s = Foo of t;;
[%%expect{|
type nonrec s = Foo of t
|}];;
#show s;;
(* this output is CORRECT, it uses nonrec *)
[%%expect{|
type nonrec s = Foo of t
|}];;



module M : sig type t val x : t end = struct type t = int let x = 0 end;;
[%%expect{|
module M : sig type t val x : t end
|}];;
(* this output is CORRECT, it does not use 'rec' *)
[%%expect{|
|}];;

module rec M : sig type t val x : M.t end = struct type t = int let x = 0 end;;
(* this output is strange, it is surprising to use M/2 here. *)
[%%expect{|
module rec M : sig type t val x : M/2.t end
|}];;
#show_module M;;
(* this output is INCORRECT, it should use 'rec' *)
[%%expect{|
module M : sig type t val x : M.t end
|}];;
