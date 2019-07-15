(* TEST
   * expect
*)

(* #8698 *)

(* Actually, this is not a bug *)
type +'a t = [> `Foo of 'a -> unit] as 'a;;
[%%expect{|
type 'a t = 'a constraint 'a = [> `Foo of 'a -> unit ]
|}, Principal{|
type +'a t = 'a constraint 'a = [> `Foo of 'a -> unit ]
|}]
