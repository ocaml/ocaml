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

(* strengthening *)

type 'a t = (('a -> unit) -> unit);;
let tl = !(ref ([] : 'a t list));;
[%%expect{|
type 'a t = ('a -> unit) -> unit
val tl : '_a t list = []
|}]

type 'a u = U of (('a -> unit) -> unit);;
let ul = !(ref ([] : 'a u list));;
[%%expect{|
type 'a u = U of (('a -> unit) -> unit)
val ul : 'a u list = []
|}]
