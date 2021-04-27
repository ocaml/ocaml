(* TEST
   * expect
*)
(** Test type-directed disambiguation and spellchecker hints *)

type t = ..
type t += Alpha | Aleph

module M = struct
  type w = ..
  type w += Alpha | Beta ;;
  type t += Beth
end;;

module F(X:sig end) = struct type u = .. type t += Gamma type u += Gamme end;;
module X = struct end;;
[%%expect {|
type t = ..
type t += Alpha | Aleph
module M : sig type w = .. type w += Alpha | Beta type t += Beth end
module F :
  functor (X : sig end) ->
    sig type u = .. type t += Gamma type u += Gamme end
module X : sig end
|}]

let x: t = Alph;;
[%%expect {|
Line 1, characters 11-15:
1 | let x: t = Alph;;
               ^^^^
Error: This variant expression is expected to have type t
       There is no constructor Alph within type t
Hint: Did you mean Aleph or Alpha?
|}]

open M;;
let y : w = Alha;;
[%%expect {|
Line 2, characters 12-16:
2 | let y : w = Alha;;
                ^^^^
Error: This variant expression is expected to have type M.w
       There is no constructor Alha within type M.w
Hint: Did you mean Alpha?
|}]

let z: t = Bet;;
[%%expect {|
Line 1, characters 11-14:
1 | let z: t = Bet;;
               ^^^
Error: This variant expression is expected to have type t
       There is no constructor Bet within type t
Hint: Did you mean Beth?
|}]


module N = F(X);;
open N
let g = (Gamm:t);;
[%%expect {|
module N : sig type u = F(X).u = .. type t += Gamma type u += Gamme end
Line 3, characters 9-13:
3 | let g = (Gamm:t);;
             ^^^^
Error: This variant expression is expected to have type t
       There is no constructor Gamm within type t
Hint: Did you mean Gamma?
|}];;

raise Not_Found;;
[%%expect {|
Line 1, characters 6-15:
1 | raise Not_Found;;
          ^^^^^^^^^
Error: This variant expression is expected to have type exn
       There is no constructor Not_Found within type exn
Hint: Did you mean Not_found?
|}]

(** Aliasing *)
type r = ..;;
module M = struct
  type t = r = ..
  type s = t = ..
  module N = struct
    type u = s = ..
    type u += Foo
  end
end
open M.N;;

type exn += Foo;;

let x : r = Foo;;
[%%expect {|
type r = ..
module M :
  sig
    type t = r = ..
    type s = t = ..
    module N : sig type u = s = .. type u += Foo end
  end
type exn += Foo
val x : r = M.N.Foo
|}]

(** Closed open extensible type support *)

module M : sig
  type t = private ..
  type t += Aleph
end = struct
  type t = ..
  type t += Aleph
end;;
open M;;

type exn += Aleph ;;
[%%expect {|
module M : sig type t = private .. type t += Aleph end
type exn += Aleph
|}]

let x : t = Aleph;;
[%%expect {|
val x : M.t = M.Aleph
|}]

module F(X: sig type t = .. end ) = struct type X.t+= Beth end
module X = struct type t = .. end
module FX = F(X) open FX
type exn += Beth;;
let x : X.t = Beth;;
[%%expect {|
module F : functor (X : sig type t = .. end) -> sig type X.t += Beth end
module X : sig type t = .. end
module FX : sig type X.t += Beth end
type exn += Beth
val x : X.t = <extension>
|}]

(** Aliasing *)

type x = ..
type x += Alpha
module P = struct type p = x end

let x: P.p = Alha;;
[%%expect {|
type x = ..
type x += Alpha
module P : sig type p = x end
Line 7, characters 13-17:
7 | let x: P.p = Alha;;
                 ^^^^
Error: This variant expression is expected to have type P.p
       There is no constructor Alha within type x
Hint: Did you mean Alpha?
|}]

module M = struct type t = .. type t += T end
module N = struct type s = M.t end
let y: N.s = T ;;
[%%expect {|
module M : sig type t = .. type t += T end
module N : sig type s = M.t end
Line 3, characters 13-14:
3 | let y: N.s = T ;;
                 ^
Error: This variant expression is expected to have type N.s
       There is no constructor T within type M.t
|}]

(** Pattern matching *)
type x = ..
type x += A | B
type u = A | B
module M = struct type y = .. type y+= A|B end
open M
let f: x -> int = function A -> 1 | B -> 2 | _ -> 0;;
[%%expect {|
type x = ..
type x += A | B
type u = A | B
module M : sig type y = .. type y += A | B  end
val f : x -> int = <fun>
|}]

(** Local exception *)
let x =
  let exception Local in
  raise Locl;;
[%%expect {|
Line 3, characters 8-12:
3 |   raise Locl;;
            ^^^^
Error: This variant expression is expected to have type exn
       There is no constructor Locl within type exn
Hint: Did you mean Local?
|}]

let x =
  let exception Local in
  let module M = struct type t = .. type t+= Local end in
  let open M in
  (Local:exn);;
[%%expect{|
val x : exn = Local
|}
]

(** Path capture *)
module M = struct type t = .. type t += T end
open M
let f = (=) M.T
module M = struct type t = .. type t += S end
open M
let y = f T ;;
[%%expect {|
module M : sig type t = .. type t += T end
val f : M.t -> bool = <fun>
module M : sig type t = .. type t += S end
val y : bool = true
|}]

(** Amniguity warning *)
[@@@warning "+41"];;
type a = Unique
type t = ..
type t += Unique
module M = struct type s = .. type s+= Unique end open M
type b = Unique
let x = Unique;;
[%%expect {|
type a = Unique
type t = ..
type t += Unique
module M : sig type s = .. type s += Unique end
type b = Unique
Line 7, characters 8-14:
7 | let x = Unique;;
            ^^^^^^
Warning 41 [ambiguous-name]: Unique belongs to several types: b M.s t a
The first one was selected. Please disambiguate if this is wrong.
val x : b = Unique
|}]
