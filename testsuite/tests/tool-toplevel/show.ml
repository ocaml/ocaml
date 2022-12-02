(* TEST
   * expect
*)

(* this is a set of tests to test the #show functionality
 * of toplevel *)

class o = object val x = 0 end;;
[%%expect{|
class o : object val x : int end
|}];;
#show o;;
[%%expect{|
type o = <  >
class o : object val x : int end
class type o = object val x : int end
|}];;
class type t = object val x : int end;;
[%%expect{|
class type t = object val x : int end
|}];;
#show t;;
[%%expect{|
type t = <  >
class type t = object val x : int end
|}];;

#show Foo;;
[%%expect {|
Unknown element.
|}];;

module type S = sig type t val x : t end;;
module M : S = struct type t = int let x = 3 end;;

[%%expect {|
module type S = sig type t val x : t end
module M : S
|}];;

#show M;;
[%%expect {|
module M : S
|}];;

#show S;;
[%%expect {|
module type S = sig type t val x : t end
|}];;

#show Invalid_argument;;
[%%expect {|
exception Invalid_argument of string
|}];;

#show Some;;
[%%expect {|
type 'a option = None | Some of 'a
|}];;

#show option;;
[%%expect {|
type 'a option = None | Some of 'a
|}];;

#show Open_binary;;
[%%expect {|
type Stdlib.open_flag =
    Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock
|}];;

#show open_flag;;
[%%expect {|
type open_flag =
    Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock
|}];;

type extensible = ..;;
type extensible += A | B of int;;
[%%expect {|
type extensible = ..
type extensible += A | B of int
|}];;

#show A;;
[%%expect {|
type extensible += A
|}];;

#show B;;
[%%expect {|
type extensible += B of int
|}];;

#show extensible;;
[%%expect {|
type extensible = ..
|}];;

type 'a t = ..;;
type _ t += A : int t;;
[%%expect{|
type 'a t = ..
type _ t += A : int t
|}];;

#show A;;
[%%expect{|
type 'a t += A : int t
|}];;




(* regression tests for #11533 *)
#show Set.OrderedType;;
[%%expect {|
module type OrderedType = sig type t val compare : t -> t -> int end
|}];;

(* extra tests after #11533

   The regression in #11533 would only show up when showing values defined
   outside the current module. Those new tests below test modules and module
   types from the standard library. To minimize test churn / promotion,
   we are looking for some that will change as little as possible
   in the future.

   - For module type it's easy: OrderedType is fixed in stone as
     changing it would break all code using Set.Make.

   - For modules we use Stdlib.Unit, one of the stdlib modules
     that is less likely to change very often (there are only
     so many features you can add to 'unit').
*)
module U = Stdlib.Unit;;
module type OT = Set.OrderedType;;
[%%expect {|
module U = Unit
module type OT = Set.OrderedType
|}];;

#show U;;
[%%expect {|
module U = Unit
module U :
  sig
    type t = unit = ()
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
  end
|}];;

#show OT;;
[%%expect {|
module type OT = Set.OrderedType
module type OT = sig type t val compare : t -> t -> int end
|}];;
