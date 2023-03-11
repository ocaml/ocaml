(* TEST
   flags = "-dshape"
   * expect
*)

let x = ()
[%%expect{|
{
 "x"[value] -> <.0>;
 }
val x : unit = ()
|}]

external y : int -> int = "%identity"
[%%expect{|
{
 "y"[value] -> <.1>;
 }
external y : int -> int = "%identity"
|}]

type t = A of foo
and foo = Bar
[%%expect{|
{
 "foo"[type] -> <.3>;
 "t"[type] -> <.2>;
 }
type t = A of foo
and foo = Bar
|}]

module type S = sig
  type t
end
[%%expect{|
{
 "S"[module type] -> <.7>;
 }
module type S = sig type t end
|}]

exception E
[%%expect{|
{
 "E"[extension constructor] -> <.8>;
 }
exception E
|}]

type ext = ..
[%%expect{|
{
 "ext"[type] -> <.9>;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> <.10>;
 "B"[extension constructor] -> <.11>;
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.13>
                 "C"[extension constructor] -> <.12>;
                 };
 }
module M : sig type ext += C end
|}]

module _ = struct
  type t = Should_not_appear_in_shape
end
[%%expect{|
{
 }
|}]

module rec M1 : sig
  type t = C of M2.t
end = struct
  type t = C of M2.t
end

and M2 : sig
  type t
  val x : t
end = struct
  type t = T
  let x = T
end
[%%expect{|
{
 "M1"[module] -> {
                  "t"[type] -> <.27>;
                  };
 "M2"[module] -> {
                  "t"[type] -> <.29>;
                  "x"[value] -> <.31>;
                  };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.32>;
 "c"[class] -> <.32>;
 "c"[class type] -> <.32>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.34>;
 "c"[class type] -> <.34>;
 }
class type c = object  end
|}]
