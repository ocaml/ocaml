(* TEST
   * expect
*)

module X = struct
  type t =
    | A : 'a * 'b * ('a -> unit) -> t
end;;
[%%expect{|
module X : sig type t = A : 'a * 'b * ('a -> unit) -> t end
|}]

module Y = struct
  type t = X.t =
    | A : 'a * 'b * ('b -> unit) -> t
end;; (* should fail *)
[%%expect{|
Lines 2-3, characters 2-37:
2 | ..type t = X.t =
3 |     | A : 'a * 'b * ('b -> unit) -> t
Error: This variant or record definition does not match that of type X.t
       Constructors do not match:
         A : 'a * 'b * ('a -> unit) -> X.t
       is not the same as:
         A : 'a * 'b * ('b -> unit) -> X.t
       The type 'a -> unit is not equal to the type 'b -> unit
       Type 'a is not equal to type 'b
|}]

(* would segfault
let () =
  match Y.A (1, "", print_string) with
  | X.A (x, y, f) -> f x
*)
