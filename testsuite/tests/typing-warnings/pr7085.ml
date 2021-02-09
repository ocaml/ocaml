(* TEST
   flags = " -w +A -strict-sequence "
   * expect
*)

module TypEq = struct
 type (_, _) t = Eq : ('a, 'a) t
end

module type T = sig
 type _ is_t = Is : ('a, 'b) TypEq.t -> 'a is_t
 val is_t : unit -> unit is_t option
end

module Make (M : T) =
 struct
   let _ =
     match M.is_t () with
     | None -> 0
     | Some _ -> 0
   let f () =
     match M.is_t () with None -> 0
end;;
[%%expect {|
module TypEq : sig type (_, _) t = Eq : ('a, 'a) t end
module type T =
  sig
    type _ is_t = Is : ('a, 'b) TypEq.t -> 'a is_t
    val is_t : unit -> unit is_t option
  end
Line 17, characters 5-35:
17 |      match M.is_t () with None -> 0
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some (Is Eq)
module Make : functor (M : T) -> sig val f : unit -> int end
|}]

module Make2 (M : T) = struct
  type t = T of unit M.is_t
  let g : t -> int = function _ -> .
end;;
[%%expect {|
Line 3, characters 30-31:
3 |   let g : t -> int = function _ -> .
                                  ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: T (Is Eq)
|}]
