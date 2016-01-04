let id x = x [@@inlne]
let f x = ( (fun x->x) [@inlne] [@inlind] ) x (* trigger 2*2=4 warnings *)
let () = (f [@inlind]) ()

let () = ( (fun x -> x) [@inlined] ) () (* no spelling mistake detected *)

[@@@warnerrror "+500"]

type 'a trap = Nothing of 'a [@warn_on_litteral_pattern]
let warn_pattern = function Nothing 0 -> true | Nothing _ -> false

let rec nop =
  function[@warnig "-300"]
  | _ :: q ->  ( nop[@tailcal] ) q
  | [] -> []

module Archaic = struct end [@@deprecate] (* trigger 2 warnings? *)

type 'a immutable = { mutable content: 'a [@deprecate_mutable] }

let f x = x.content <- 1

external k : (int [@untaged]) -> (float [@unboxd]) = "k" [@@no_alloc]

module Deactivate_warning = struct
external k : (int [@untaged]) -> (float [@unboxd]) = "k" [@@no_alloc]
end[@@warning "-59"]
