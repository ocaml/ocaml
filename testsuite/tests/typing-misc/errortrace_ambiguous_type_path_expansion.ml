(* TEST
 expect;
*)

(* Regression test for [trees_of_type_path_expansion]: when printing a
   [Name_type_mismatch] error, the expected record type may be a type alias
   [outer] whose expansion is a distinct path [inner]. The message must show
   ["outer" = "inner"], not ["outer" = "outer"]. *)

module Other = struct
  type t = { y : int }
end

type inner = { x : int }
type outer = inner

let f (r : outer) = r.Other.y

[%%expect{|
module Other : sig type t = { y : int; } end
type inner = { x : int; }
type outer = inner
Line 8, characters 22-29:
8 | let f (r : outer) = r.Other.y
                          ^^^^^^^
Error: The field "Other.y" belongs to the record type "Other.t"
       but a field was expected belonging to the record type "outer" = "inner"
|}]
