(* TEST
   * expect
*)

(* PR#6394 *)

module rec X : sig
 type t = int * bool
end = struct
 type t = A | B
 let f = function A | B -> 0
end;;
[%%expect{|
Line _, characters 6-61:
  ......struct
   type t = A | B
   let f = function A | B -> 0
  end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t = A | B val f : t -> int end
       is not included in
         sig type t = int * bool end
       Type declarations do not match:
         type t = X.t = A | B
       is not included in
         type t = int * bool
|}];;


(* PR#7838 *)

module Make (X : sig val f : [ `A ] -> unit end) = struct
 let make f1 f2 arg = match arg with `A -> f1 arg; f2 arg
 let f = make X.f (fun _ -> ())
end;;
[%%expect{|
module Make :
  functor (X : sig val f : [ `A ] -> unit end) ->
    sig
      val make : (([< `A ] as 'a) -> 'b) -> ('a -> 'c) -> 'a -> 'c
      val f : [ `A ] -> unit
    end
|}]
