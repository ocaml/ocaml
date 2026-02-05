(* TEST
 flat-float-array;
 expect;
*)


(* Check interaction of module-dependent functions with separability. *)
module type T =  sig type !'a t end
type 'a t = 'b constraint 'a = (module M:T) -> 'b M.t
type any = Any: 'a t -> any [@@unboxed]

[%%expect{|
module type T = sig type !'a t end
type 'a t = 'b constraint 'a = (module M : T) -> 'b M.t
Line 3, characters 0-39:
3 | type any = Any: 'a t -> any [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of an unnamed existential variable.
       You should annotate it with "[@@ocaml.boxed]".
|}]
