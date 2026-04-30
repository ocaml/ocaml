(* TEST
   expect;
*)

(* Cannot define a self-referential primitive. *)
module rec A : sig
  external p = A.p
end = struct
  external p = A.p
end

[%%expect {|
Line 2, characters 15-18:
2 |   external p = A.p
                   ^^^
Error: Unbound value "A.p"
|}]

(* Cannot define mutually-recursive primitives. *)
module rec B : sig
  external p = C.p
end = struct
  external p = C.p
end

and C : sig
  external p = B.p
end = struct
  external p = B.p
end

[%%expect {|
Line 2, characters 15-18:
2 |   external p = C.p
                   ^^^
Error: Unbound value "C.p"
|}]

(* Cannot refer to primitives from other mutually-recursive modules. *)
module rec D : sig
  external p = E.p
end = struct
  external p = Obj.magic
end

and E : sig
  external p = Obj.magic
end = struct
  external p = D.p
end

[%%expect {|
Line 2, characters 15-18:
2 |   external p = E.p
                   ^^^
Error: Unbound value "E.p"
|}]

external identity : 'a -> 'a = Obj.magic

[%%expect {|
external identity : 'a -> 'a = "%identity"
|}]

(* Can't use a recursive module to generalize a primitive's type. *)
module rec F : sig
  external magic : 'a -> 'b = identity
end = F

[%%expect {|
Line 2, characters 19-27:
2 |   external magic : 'a -> 'b = identity
                       ^^^^^^^^
Error: The type of this alias does not match that of the aliased primitive.
       Type "'a -> 'b" is not compatible with type "'a0 -> 'a0"
|}]

(* As above, but with concrete types. *)
module rec G : sig
  type a
  type b

  external magic : a -> b = identity
end = G

[%%expect {|
Line 5, characters 19-25:
5 |   external magic : a -> b = identity
                       ^^^^^^
Error: The type of this alias does not match that of the aliased primitive.
       Type "a -> b" is not compatible with type "'a -> 'a"
       Type "b" is not compatible with type "'a" = "a"
|}]
