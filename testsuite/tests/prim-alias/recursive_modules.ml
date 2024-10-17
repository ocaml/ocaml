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
Line 2, characters 2-18:
2 |   external p = A.p
      ^^^^^^^^^^^^^^^^
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
Line 2, characters 2-18:
2 |   external p = C.p
      ^^^^^^^^^^^^^^^^
Error: Unbound value "C.p"
|}]

(* Can refer to primitives from other mutually-recursive modules. *)
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
module rec D : sig external p : 'a -> 'b = "%identity" end
and E : sig external p : 'a -> 'b = "%identity" end
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

external int_to_int : int -> int = identity

[%%expect {|
external int_to_int : int -> int = "%identity"
|}]

(* As above, but with concrete types. *)
module rec G : sig
  external int_to_string : int -> string = int_to_int
end = G

(* "Less general" is somewhat misleading, but it is good to error here. *)
[%%expect {|
Line 3, characters 6-7:
3 | end = G
          ^
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: G -> G.
       There are no safe modules in this cycle (see manual section 12.2).
Line 2, characters 2-53:
2 |   external int_to_string : int -> string = int_to_int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Module "G" defines an unsafe primitive alias, "int_to_string" .
  The type of the aliased primitive is less general than that of its alias.
|}]
