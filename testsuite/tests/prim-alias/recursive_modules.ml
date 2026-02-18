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
Line 6, characters 6-7:
6 | end = G
          ^
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: G -> G.
       There are no safe modules in this cycle (see manual section 12.2).
Line 5, characters 2-36:
5 |   external magic : a -> b = identity
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Module "G" defines an unsafe primitive alias, "magic" .
  The type of the aliased primitive is less general than that of its alias.
|}]

type e
external id: e -> e = "%identity"
type f = int -> int

[%%expect{|
type e
external id : e -> e = "%identity"
type f = int -> int
|}]

module rec A: sig
  type t
  external id: t -> f = id
end = struct
  type t = A
  external id = A.id
end

[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = A
6 |   external id = A.id
7 | end
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: A -> A.
       There are no safe modules in this cycle (see manual section 12.2).
Line 3, characters 2-26:
3 |   external id: t -> f = id
      ^^^^^^^^^^^^^^^^^^^^^^^^
  Module "A" defines an unsafe primitive alias, "id" .
  The type of the aliased primitive is less general than that of its alias.
|}]

module rec A: sig
  type t
  external id: t -> f = id
end = struct
  type t = A
  external id: t -> f = A.id
end

[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = A
6 |   external id: t -> f = A.id
7 | end
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: A -> A.
       There are no safe modules in this cycle (see manual section 12.2).
Line 3, characters 2-26:
3 |   external id: t -> f = id
      ^^^^^^^^^^^^^^^^^^^^^^^^
  Module "A" defines an unsafe primitive alias, "id" .
  The type of the aliased primitive is less general than that of its alias.
|}]

(* Certain types of recursion are safe. *)
module rec A : sig
  external identity = identity
end = A

[%%expect{|
module rec A : sig external identity : 'a -> 'a = "%identity" end
|}]

(* However, we still reject some things that could plausibly be compiled. *)
module rec A : sig
  type t
  external id : t -> t = id
end = struct
  type t = e
  external id = A.id
end

[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = e
6 |   external id = A.id
7 | end
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: A -> A.
       There are no safe modules in this cycle (see manual section 12.2).
Line 3, characters 2-27:
3 |   external id : t -> t = id
      ^^^^^^^^^^^^^^^^^^^^^^^^^
  Module "A" defines an unsafe primitive alias, "id" .
  The type of the aliased primitive is less general than that of its alias.
|}]
