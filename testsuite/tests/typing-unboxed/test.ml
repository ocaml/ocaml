(* TEST
   flags = "-strict-sequence"
   * expect
*)
external a : (int [@untagged]) -> unit = "a" "a_nat"
external b : (int32 [@unboxed]) -> unit = "b" "b_nat"
external c : (int64 [@unboxed]) -> unit = "c" "c_nat"
external d : (nativeint [@unboxed]) -> unit = "d" "d_nat"
external e : (float [@unboxed]) -> unit = "e" "e_nat"

type t = private int

external f : (t [@untagged]) -> unit = "f" "f_nat"

module M : sig
  external a : int -> (int [@untagged]) = "a" "a_nat"
  external b : (int [@untagged]) -> int = "b" "b_nat"
end = struct
  external a : int -> (int [@untagged]) = "a" "a_nat"
  external b : (int [@untagged]) -> int = "b" "b_nat"
end;;

[%%expect{|
external a : (int [@untagged]) -> unit = "a" "a_nat"
external b : (int32 [@unboxed]) -> unit = "b" "b_nat"
external c : (int64 [@unboxed]) -> unit = "c" "c_nat"
external d : (nativeint [@unboxed]) -> unit = "d" "d_nat"
external e : (float [@unboxed]) -> unit = "e" "e_nat"
type t = private int
external f : (t [@untagged]) -> unit = "f" "f_nat"
module M :
  sig
    external a : int -> (int [@untagged]) = "a" "a_nat"
    external b : (int [@untagged]) -> int = "b" "b_nat"
  end
|}]

module Global_attributes = struct
  [@@@ocaml.alert "-deprecated"]

  external a : float -> float = "a" "noalloc" "a_nat" "float"
  external b : float -> float = "b" "noalloc" "b_nat"
  external c : float -> float = "c" "c_nat" "float"
  external d : float -> float = "d" "noalloc"
  external e : float -> float = "e"

  (* Should output a warning: no native implementation provided *)
  external f : (int32 [@unboxed]) -> (int32 [@unboxed]) = "f" "noalloc"
  external g : int32 -> int32 = "g" "g_nat" [@@unboxed] [@@noalloc]

  external h : (int [@untagged]) -> (int [@untagged]) = "h" "h_nat" "noalloc"
  external i : int -> int = "i" "i_nat" [@@untagged] [@@noalloc]
end;;

[%%expect{|
Line 11, characters 2-71:
11 |   external f : (int32 [@unboxed]) -> (int32 [@unboxed]) = "f" "noalloc"
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
       when attributes [@untagged] or [@unboxed] are present.
|}]

module Old_style_warning = struct
  [@@@ocaml.warning "+3"]
  external a : float -> float = "a" "noalloc" "a_nat" "float"
  external b : float -> float = "b" "noalloc" "b_nat"
  external c : float -> float = "c" "c_nat" "float"
  external d : float -> float = "d" "noalloc"
  external e : float -> float = "c" "float"
end;;
[%%expect{|
Line 3, characters 2-61:
3 |   external a : float -> float = "a" "noalloc" "a_nat" "float"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert deprecated: [@@unboxed] + [@@noalloc] should be used
instead of "float"
Line 4, characters 2-53:
4 |   external b : float -> float = "b" "noalloc" "b_nat"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert deprecated: [@@noalloc] should be used instead of "noalloc"
Line 5, characters 2-51:
5 |   external c : float -> float = "c" "c_nat" "float"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert deprecated: [@@unboxed] + [@@noalloc] should be used
instead of "float"
Line 6, characters 2-45:
6 |   external d : float -> float = "d" "noalloc"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert deprecated: [@@noalloc] should be used instead of "noalloc"
module Old_style_warning :
  sig
    external a : float -> float = "a" "a_nat" [@@unboxed] [@@noalloc]
    external b : float -> float = "b" "b_nat" [@@noalloc]
    external c : float -> float = "c" "c_nat" [@@unboxed] [@@noalloc]
    external d : float -> float = "d" [@@noalloc]
    external e : float -> float = "c" "float"
  end
|}]

(* Bad: attributes not reported in the interface *)

module Bad1 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  external f : int -> (int [@untagged]) = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> (int [@untagged]) = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> (int [@untagged]) = "f" "f_nat" end
       is not included in
         sig external f : int -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> (int [@untagged]) = "f" "f_nat"
       is not included in
         external f : int -> int = "f" "f_nat"
       The two primitives' results have different representations
|}]

module Bad2 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  external f : (int [@untagged]) -> int = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : (int [@untagged]) -> int = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : (int [@untagged]) -> int = "f" "f_nat" end
       is not included in
         sig external f : int -> int = "f" "f_nat" end
       Values do not match:
         external f : (int [@untagged]) -> int = "f" "f_nat"
       is not included in
         external f : int -> int = "f" "f_nat"
       The two primitives' 1st arguments have different representations
|}]

module Bad3 : sig
  external f : int -> int = "a" "a_nat"
end = struct
  external f : (int [@untagged]) -> int = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : (int [@untagged]) -> int = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : (int [@untagged]) -> int = "f" "f_nat" end
       is not included in
         sig external f : int -> int = "a" "a_nat" end
       Values do not match:
         external f : (int [@untagged]) -> int = "f" "f_nat"
       is not included in
         external f : int -> int = "a" "a_nat"
       The names of the primitives are not the same
|}]

module Bad4 : sig
  external f : float -> float = "f" "f_nat"
end = struct
  external f : float -> (float [@unboxed]) = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : float -> (float [@unboxed]) = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : float -> (float [@unboxed]) = "f" "f_nat" end
       is not included in
         sig external f : float -> float = "f" "f_nat" end
       Values do not match:
         external f : float -> (float [@unboxed]) = "f" "f_nat"
       is not included in
         external f : float -> float = "f" "f_nat"
       The two primitives' results have different representations
|}]

module Bad5 : sig
  external f : float -> float = "f" "f_nat"
end = struct
  external f : (float [@unboxed]) -> float = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : (float [@unboxed]) -> float = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : (float [@unboxed]) -> float = "f" "f_nat" end
       is not included in
         sig external f : float -> float = "f" "f_nat" end
       Values do not match:
         external f : (float [@unboxed]) -> float = "f" "f_nat"
       is not included in
         external f : float -> float = "f" "f_nat"
       The two primitives' 1st arguments have different representations
|}]

module Bad6 : sig
  external f : float -> float = "a" "a_nat"
end = struct
  external f : (float [@unboxed]) -> float = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : (float [@unboxed]) -> float = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : (float [@unboxed]) -> float = "f" "f_nat" end
       is not included in
         sig external f : float -> float = "a" "a_nat" end
       Values do not match:
         external f : (float [@unboxed]) -> float = "f" "f_nat"
       is not included in
         external f : float -> float = "a" "a_nat"
       The names of the primitives are not the same
|}]

module Bad7 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  external f : int -> int = "f" "f_nat" [@@noalloc]
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> int = "f" "f_nat" [@@noalloc]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "f" "f_nat" [@@noalloc] end
       is not included in
         sig external f : int -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> int = "f" "f_nat" [@@noalloc]
       is not included in
         external f : int -> int = "f" "f_nat"
       The first primitive is [@@noalloc] but the second is not
|}]

(* Bad: attributes in the interface but not in the implementation *)

module Bad8 : sig
  external f : int -> (int [@untagged]) = "f" "f_nat"
end = struct
  external f : int -> int = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> int = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "f" "f_nat" end
       is not included in
         sig external f : int -> (int [@untagged]) = "f" "f_nat" end
       Values do not match:
         external f : int -> int = "f" "f_nat"
       is not included in
         external f : int -> (int [@untagged]) = "f" "f_nat"
       The two primitives' results have different representations
|}]

module Bad9 : sig
  external f : (int [@untagged]) -> int = "f" "f_nat"
end = struct
  external f : int -> int = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> int = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "f" "f_nat" end
       is not included in
         sig external f : (int [@untagged]) -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> int = "f" "f_nat"
       is not included in
         external f : (int [@untagged]) -> int = "f" "f_nat"
       The two primitives' 1st arguments have different representations
|}]

module Bad10 : sig
  external f : (int [@untagged]) -> int = "f" "f_nat"
end = struct
  external f : int -> int = "a" "a_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> int = "a" "a_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "a" "a_nat" end
       is not included in
         sig external f : (int [@untagged]) -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> int = "a" "a_nat"
       is not included in
         external f : (int [@untagged]) -> int = "f" "f_nat"
       The names of the primitives are not the same
|}]

module Bad11 : sig
  external f : float -> (float [@unboxed]) = "f" "f_nat"
end = struct
  external f : float -> float = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : float -> float = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : float -> float = "f" "f_nat" end
       is not included in
         sig external f : float -> (float [@unboxed]) = "f" "f_nat" end
       Values do not match:
         external f : float -> float = "f" "f_nat"
       is not included in
         external f : float -> (float [@unboxed]) = "f" "f_nat"
       The two primitives' results have different representations
|}]

module Bad12 : sig
  external f : (float [@unboxed]) -> float = "f" "f_nat"
end = struct
  external f : float -> float = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : float -> float = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : float -> float = "f" "f_nat" end
       is not included in
         sig external f : (float [@unboxed]) -> float = "f" "f_nat" end
       Values do not match:
         external f : float -> float = "f" "f_nat"
       is not included in
         external f : (float [@unboxed]) -> float = "f" "f_nat"
       The two primitives' 1st arguments have different representations
|}]

module Bad13 : sig
  external f : (float [@unboxed]) -> float = "f" "f_nat"
end = struct
  external f : float -> float = "a" "a_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : float -> float = "a" "a_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : float -> float = "a" "a_nat" end
       is not included in
         sig external f : (float [@unboxed]) -> float = "f" "f_nat" end
       Values do not match:
         external f : float -> float = "a" "a_nat"
       is not included in
         external f : (float [@unboxed]) -> float = "f" "f_nat"
       The names of the primitives are not the same
|}]

module Bad14 : sig
  external f : int -> int = "f" "f_nat" [@@noalloc]
end = struct
  external f : int -> int = "f" "f_nat"
end;;

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> int = "f" "f_nat"
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "f" "f_nat" end
       is not included in
         sig external f : int -> int = "f" "f_nat" [@@noalloc] end
       Values do not match:
         external f : int -> int = "f" "f_nat"
       is not included in
         external f : int -> int = "f" "f_nat" [@@noalloc]
       The second primitive is [@@noalloc] but the first is not
|}]

(* Bad: claiming something is a primitive when it isn't *)

module Bad15 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  let f x = x + 1
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = x + 1
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int end
       is not included in
         sig external f : int -> int = "f" "f_nat" end
       Values do not match:
         val f : int -> int
       is not included in
         external f : int -> int = "f" "f_nat"
       The implementation is not a primitive.
|}]

(* Good: not claiming something is a primitive when it is *)

module Good16 : sig
  val f : int -> int
end = struct
  external f : int -> int = "f" "f_nat"
end
(* The expected error here is that "f" isn't defined -- that means typechecking
   succeeded *)

[%%expect{|
Line 1:
Error: The external function `f' is not available
|}]

(* Bad: mismatched names and native names *)

module Bad17 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  external f : int -> int = "gg" "f_nat"
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> int = "gg" "f_nat"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "gg" "f_nat" end
       is not included in
         sig external f : int -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> int = "gg" "f_nat"
       is not included in
         external f : int -> int = "f" "f_nat"
       The names of the primitives are not the same
|}]

module Bad18 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  external f : int -> int = "f" "gg_nat"
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> int = "f" "gg_nat"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "f" "gg_nat" end
       is not included in
         sig external f : int -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> int = "f" "gg_nat"
       is not included in
         external f : int -> int = "f" "f_nat"
       The native names of the primitives are not the same
|}]

module Bad19 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  external f : int -> int = "gg" "gg_nat"
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> int = "gg" "gg_nat"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "gg" "gg_nat" end
       is not included in
         sig external f : int -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> int = "gg" "gg_nat"
       is not included in
         external f : int -> int = "f" "f_nat"
       The names of the primitives are not the same
|}]

(* Bad: mismatched arities *)

(* NB: The compiler checks primitive arities *syntactically*, based on the
   number of arrows it sees.  Thus, hiding function types behind type synonyms
   will produce an error about the primitive arities not matching, even when the
   types agree. *)

module Bad20 : sig
  type int_int := int -> int
  external f : int -> int_int = "f" "f_nat"
end = struct
  external f : int -> int -> int = "f" "f_nat"
end

[%%expect{|
Lines 4-6, characters 6-3:
4 | ......struct
5 |   external f : int -> int -> int = "f" "f_nat"
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int -> int = "f" "f_nat" end
       is not included in
         sig external f : int -> int -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> int -> int = "f" "f_nat"
       is not included in
         external f : int -> int -> int = "f" "f_nat"
       The syntactic arities of these primitives were not the same.
       (They must have the same number of arrows present in the source.)
|}]

module Bad21 : sig
  external f : int -> int -> int = "f" "f_nat"
end = struct
  type int_int = int -> int
  external f : int -> int_int = "f" "f_nat"
end

[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   type int_int = int -> int
5 |   external f : int -> int_int = "f" "f_nat"
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type int_int = int -> int
           external f : int -> int_int = "f" "f_nat"
         end
       is not included in
         sig external f : int -> int -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> int_int = "f" "f_nat"
       is not included in
         external f : int -> int -> int = "f" "f_nat"
       The syntactic arities of these primitives were not the same.
       (They must have the same number of arrows present in the source.)
|}]

(* This will fail with a *type* error, instead of an arity mismatch *)
module Bad22 : sig
  external f : int -> int = "f" "f_nat"
end = struct
  external f : int -> int -> int = "f" "f_nat"
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external f : int -> int -> int = "f" "f_nat"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int -> int = "f" "f_nat" end
       is not included in
         sig external f : int -> int = "f" "f_nat" end
       Values do not match:
         external f : int -> int -> int = "f" "f_nat"
       is not included in
         external f : int -> int = "f" "f_nat"
       The type int -> int -> int is not compatible with the type int -> int
       Type int -> int is not compatible with type int
|}]

(* Bad: unboxed or untagged with the wrong type *)

external g : (float [@untagged]) -> float = "g" "g_nat";;
[%%expect{|
Line 1, characters 14-19:
1 | external g : (float [@untagged]) -> float = "g" "g_nat";;
                  ^^^^^
Error: Don't know how to untag this type. Only int can be untagged.
|}]
external h : (int [@unboxed]) -> float = "h" "h_nat";;
[%%expect{|
Line 1, characters 14-17:
1 | external h : (int [@unboxed]) -> float = "h" "h_nat";;
                  ^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64 and nativeint can be unboxed.
|}]

(* Bad: unboxing the function type *)
external i : int -> float [@unboxed] = "i" "i_nat";;
[%%expect{|
Line 1, characters 13-25:
1 | external i : int -> float [@unboxed] = "i" "i_nat";;
                 ^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64 and nativeint can be unboxed.
|}]

(* Bad: unboxing a "deep" sub-type. *)
external j : int -> (float [@unboxed]) * float = "j" "j_nat";;
[%%expect{|
Line 1, characters 21-26:
1 | external j : int -> (float [@unboxed]) * float = "j" "j_nat";;
                         ^^^^^
Error: The attribute '@unboxed' should be attached to
       a direct argument or result of the primitive,
       it should not occur deeply into its type.
|}]

(* This should be rejected, but it is quite complicated to do
   in the current state of things *)

external k : int -> (float [@unboxd]) = "k" "k_nat";;
[%%expect{|
external k : int -> float = "k" "k_nat"
|}]

(* Bad: old style annotations + new style attributes *)

external l : float -> float = "l" "l_nat" "float" [@@unboxed];;
[%%expect{|
Line 1, characters 0-61:
1 | external l : float -> float = "l" "l_nat" "float" [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with [@unboxed]/[@untagged].
|}]
external m : (float [@unboxed]) -> float = "m" "m_nat" "float";;
[%%expect{|
Line 1, characters 0-62:
1 | external m : (float [@unboxed]) -> float = "m" "m_nat" "float";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with [@unboxed]/[@untagged].
|}]
external n : float -> float = "n" "noalloc" [@@noalloc];;
[%%expect{|
Line 1, characters 0-55:
1 | external n : float -> float = "n" "noalloc" [@@noalloc];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "noalloc" in conjunction with [@@noalloc].
|}]

(* Warnings: unboxed / untagged without any native implementation *)
external o : (float[@unboxed]) -> float = "o";;
[%%expect{|
Line 1, characters 0-45:
1 | external o : (float[@unboxed]) -> float = "o";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
       when attributes [@untagged] or [@unboxed] are present.
|}]
external p : float -> (float[@unboxed]) = "p";;
[%%expect{|
Line 1, characters 0-45:
1 | external p : float -> (float[@unboxed]) = "p";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
       when attributes [@untagged] or [@unboxed] are present.
|}]
external q : (int[@untagged]) -> float = "q";;
[%%expect{|
Line 1, characters 0-44:
1 | external q : (int[@untagged]) -> float = "q";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
       when attributes [@untagged] or [@unboxed] are present.
|}]
external r : int -> (int[@untagged]) = "r";;
[%%expect{|
Line 1, characters 0-42:
1 | external r : int -> (int[@untagged]) = "r";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
       when attributes [@untagged] or [@unboxed] are present.
|}]
external s : int -> int = "s" [@@untagged];;
[%%expect{|
Line 1, characters 0-42:
1 | external s : int -> int = "s" [@@untagged];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
       when attributes [@untagged] or [@unboxed] are present.
|}]
external t : float -> float = "t" [@@unboxed];;
[%%expect{|
Line 1, characters 0-45:
1 | external t : float -> float = "t" [@@unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
       when attributes [@untagged] or [@unboxed] are present.
|}]

(* PR#7424 *)
type 'a b = B of 'a b b [@@unboxed];;
[%%expect{|
type 'a b = B of 'a b b [@@unboxed]
|}]


(* MPR#7828 *)
type i = I of int
external id : i -> i = "%identity";;
[%%expect{|
type i = I of int
Line 2, characters 0-34:
2 | external id : i -> i = "%identity";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 61 [unboxable-type-in-prim-decl]: This primitive declaration uses type i, whose representation
may be either boxed or unboxed. Without an annotation to indicate
which representation is intended, the boxed representation has been
selected by default. This default choice may change in future
versions of the compiler, breaking the primitive implementation.
You should explicitly annotate the declaration of i
with [@@boxed] or [@@unboxed], so that its external interface
remains stable in the future.
external id : i -> i = "%identity"
|}];;

type i = I of int
type j = J of int
external id : i -> j = "%identity";;
[%%expect{|
type i = I of int
type j = J of int
Line 3, characters 0-34:
3 | external id : i -> j = "%identity";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 61 [unboxable-type-in-prim-decl]: This primitive declaration uses type i, whose representation
may be either boxed or unboxed. Without an annotation to indicate
which representation is intended, the boxed representation has been
selected by default. This default choice may change in future
versions of the compiler, breaking the primitive implementation.
You should explicitly annotate the declaration of i
with [@@boxed] or [@@unboxed], so that its external interface
remains stable in the future.
Line 3, characters 0-34:
3 | external id : i -> j = "%identity";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 61 [unboxable-type-in-prim-decl]: This primitive declaration uses type j, whose representation
may be either boxed or unboxed. Without an annotation to indicate
which representation is intended, the boxed representation has been
selected by default. This default choice may change in future
versions of the compiler, breaking the primitive implementation.
You should explicitly annotate the declaration of j
with [@@boxed] or [@@unboxed], so that its external interface
remains stable in the future.
external id : i -> j = "%identity"
|}];;

type ib = I of int [@@boxed]
external idb : ib -> ib = "%identity";;
[%%expect{|
type ib = I of int
external idb : ib -> ib = "%identity"
|}];;

type iub = I of int [@@unboxed]
external idub : iub -> iub = "%identity";;
[%%expect{|
type iub = I of int [@@unboxed]
external idub : iub -> iub = "%identity"
|}];;

(* #9607: separability was not computed on with-constraints *)
module type T  = sig type 'k t end
module M : T with type 'k t = string = struct
  type 'k t = string
end
type t = T : 'k M.t -> t [@@unboxed]

[%%expect{|
module type T = sig type 'k t end
module M : sig type 'k t = string end
type t = T : 'k M.t -> t [@@unboxed]
|}];;
