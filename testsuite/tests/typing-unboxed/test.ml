(* TEST
   flags = "-strict-sequence"
   * expect
*)
external a : (int [@untagged]) -> unit = "%identity" "%identity"
external b : (int32 [@unboxed]) -> unit = "%identity" "%identity"
external c : (int64 [@unboxed]) -> unit = "%identity" "%identity"
external d : (nativeint [@unboxed]) -> unit = "%identity" "%identity"
external e : (float [@unboxed]) -> unit = "%identity" "%identity"

type t = private int

external f : (t [@untagged]) -> unit = "%identity" "%identity"

module M : sig
  external a : int -> (int [@untagged]) = "%identity" "%identity"
  external b : (int [@untagged]) -> int = "%identity" "%identity"
end = struct
  external a : int -> (int [@untagged]) = "%identity" "%identity"
  external b : (int [@untagged]) -> int = "%identity" "%identity"
end;;

[%%expect{|
external a : (int [@untagged]) -> unit = "%identity" "%identity"
external b : (int32 [@unboxed]) -> unit = "%identity" "%identity"
external c : (int64 [@unboxed]) -> unit = "%identity" "%identity"
external d : (nativeint [@unboxed]) -> unit = "%identity" "%identity"
external e : (float [@unboxed]) -> unit = "%identity" "%identity"
type t = private int
external f : (t [@untagged]) -> unit = "%identity" "%identity"
module M :
  sig
    external a : int -> (int [@untagged]) = "%identity" "%identity"
    external b : (int [@untagged]) -> int = "%identity" "%identity"
  end
|}]

module Global_attributes = struct
  [@@@ocaml.warning "-3"]

  external a : float -> float = "%identity" "noalloc" "%identity" "float"
  external b : float -> float = "%identity" "noalloc" "%identity"
  external c : float -> float = "%identity" "%identity" "float"
  external d : float -> float = "%identity" "noalloc"
  external e : float -> float = "%identity"

  (* Should output a warning: no native implementation provided *)
  external f : (int32 [@unboxed]) -> (int32 [@unboxed]) = "%identity" "noalloc"
  external g : int32 -> int32 = "%identity" "%identity" [@@unboxed] [@@noalloc]

  external h : (int [@untagged]) -> (int [@untagged]) = "%identity" "%identity" "noalloc"
  external i : int -> int = "%identity" "%identity" [@@untagged] [@@noalloc]
end;;

[%%expect{|
Line 11, characters 2-79:
    external f : (int32 [@unboxed]) -> (int32 [@unboxed]) = "%identity" "noalloc"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
when attributes [@untagged] or [@unboxed] are present.
|}]

module Old_style_warning = struct
  [@@@ocaml.warning "+3"]
  external a : float -> float = "%identity" "noalloc" "%identity" "float"
  external b : float -> float = "%identity" "noalloc" "%identity"
  external c : float -> float = "%identity" "%identity" "float"
  external d : float -> float = "%identity" "noalloc"
  external e : float -> float = "%identity" "float"
end;;
[%%expect{|
Line 3, characters 2-73:
    external a : float -> float = "%identity" "noalloc" "%identity" "float"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 3: deprecated: [@@unboxed] + [@@noalloc] should be used
instead of "float"
Line 4, characters 2-65:
    external b : float -> float = "%identity" "noalloc" "%identity"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 3: deprecated: [@@noalloc] should be used instead of "noalloc"
Line 5, characters 2-63:
    external c : float -> float = "%identity" "%identity" "float"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 3: deprecated: [@@unboxed] + [@@noalloc] should be used
instead of "float"
Line 6, characters 2-53:
    external d : float -> float = "%identity" "noalloc"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 3: deprecated: [@@noalloc] should be used instead of "noalloc"
module Old_style_warning :
  sig
    external a : float -> float = "%identity" "%identity" [@@unboxed]
      [@@noalloc]
    external b : float -> float = "%identity" "%identity" [@@noalloc]
    external c : float -> float = "%identity" "%identity" [@@unboxed]
      [@@noalloc]
    external d : float -> float = "%identity" [@@noalloc]
    external e : float -> float = "%identity" "float"
  end
|}]

(* Bad: attributes not reported in the interface *)

module Bad1 : sig
  external f : int -> int = "%identity" "%identity"
end = struct
  external f : int -> (int [@untagged]) = "%identity" "%identity"
end;;

[%%expect{|
Line 3, characters 6-82:
  ......struct
    external f : int -> (int [@untagged]) = "%identity" "%identity"
  end..
Error: Signature mismatch:
       Modules do not match:
         sig
           external f : int -> (int [@untagged]) = "%identity" "%identity"
         end
       is not included in
         sig external f : int -> int = "%identity" "%identity" end
       Values do not match:
         external f : int -> (int [@untagged]) = "%identity" "%identity"
       is not included in
         external f : int -> int = "%identity" "%identity"
|}]

module Bad2 : sig
  external f : int -> int = "%identity" "%identity"
end = struct
  external f : (int [@untagged]) -> int = "%identity" "%identity"
end;;

[%%expect{|
Line 3, characters 6-82:
  ......struct
    external f : (int [@untagged]) -> int = "%identity" "%identity"
  end..
Error: Signature mismatch:
       Modules do not match:
         sig
           external f : (int [@untagged]) -> int = "%identity" "%identity"
         end
       is not included in
         sig external f : int -> int = "%identity" "%identity" end
       Values do not match:
         external f : (int [@untagged]) -> int = "%identity" "%identity"
       is not included in
         external f : int -> int = "%identity" "%identity"
|}]

module Bad3 : sig
  external f : float -> float = "%identity" "%identity"
end = struct
  external f : float -> (float [@unboxed]) = "%identity" "%identity"
end;;

[%%expect{|
Line 3, characters 6-85:
  ......struct
    external f : float -> (float [@unboxed]) = "%identity" "%identity"
  end..
Error: Signature mismatch:
       Modules do not match:
         sig
           external f : float -> (float [@unboxed]) = "%identity" "%identity"
         end
       is not included in
         sig external f : float -> float = "%identity" "%identity" end
       Values do not match:
         external f : float -> (float [@unboxed]) = "%identity" "%identity"
       is not included in
         external f : float -> float = "%identity" "%identity"
|}]

module Bad4 : sig
  external f : float -> float = "%identity" "%identity"
end = struct
  external f : (float [@unboxed]) -> float = "%identity" "%identity"
end;;

[%%expect{|
Line 3, characters 6-85:
  ......struct
    external f : (float [@unboxed]) -> float = "%identity" "%identity"
  end..
Error: Signature mismatch:
       Modules do not match:
         sig
           external f : (float [@unboxed]) -> float = "%identity" "%identity"
         end
       is not included in
         sig external f : float -> float = "%identity" "%identity" end
       Values do not match:
         external f : (float [@unboxed]) -> float = "%identity" "%identity"
       is not included in
         external f : float -> float = "%identity" "%identity"
|}]

(* Bad: attributes in the interface but not in the implementation *)

module Bad5 : sig
  external f : int -> (int [@untagged]) = "%identity" "%identity"
end = struct
  external f : int -> int = "%identity" "%identity"
end;;

[%%expect{|
Line 3, characters 6-68:
  ......struct
    external f : int -> int = "%identity" "%identity"
  end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "%identity" "%identity" end
       is not included in
         sig
           external f : int -> (int [@untagged]) = "%identity" "%identity"
         end
       Values do not match:
         external f : int -> int = "%identity" "%identity"
       is not included in
         external f : int -> (int [@untagged]) = "%identity" "%identity"
|}]

module Bad6 : sig
  external f : (int [@untagged]) -> int = "%identity" "%identity"
end = struct
  external f : int -> int = "%identity" "%identity"
end;;

[%%expect{|
Line 3, characters 6-68:
  ......struct
    external f : int -> int = "%identity" "%identity"
  end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : int -> int = "%identity" "%identity" end
       is not included in
         sig
           external f : (int [@untagged]) -> int = "%identity" "%identity"
         end
       Values do not match:
         external f : int -> int = "%identity" "%identity"
       is not included in
         external f : (int [@untagged]) -> int = "%identity" "%identity"
|}]

module Bad7 : sig
  external f : float -> (float [@unboxed]) = "%identity" "%identity"
end = struct
  external f : float -> float = "%identity" "%identity"
end;;

[%%expect{|
Line 3, characters 6-72:
  ......struct
    external f : float -> float = "%identity" "%identity"
  end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : float -> float = "%identity" "%identity" end
       is not included in
         sig
           external f : float -> (float [@unboxed]) = "%identity" "%identity"
         end
       Values do not match:
         external f : float -> float = "%identity" "%identity"
       is not included in
         external f : float -> (float [@unboxed]) = "%identity" "%identity"
|}]

module Bad8 : sig
  external f : (float [@unboxed]) -> float = "%identity" "%identity"
end = struct
  external f : float -> float = "%identity" "%identity"
end;;

[%%expect{|
Line 3, characters 6-72:
  ......struct
    external f : float -> float = "%identity" "%identity"
  end..
Error: Signature mismatch:
       Modules do not match:
         sig external f : float -> float = "%identity" "%identity" end
       is not included in
         sig
           external f : (float [@unboxed]) -> float = "%identity" "%identity"
         end
       Values do not match:
         external f : float -> float = "%identity" "%identity"
       is not included in
         external f : (float [@unboxed]) -> float = "%identity" "%identity"
|}]

(* Bad: unboxed or untagged with the wrong type *)

external g : (float [@untagged]) -> float = "%identity" "%identity";;
[%%expect{|
Line 1, characters 14-19:
  external g : (float [@untagged]) -> float = "%identity" "%identity";;
                ^^^^^
Error: Don't know how to untag this type. Only int can be untagged.
|}]
external h : (int [@unboxed]) -> float = "%identity" "%identity";;
[%%expect{|
Line 1, characters 14-17:
  external h : (int [@unboxed]) -> float = "%identity" "%identity";;
                ^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64 and nativeint can be unboxed.
|}]

(* Bad: unboxing the function type *)
external i : int -> float [@unboxed] = "%identity" "%identity";;
[%%expect{|
Line 1, characters 13-25:
  external i : int -> float [@unboxed] = "%identity" "%identity";;
               ^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64 and nativeint can be unboxed.
|}]

(* Bad: unboxing a "deep" sub-type. *)
external j : int -> (float [@unboxed]) * float = "%identity" "%identity";;
[%%expect{|
Line 1, characters 21-26:
  external j : int -> (float [@unboxed]) * float = "%identity" "%identity";;
                       ^^^^^
Error: The attribute '@unboxed' should be attached to
       a direct argument or result of the primitive,
       it should not occur deeply into its type.
|}]

(* This should be rejected, but it is quite complicated to do
   in the current state of things *)

external k : int -> (float [@unboxd]) = "%identity" "%identity";;
[%%expect{|
external k : int -> float = "%identity" "%identity"
|}]

(* Bad: old style annotations + new style attributes *)

external l : float -> float = "%identity" "%identity" "float" [@@unboxed];;
[%%expect{|
Line 1, characters 0-73:
  external l : float -> float = "%identity" "%identity" "float" [@@unboxed];;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with [@unboxed]/[@untagged].
|}]
external m : (float [@unboxed]) -> float = "%identity" "%identity" "float";;
[%%expect{|
Line 1, characters 0-74:
  external m : (float [@unboxed]) -> float = "%identity" "%identity" "float";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with [@unboxed]/[@untagged].
|}]
external n : float -> float = "%identity" "noalloc" [@@noalloc];;
[%%expect{|
Line 1, characters 0-63:
  external n : float -> float = "%identity" "noalloc" [@@noalloc];;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "noalloc" in conjunction with [@@noalloc].
|}]

(* Warnings: unboxed / untagged without any native implementation *)
external o : (float[@unboxed]) -> float = "%identity";;
[%%expect{|
Line 1, characters 0-53:
  external o : (float[@unboxed]) -> float = "%identity";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
when attributes [@untagged] or [@unboxed] are present.
|}]
external p : float -> (float[@unboxed]) = "%identity";;
[%%expect{|
Line 1, characters 0-53:
  external p : float -> (float[@unboxed]) = "%identity";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
when attributes [@untagged] or [@unboxed] are present.
|}]
external q : (int[@untagged]) -> float = "%identity";;
[%%expect{|
Line 1, characters 0-52:
  external q : (int[@untagged]) -> float = "%identity";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
when attributes [@untagged] or [@unboxed] are present.
|}]
external r : int -> (int[@untagged]) = "%identity";;
[%%expect{|
Line 1, characters 0-50:
  external r : int -> (int[@untagged]) = "%identity";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
when attributes [@untagged] or [@unboxed] are present.
|}]
external s : int -> int = "%identity" [@@untagged];;
[%%expect{|
Line 1, characters 0-50:
  external s : int -> int = "%identity" [@@untagged];;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@The native code version of the primitive is mandatory
when attributes [@untagged] or [@unboxed] are present.
|}]
external t : float -> float = "%identity" [@@unboxed];;
[%%expect{|
Line 1, characters 0-53:
  external t : float -> float = "%identity" [@@unboxed];;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
  external id : i -> i = "%identity";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 61: This primitive declaration uses type i, which is unannotated and
unboxable. The representation of such types may change in future
versions. You should annotate the declaration of i with [@@boxed]
or [@@unboxed].
external id : i -> i = "%identity"
|}];;

type i = I of int
type j = J of int
external id : i -> j = "%identity";;
[%%expect{|
type i = I of int
type j = J of int
Line 3, characters 0-34:
  external id : i -> j = "%identity";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 61: This primitive declaration uses type i, which is unannotated and
unboxable. The representation of such types may change in future
versions. You should annotate the declaration of i with [@@boxed]
or [@@unboxed].
Line 3, characters 0-34:
  external id : i -> j = "%identity";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 61: This primitive declaration uses type j, which is unannotated and
unboxable. The representation of such types may change in future
versions. You should annotate the declaration of j with [@@boxed]
or [@@unboxed].
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
