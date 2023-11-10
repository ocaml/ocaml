(* TEST
 flags = "-w +A-22-27-32-60-67-70-71-72";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

module type TestAlertSig = sig
  type t1 = Foo1 [@alert foo "foo"] (* accepted *)

  val x : int [@alert foo "foo"] (* rejected *)

  type 'a t2 = 'a [@@alert foo "foo"] (* accepted *)

  val y : int [@@alert foo "foo"] (* accepted *)

  [@@@alert foo "foo"] (* rejected *)
end

module TestAlertStruct = struct
  let x = 5 [@alert foo "foo"] (* rejected *)

  let y = 10 [@@alert foo "foo"] (* rejected *)

  [@@@alert foo "foo"] (* rejected *)
end


module type TestBoxedSig = sig
  type t1 = { x : int [@boxed] } (* rejected *)

  val x : int [@boxed] (* rejected *)

  type t2 = { x : int } [@@boxed] (* accepted *)

  val y : int [@@boxed] (* rejected *)

  [@@@boxed] (* rejected *)
end

module TestBoxedStruct = struct
  let x = (42 [@boxed], 84) (* rejected *)

  let y = 10 [@@boxed] (* rejected *)

  [@@@boxed] (* rejected *)
end


module type TestDeprecatedSig = sig
  type t1 = Foo1 [@deprecated] (* accepted *)

  val x : int [@deprecated] (* rejected *)

  type 'a t2 = 'a [@@deprecated] (* accepted *)

  val y : int [@@deprecated] (* accepted *)

  [@@@deprecated] (* rejected *)
end

module TestDeprecatedStruct = struct
  let x = 5 [@deprecated] (* rejected *)

  let y = 10 [@@deprecated] (* rejected *)

  [@@@deprecated] (* rejected *)
end


module type TestDeprecatedMutableSig = sig
  type t1 = Foo1 [@deprecated_mutable] (* rejected *)

  val x : int [@deprecated_mutable] (* rejected *)

  type 'a t2 = 'a [@@deprecated_mutable] (* rejected *)

  type t3 = { mutable x : int [@deprecated_mutable] } (* accepted *)

  type t4 = { x : int [@deprecated_mutable] } (* rejected *)

  val y : int [@@deprecated_mutable] (* rejected *)

  [@@@deprecated_mutable] (* rejected *)
end

module TestDeprecatedMutableStruct = struct
  let x = 5 [@deprecated_mutable] (* rejected *)

  let y = 10 [@@deprecated_mutable] (* rejected *)

  [@@@deprecated_mutable] (* rejected *)
end


module type TestExplicitAritySig = sig
  type t1 = Foo1 of int * int [@explicit_arity] (* rejected *)

  val x : int [@explicit_arity] (* rejected *)

  type 'a t2 = 'a [@explicit_arity] (* rejected *)

  val y : int [@@explicit_arity] (* rejected *)

  [@@@explicit_arity] (* rejected *)
end

module TestExplicitArityStruct = struct
  let x = 5 [@explicit_arity] (* rejected *)

  let y = 10 [@@explicit_arity] (* rejected *)

  [@@@explicit_arity] (* rejected *)
end


module type TestImmediateSig = sig
  type t1 = int [@immediate] (* rejected *)
  type t2 = int [@@immediate] (* accepted *)
  type t3 [@@immediate64] (* accepted *)

  val x : int [@immediate] (* rejected *)
  val x : int [@@immediate] (* rejected *)

  [@@@immediate] (* rejected *)
  [@@@immediate64] (* rejected *)
end

module TestImmediateStruct = struct
  let x = (4 [@immediate], 42 [@immediate64]) (* rejected *)
  let y = (4, 42) [@@immediate] (* rejected *)
  let z = (4, 42) [@@immediate64] (* rejected *)

  type t1 = int [@immediate] (* rejected *)
  type t2 = int [@@immediate] (* accepted *)
  type t3 [@@immediate64] (* accepted *)

  [@@@immediate] (* rejected *)
  [@@@immediate64] (* rejected *)
end


module type TestInlineSig = sig
  type t1 = int -> int [@inline] (* rejected *)
  type t2 = int -> int [@@inline] (* rejected *)
  type t3 = int -> int [@inlined] (* rejected *)
  type t4 = int -> int [@@inlined] (* rejected *)

  val f1 : int -> int [@inline] (* rejected *)
  val f2 : int -> int [@@inline] (* rejected *)
  val f3 : int -> int [@inlined] (* rejected *)
  val f4 : int -> int [@@inlined] (* rejected *)

  module type F = functor (X : sig end) -> sig end [@inline] (* rejected *)
  module type G = functor (X : sig end) -> sig end [@@inline] (* rejected *)

  [@@@inline] (* rejected *)
  [@@@inlined] (* rejected *)
end

module TestInlineStruct = struct
  let h x = x [@inline] (* rejected *)
  let h x = x [@ocaml.inline] (* rejected *)

  let i x = x [@inlined] (* rejected *)
  let j x = x [@ocaml.inlined] (* rejected *)
  let k x = (h [@inlined]) x (* accepted *)
  let k' x = (h [@ocaml.inlined]) x (* accepted *)
  let l x = h x [@inlined] (* rejected *)

  module type E = sig end

  module A(E:E) = struct end [@@inline] (* accepted *)
  module A'(E:E) = struct end [@@ocaml.inline] (* accepted *)
  module B = ((functor (E:E) -> struct end) [@inline]) (* accepted *)
  module B' = ((functor (E:E) -> struct end) [@ocaml.inline]) (* accepted *)
  module C = struct end [@@inline] (* rejected *)
  module C' = struct end [@@ocaml.inline] (* rejected *)
  module D = struct end [@@inlined] (* rejected *)
  module D' = struct end [@@ocaml.inlined] (* rejected *)

  module F = (A [@inlined])(struct end) (* accepted *)
  module F' = (A [@ocaml.inlined])(struct end) (* accepted *)
  module G = (A [@inline])(struct end) (* rejected *)
  module G' = (A [@ocaml.inline])(struct end) (* rejected *)

  module H = Set.Make [@inlined] (Int32) (* accepted *) (* GPR#1808 *)

  module I = Set.Make [@inlined] (* rejected *)
  module I' = Set.Make [@ocaml.inlined] (* rejected *)

  module J = Set.Make [@@inlined] (* rejected *)
  module J' = Set.Make [@@ocaml.inlined] (* rejected *)
end


module type TestNoallocSig = sig
  type 'a t1 = 'a [@@noalloc] (* rejected *)
  type s1 = Foo1 [@noalloc] (* rejected *)
  val x : int64 [@@noalloc] (* rejected *)

  external y : (int64 [@noalloc]) -> (int64 [@noalloc]) = "x" (* rejected *)
  external z : int64 -> int64 = "x" [@@noalloc] (* accepted *)
end

module TestNoallocStruct = struct
  type 'a t1 = 'a [@@noalloc] (* rejected *)
  type s1 = Foo1 [@noalloc] (* rejected *)
  let x : int64 = 42L [@@noalloc] (* rejected *)

  external y : (int64 [@noalloc]) -> (int64 [@noalloc]) = "x" (* rejected *)
  external z : int64 -> int64 = "x" [@@noalloc] (* accepted *)
end


module type TestPpwarningSig = sig
  type 'a t1 = 'a [@@ppwarning "foo01"] (* accepted *)
  type s1 = Foo1 [@ppwarning "foo02"] (* accepted *)
  val x : int [@@ppwarning "foo03"] (* accepted *)

  external z : int -> int = "x" [@@ppwarning "foo04"] (* accepted *)

  [@@@ppwarning "foo05"] (* accepted *)
end

module TestPpwarningStruct = struct
  type 'a t1 = 'a [@@ppwarning "foo06"] (* accepted *)
  type s1 = Foo1 [@ppwarning "foo07"] (* accepted *)
  let x  = 42 [@@ppwarning "foo08"] (* accepted *)

  let f x = (25 [@ppwarning "foo09"]) + 17 (* accepted *)

  external z : int -> int = "x" [@@ppwarning "foo10"] (* accepted *)

  [@@@ppwarning "foo11"] (* accepted *)
end


module type TestTailcallSig = sig
  type 'a t1 = 'a [@@tailcall] (* rejected *)
  type s1 = Foo1 [@tailcall] (* rejected *)
  val x : int [@tailcall] (* rejected *)

  external z : int -> int = "x" [@@tailcall] (* rejected *)

  [@@@tailcall] (* rejected *)
end

module TestTailcallStruct = struct
  type 'a t1 = 'a [@@tailcall] (* rejected *)
  type s1 = Foo1 [@tailcall] (* rejected *)

  let m x = x [@tailcall] (* rejected *)
  let n x = x [@ocaml.tailcall] (* rejected *)
  let o x = (m [@tailcall]) x (* accepted *)
  let p x = (m [@ocaml.tailcall]) x (* accepted *)
  let q x = m x [@tailcall] (* rejected *)

  external z : int -> int = "x" [@@tailcall] (* rejected *)

  [@@@tailcall] (* rejected *)
end


module type TestUnboxedSig = sig
  type t1 = { x : int [@unboxed] } (* rejected *)

  val x : int [@unboxed] (* rejected *)

  type t2 = { x : int } [@@unboxed] (* accepted *)

  val y : int [@@unboxed] (* rejected *)

  external z : float -> float = "x" "y" [@@unboxed] (* accepted *)

  [@@@unboxed] (* rejected *)
end

module TestUnboxedStruct = struct
  let x = (42 [@unboxed], 84) (* rejected *)

  let y = 10 [@@unboxed] (* rejected *)

  external z : float -> float = "x" "y" [@@unboxed] (* accepted *)

  [@@@unboxed] (* rejected *)
end


module type TestUntaggedSig = sig
  type 'a t1 = 'a [@@untagged] (* rejected *)
  type s1 = Foo1 [@untagged] (* rejected *)
  val x : int [@@untagged] (* rejected *)

  external y : (int [@untagged]) -> (int [@untagged]) = "x" "y" (* accepted *)
  external z : int -> int = "x" "y" [@@untagged] (* accepted *)
end

module TestUntaggedStruct = struct
  type 'a t1 = 'a [@@untagged] (* rejected *)
  type s1 = Foo1 [@untagged] (* rejected *)
  let x : int = 42 [@@untagged] (* rejected *)

  external y : (int [@untagged]) -> (int [@untagged]) = "x" "y" (* accepted *)
  external z : int -> int = "x" "y" [@@untagged] (* accepted *)
end


module type TestUnrolledSig = sig
  type t1 = { x : int [@unrolled 42] } (* rejected *)
  type t2 = { x : int } [@@unrolled 42] (* rejected *)

  val f : int -> int [@unrolled 42] (* rejected *)
  val g : int -> int [@@unrolled 42] (* rejected *)

  external z : float -> float = "x" [@@unrolled 42] (* rejected *)

  [@@@unrolled 42] (* rejected *)
end

module TestUnrolledStruct = struct
  let [@unrolled 42] rec test_unrolled x = (* rejected *)
    match x with
    | 0 -> ()
    | n -> test_unrolled (n - 1)

  let () = (test_unrolled [@unrolled 42]) 10 (* accepted *)

  type t1 = { x : int [@unrolled 42] } (* rejected *)
  type t2 = { x : int } [@@unrolled 42] (* rejected *)

  let rec f x = f x [@unrolled 42] (* rejected *)
  let rec f x = f x [@@unrolled 42] (* rejected *)

  external z : int -> int = "x" "y" [@@unrolled 42] (* rejected *)

  [@@@unrolled 42] (* rejected *)
end


module type TestWarnErrorSig = sig
  type t1 = Foo1 [@warnerror "-a+31"] (* accepted *)

  val x : int [@warnerror "-a+31"] (* accepted *)

  type 'a t2 = 'a [@@warnerror "-a+31"] (* accepted *)

  val y : int [@@warnerror "-a+31"] (* accepted *)

  [@@@warnerror "-a+31"] (* accepted *)
end

module TestWarnErrorStruct = struct
  let x = 5 [@warnerror "-a+31"] (* accepted *)

  let y = 10 [@@warnerror "-a+31"] (* accepted *)

  [@@@warnerror "-a+31"] (* accepted *)
end


module type TestWarningSig = sig
  type t1 = Foo1 [@warning "-a+31"] (* accepted *)

  val x : int [@warning "-a+31"] (* accepted *)

  type 'a t2 = 'a [@@warning "-a+31"] (* accepted *)

  val y : int [@@warning "-a+31"] (* accepted *)

  [@@@warning "-a+31"] (* accepted *)
end

module TestWarningStruct = struct
  let x = 5 [@warning "-a+31"] (* accepted *)

  let y = 10 [@@warning "-a+31"] (* accepted *)

  [@@@warning "-a+31"] (* accepted *)
end


module type TestWarnOnLiteralPatternSig = sig
  type test_literal_pattern =
    | Lit_pat1 of int [@warn_on_literal_pattern]  (* accepted *)
    | Lit_pat2 of int [@@warn_on_literal_pattern] (* rejected *)

  type t1 = Foo1 [@warn_on_literal_pattern] (* accepted *)

  val x : int [@warn_on_literal_pattern] (* rejected *)

  type 'a t2 = 'a [@@warn_on_literal_pattern] (* rejected *)

  val y : int [@@warn_on_literal_pattern] (* rejected *)

  [@@@warn_on_literal_pattern] (* rejected *)
end

module TestWarnOnLiteralPatternStruct = struct
  type test_literal_pattern =
    | Lit_pat1 of int [@warn_on_literal_pattern]  (* accepted *)
    | Lit_pat2 of int [@@warn_on_literal_pattern] (* rejected *)

  let x = 5 [@warn_on_literal_pattern] (* rejected *)

  let y = 10 [@@warn_on_literal_pattern] (* rejected *)

  [@@@warn_on_literal_pattern] (* rejected *)
end


(* Attributes in attributes shouldn't be tracked for w53 *)
[@@@foo [@@@deprecated]]


module type TestPollSig = sig
  type 'a t1 = 'a [@@poll error] (* rejected *)
  type s1 = Foo1 [@poll error] (* rejected *)
  val x : int64 [@@poll error] (* rejected *)

  external y : (int64 [@poll error]) -> (int64 [@poll error]) = (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@poll error] (* rejected *)
end

module TestPollStruct = struct
  type 'a t1 = 'a [@@poll error] (* rejected *)
  type s1 = Foo1 [@poll error] (* rejected *)
  let x : int64 = 42L [@@poll error] (* rejected *)
  let [@poll error] f x = x (* accepted *)

  external y : (int64 [@poll error]) -> (int64 [@poll error]) =  (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@poll error] (* rejected *)
end


module type TestSpecialiseSig = sig
  type 'a t1 = 'a [@@specialise] (* rejected *)
  type s1 = Foo1 [@specialise] (* rejected *)
  val x : int64 [@@specialise] (* rejected *)

  external y : (int64 [@specialise]) -> (int64 [@specialise]) = (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@specialise] (* rejected *)
end

module TestSpecialiseStruct = struct
  type 'a t1 = 'a [@@specialise] (* rejected *)
  type s1 = Foo1 [@specialise] (* rejected *)
  let x : int64 = 42L [@@specialise] (* rejected *)
  let [@specialise] f x = x (* accepted *)
  let g x = (f[@specialise]) x (* rejected *)

  external y : (int64 [@specialise]) -> (int64 [@specialise]) = (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@specialise] (* rejected *)
end


module type TestSpecialisedSig = sig
  type 'a t1 = 'a [@@specialised] (* rejected *)
  type s1 = Foo1 [@specialised] (* rejected *)
  val x : int64 [@@specialised] (* rejected *)

  external y : (int64 [@specialised]) -> (int64 [@specialised]) = (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@specialised] (* rejected *)
end

module TestSpecialisedStruct = struct
  type 'a t1 = 'a [@@specialised] (* rejected *)
  type s1 = Foo1 [@specialised] (* rejected *)
  let x : int64 = 42L [@@specialised] (* rejected *)
  let [@specialised] f x = x (* rejected *)
  let g x = (f[@specialised]) x (* accepted *)

  external y : (int64 [@specialised]) -> (int64 [@specialised]) = (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@specialised] (* rejected *)
end


module type TestTailModConsSig = sig
  type 'a t1 = 'a [@@tail_mod_cons] (* rejected *)
  type s1 = Foo1 [@tail_mod_cons] (* rejected *)
  val x : int64 [@@tail_mod_cons] (* rejected *)

  external y : (int64 [@tail_mod_cons]) -> (int64 [@tail_mod_cons]) =
    (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@tail_mod_cons] (* rejected *)
end

module TestTailModConsStruct = struct
  type 'a t1 = 'a [@@tail_mod_cons] (* rejected *)
  type s1 = Foo1 [@tail_mod_cons] (* rejected *)
  let x : int64 = 42L [@@tail_mod_cons] (* rejected *)
  let [@tail_mod_cons] f x = x (* accepted *)
  let g x = (f[@tail_mod_cons]) x (* rejected *)

  external y : (int64 [@tail_mod_cons]) -> (int64 [@tail_mod_cons]) =
    (* rejected *)
    "x"
  external z : int64 -> int64 = "x" [@@tail_mod_cons] (* rejected *)
end
