# Constructor unboxing specification

We describe the specification of "constructor unboxing", as initially proposed by Jeremy Yallop (@yallop) in [RFC#14](https://github.com/ocaml/RFCs/pull/14) in March 2020, and implemented by Nicolas Chataing and Gabriel Scherer in [Summer 2021](https://github.com/ocaml/RFCs/pull/14#issuecomment-920643103). We published a paper (also with Stephen Dolan) summarizing this work in 2022: [Unboxed data constructors -- or, how cpp decides a halting problem](https://arxiv.org/abs/2311.07369).

We support a per-constructor `[@unboxed]` attribute, that can be used in a variant type as long as the set of values corresponding to each constructor (boxed or unboxed) remain disjoint.

For example:

```ocaml
type bignum =
  | Short of int [@unboxed] (* represented directly by an immediate *)
  | Long of Gmp.t           (* Block of tag 0 (first non-unboxed constructor) *)
```

## Precise specification

We define the *head* of an OCaml value as follows:
- the head of an immediate value `v` is `Imm(v)`
- the head of a heap block with tag `t` and size `sz` is `Block(t, sz)`.

(In other words, the head tracks whether a value is immediate or a block, and for blocks keeps the tag and size.)

The "head shape" of a type is an over-approximation of the set of heads of all possible values of this type.

Now consider a variant type declaration containing one or several constructors annotated with `[@unboxed]`:

```ocaml
type ('a, 'b, ...) t =
  | Const0 (* some constant constructors *)
  | Const1
  | ...
  | Const{m}
  | NonConst0 of t00 * t01 * ... t0{sz0-1}
  | Nonconst1 of t10 * t11 * ... t1{sz1-1}
  | ...
  | NonConst{n} of t{n}0 * t{n}1 * ... t{n}{sz{n}-1}
  | Unboxed1 of u0 [@unboxed]
  | Unboxed2 of u1 [@unboxed]
  | ...
  | Unboxed{l} of u{l} [@unboxed]
```

(For simplicity we wrote above all constant constructors first, then all non-constant constructors then all unboxed constructors. Of course we support arbitrary interleaving of these categories.)

The compiled representation of this type is as follows:
- as before, constant constructors `Const{k}` are represented by the immediate number `k`
- as before, non-constant constructors `Nonconst{k} of ...` are represented by a heap block with tag `k`
- unboxed constructors `Unboxed{k} of u{k}` are represented directly by the value of type `u{k}`, without
  any boxing

This definition is *rejected* statically if the unboxed constructors overlap with the other values of the type, in the following precise sense:

1. We compute the "boxed head shape" `BS` of this type *without* the unboxed constructors; by definition of the head shape, this is the set `{Imm(0), Imm(1)... Imm(m), Block(0, sz0), .., Block(n, sz{n})}`.

2. Then we compute the "unboxed shapes" `US{k}` of each unboxed constructor, that is the head shape of `u{k}`.

3. The type is accepted if and only if the shapes `BS, US0, US1, ..., US{l}` are disjoint from each other. The head shape of the whole shape is then the disjoint union `BS ⊎ US0 ⊎ US1 ⊎ ... ⊎ US{l}`.

Unknown/abstract types are assumed to have a "top" shape containing any possible head, unless it is annotated by the user to describe a more precise head shape.


## Examples

```ocaml
(** Basic examples *)

(* rejected *)
type t =
  | Int of int [@unboxed] (* shape: Imm(_), any immediate *)
  | Unit                  (* shape: Imm(0), conflicts with Int above *)

(* accepted *)
type t =
  | Int of int [@unboxed]  (* shape: Imm(_) *)
  | Box of t               (* shape: Block(0, 1) *)
  (* shape(t): Imm(_) ⊎ Block(0, 1) *)

(* accepted *)
type prod = t * t
and t =
  | Int of int [@unboxed]        (* shape: Imm(_), any immediate *)
  | String of string [@unboxed]  (* shape: Block(Obj.string_tag, _)    (Obj.string_tag is 252) *)
  | Prod of prod [@unboxed]      (* shape: Block(0, 2) *)
  (* shape(t): Imm(_) ⊎ Block(string_tag, _) ⊎ Block(0, 2) *)


(** With abstract types *)

type abstract (* any shape *)

(* accepted *)
type t =
  | Int of int [@unboxed] (* shape: Imm(_) *)
  | Abs of abstract       (* shape: Block(0, 1) *)
  (* shape(t): Imm(_) ⊎ Block(0, 1) *)

(* rejected *)
type t =
  | Int of int                 (* shape: Block(0, 1) *)
  | Abs of abstract [@unboxed] (* any shape, conflicts with Int *)

(* accepted *)
type gmp_t [@@shape [custom]] (* shape: Block(custom_tag, _) *)
type t =
  | Int of int [@unboxed]   (* shape: Imm(_) *)
  | Gmp of gmp_t [@unboxed] (* shape: Block(custom_tag, _) *)
  (* shape(t): Imm(_) ⊎ Block(custom_tag, _) *)


(** Nested unboxing *)

type t1 =
  | Int of int [@unboxed]
  | Block of unit
  (* shape(t1): Imm(_) ⊎ Block(0, 1) *)

(* rejected *)
type t2 =
  | T1 of t1 [@unboxed] (* shape: Imm(_) ⊎ Block(0, 1) *)
  | S of string         (* shape: Block(0, _), conflicts with T1 *)

(* accepted *)
type t3 =
  | T1 of t1 [@unboxed]    (* shape: Imm(_) ⊎ Block(0, 1) *)
  | S of string [@unboxed] (* shape: Block(string_tag, _) *)
  (* shape(t3): Imm(_) ⊎ Block(0, 1) ⊎ Block(string_tag, _) *)
```

## Discussion

### Comparison with Yallop's proposal [RFC#14](https://github.com/ocaml/RFCs/pull/14)

Jeremy Yallop's proposal uses a global annotation `[@@unboxed]` on all constructors at once, we use a per-constructor annotation `[@unboxed]`. (The RFC mentions this as a possible extension in "Extension: partial unboxing".) It would be easy to interpret `[@@unboxed]` as just "[@unboxed] on all constructors", but we have not implemented this yet.

A major difference is that the RFC#14 specification suggests renumbering constructors in some cases, where the representation of `C of foo [@unboxed]` is taken to be different from the representation of `foo`, in order to avoid conflicts with other constructors at this type. We do not support any such renumbering:
- the representation of `Unboxed of foo [@unboxed]` is always the representation of `foo`
- the representation of `Boxed of foo` always uses the block tag consecutive/next/succedent to the previous boxed-constructor tag in the declaration (filtering out unboxed constructors).

### separability

When the compiler is in `flat-float-array` mode, soundness relies on the property that all OCaml types are "separated": they contain either (1) only `float` values, or (2) no `float` value. New forms of unboxing must preserve this property.

We track separatedness as part of the head-shape computation for unboxed type declaration, by adding to head-shape data a "separated" bit. This approach is more precise than the pre-existing "separability analysis" (which tries to check this modularly in a way similiar to variance), and we could the separability analysis entirely (left as future work).


### Leftover question: how close to the compiler-distribution runtime should the specification be?

We define static accept/reject decisions for partially-unboxed types using "head shapes", which are defined in terms of the value-representation strategy of the main OCaml implementation. Should we have a more abstract definition, that leaves more room to other representations in alternative implementations?

Our intuition is that we could choose a "weakening" of our current specification that distinguishes less different sort of shapes -- thus rejects more definitions -- and gives good flexibility for alternative implementations. Here are some things we could do:

- We could stop making assumptions about the shape of function closures (currently: {Closure_tag, Infix_tag}), preventing the unboxing of closure-holding constructors.
- We could also weaken our assumptions about built-in containers (string/byte, arrays, double-arrays, etc.)
- We could stop distinguishing "float" from immediates (ouch!) if jsoo does this. What about Int32, Int64, should they be known as custom values?

In other words: what amount of runtime type information should we require from OCaml implementations?

At the limit, one extreme choice would be to only reason on the tag of variant constructors (constant or not), which are distinguishable from each other in any OCaml implementation, and not make any other assumption about head shapes (map all types except variants to the "top" shape). This would reject most unboxing definitions, leave maximal freedom for language implementations. Unfortunately this would also prevent the actually-interesting uses of the feature we know about, which mostly resolve around unboxing an `int`-carrying constructor.

One intermediate position would be to use a description similar to the 'shape names' that we support in attributes, described by the following type:

```
type Asttypes.shape_name =
  | Any
  | Imm of int
  | Constructor of { tag : int; size : int option }
  | Int
  | Float
  | String
  | Tuple of { size : int option }
  | Array
  | Floatarray
  | Function
  | Object
  | Lazy of shape_name list
  | Continuation
  | Extensible_variant
  | Polymorphic_variant of { has_consts : bool; has_nonconsts : bool }
  | Abstract of { size : int option }
  | Custom of { size : int option }
```

Notice for example that:
- `String` or `Function` are given abstractly, rather than listing a set of possible tags.
- Only some category (`Constructor/Tuple`, `Abstract`, `Custom`) support distinguishing on a size

This could be the basis for a more high-level/portable specification of head shapes.
