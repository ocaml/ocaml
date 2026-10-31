# Constructor unboxing

This PR implements a variant of the "constructor unboxing" feature proposed by Jeremy Yallop (@yallop) in [RFC #14](https://github.com/ocaml/RFCs/pull/14) in March 2020, which extends the current `[@@unboxed]` annotation for single-constructor variant types (or single-field record types). A first experimental implementation of this feature was done Nicolas Chataing (@nchataing) in Summer 2021. Gabriel Scherer rebased and reimplemented it in Fall 2024, and rebased it again in Fall 2026 for submission upstream.

### Short example

The following is now accepted:

```ocaml
type gmt_t [@@shape [custom]]
type bignum =
  | Short of int [@unboxed]  (* represented directly by an integer *)
  | Long of Gmp.t [@unboxed] (* Block with Custom_tag (and any size) *)
```

In a [small benchmark](https://github.com/gasche/ocaml/pull/10) in 2020, we tested that this representation provides the same performances as the current Zarith implementation when computations remain in the "short integer" range -- Zarith uses [dirty unsafe tricks](https://github.com/ocaml/Zarith/blob/2be3c25/z.ml#L28-L37) to avoid boxing short integers, which we can replace with [natural OCaml code](https://github.com/gasche/ocaml/blob/head_shape_zarith/Zarith/test.ml#L110-L125).

### Precise specification

See [HEAD_SHAPE.spec.md](HEAD_SHAPE.spec.md).

## Implementation strategy

The main components required for this feature are:
1. We need to compute head shape information for unboxed constructors, which in turn requires computing head shape information for whole type.
2. At type-declaration time, we need to check disjointness of head shapes and reject invalid declarations.
3. We need to compile unboxed constructors in expressions.
4. We need to compile unboxed constructors in patterns.

(3) is in fact very easy: unboxed constructors in expressions are implemented by doing [nothing at all](https://github.com/gasche/ocaml/blob/head_shape/lambda/translcore.ml#L336-L337).

(1) and (2) proved fairly difficult to implement cleanly due in large part to accidental complexity.

### Head shapes

Representing head shapes by their set of possible values is not practical, as `Imm(_)` (the shape of all immediate values) would be too large a set. Instead we use a representation with an explicit "any" value in either domains (for immediates or for tags):

```ocaml
type imm = Imm of int [@@unboxed]
type tag = Tag of int [@@unboxed]
type size = Size of int [@@unboxed]

module ImmSet : Set.S with type elt = imm
module SizeSet : Set.S with type elt = size
module TagMap : Map.S with type key = tag

type 'a or_any = Those of 'a | Any

type imm_set = ImmSet.t or_any
type size_set = SizeSet.t or_any
type block_set = size_set TagMap.t or_any

type shape = {
  imms: imm_set; (* set of immediates the head can be *)
  blocks: block_set; (* set of block shapes the head can be *)
}
```

This definition, along as various shape operations, are found in `typing/head_shape_types.{ml,mli}`: https://github.com/gasche/ocaml/blob/head_shape_rebase/typing/head_shape_types.mli

### Compiling unboxed constructors in pattern-matching

Pattern-matching compilation is done in [lambda/matching.ml](https://github.com/gasche/ocaml/blob/head_shape_rebase/lambda/matching.ml).

The pattern-matching compiler works by decomposing pattern-matching clauses into a lower-level representation built from "switches", a lower-level control-flow construct that just dispatches on the value of immediates and tags of a head block. Those switches are then lowered further to Lambda constructs (`Lifthenelse` + tag accesses, or a native `Lswitch` instruction used for the bytecode compiler).

A switch is a list of switch clauses, which are of the form `(cstr_tag, action)`, where `cstr_tag` is either an immediate value or a block tag, and actions are pieces of lambda-code. Our implementation adds a new sort of `cstr_tag`, morally `Cstr_unboxed of head_shape` (representation details in the next section); these "unboxed tags" are "expanded away" by replacing clauses witch unboxed tags by clauses for the underlying elements of the head shape.

For example, consider the following type declarations:

```ocaml
type internal = Foo | Bar of string [@unboxed]        (* head shape: Imm(0) ⊎ Block(252, _) *)
type t = First of int | Second of internal [@unboxed]
```

An exhaustive switch on a value of type `t` may look like

```ocaml
| Cstr_block 0 -> act1
| Cstr_unboxed [Imm(0); Block(252)] -> act2)
```

and we expand it to a switch without unboxed constructors, to be compiled by the existing machinery:

```ocaml
| Cstr_block 0 -> act1
| Cstr_constant 0 -> act2
| Cstr_block 252 -> act2
```

This is the general idea, but in fact we need slightly more sophistication to deal with `Imm(_)` or `Block(_, _ )` shapes. We change the definition of clauses to contain both:
- either a list of specific actions for some immediates, or an "any" action for all immediates, and
- either a list of specific actions for some block tags, or an "any" action for all blocks

The code that lowers switches into `Lifthenelse` or `Lswitch` has to be adapted to deal with this new structure.

Remark: To emit `Lswitch` instructions, the compiler needs to know of the range of possible immediate values and tags (when we don't have `any_{const,nonconst}` actions). More precisely, we want the maximal possible immediate constructor and block tag at this type (minimal values are always assumed to be 0).

This new representation of switches with "any immediate" or "any block tag" cases is in a new `Cases` submodule:
https://github.com/gasche/ocaml/blob/head_shape_rebase/lambda/matching.ml#L3207-L3312

The expansion of unboxed constructors into separate actions is performed in the `split_cases` function:
https://github.com/gasche/ocaml/blob/head_shape_rebase/lambda/matching.ml#L3314-L3349

Finally, the lowering of those switches to Lambda code is done in the `combine_regular_constructor` function:
https://github.com/gasche/ocaml/blob/head_shape_rebase/lambda/matching.ml#L3444-L3744


### General approach to computing head shapes

We use the "on-demand" approach advertised by @lpw25 and @stedolan to compute type properties on-demand, at each type declaration with unboxed constructors, instead of trying to compute the property modularly for all type definitions (which may or may not occur in the type of an unboxed constructor later).

Here "demand" is defined as either:
- a type declaration using some unboxed constructors is processed by the type-checker
  (this "demands" computing the shape of each unboxed constructor)
- the pattern-matching compiler encounters an unboxed constructor

In particular, it can happen that an unboxed-constructor is compiled *without* the type-checker having processed its declaration first! This is the case where the type declaration comes from a .cmi – it has been checked by a previous run of the compiler, and is then imported as-is into the environment without being re-checked.

The head shape computed for an unboxed constructor is cached on first demand, and will not be recomputed several times. The computation requires a typing environment -- which must be an extension of the environment in which the constructor was defined -- to unfold the definition of typing constructors used in the unboxed constructor parameter type, and further constructions reachable from them.

The computation of the head shape is done in the functions `of_type_expr` and `of_typedescr` in a new `Head_shape` module of [typing/typedecl_unboxed.ml](https://github.com/gasche/ocaml/blob/head_shape/typing/typedecl_unboxed.ml). We use fuel to avoid unbounded loop on some recursive types. (In the constructor-unboxing [paper](https://arxiv.org/abs/2311.07369) we discuss an elaborate termination measure, but we judged that implementing this would add complexity and fragility.)

### Storing unboxing information in type descriptions

This, surprisingly, turned out to be the most difficult part of the implementation to get right.

As discussed above, we need to store two different kind of information for compilation:
- per-unboxed-constructor information: what is the shape of the unboxed constructor argument
- per-sum-type information: how many unboxed constructors are there,
  what is the maximal value of the immediates and block tags they contain

The existing codebase already computes per-constructor information (imm/tag values) and per-sum-type information (the number of constant and non-constant constructors) in [typing/datarepr.ml : constructor_descrs](https://github.com/gasche/ocaml/blob/head_shape_rebase/typing/datarepr.ml#L107-L189). This function is called (through `Datarepr.constructors_of_type`) from the `Env` module, which is in the business of converting type *declarations* (as provided by the user) into type *descriptions* (with extra information for compilation) as stored in the typing environment.

Unfortunately, this scheme cannot be extended for our unboxing-related information. Computing the head shape of an unboxed constructor requires unfolding type constructors from the typing environment, so in particular the function doing the computation needs an `Env.t` argument and depends on the `Env` module. It cannot be put in the module `Datarepr` that itself is called from `Env`, as this would be create a circular dependency.

We tried many ways to avoid this cyclic dependency (for example we went half-way through large refactorings trying to compute type descriptions in `Typedecl`, and pass it to `Env`-extending functions, instead of computing them within `Env`). In the end we settled on a very simple approach: the per-unboxed-constructor information has a type that is morally `head_shape option ref`, and the per-sum-type information has type that is morally `unboxed_type_data option ref`, they are initialized to `ref None` by the `Datarepr` module, and then filled on-demand -- from a module that *can* depend on `Env`.

## Testing

We produced two new test directories.

### `testsuite/tests/head_shape`

These tests exercize the head-shape computation logic, as well as the type-system checks to avoid unsound declarations.

`basic.ml` contains various datatypes to exercise the head-shape computation machinery, using a new `-dheadshape` option that prints debug info on type declarations. For example:

```ocaml
type 'a homemade_list = Nil | Cons of 'a * 'a list
[%%expect{|
shape of _ homemade_list: {imm = [0]; blocks = [0 size [2]];}
type 'a homemade_list = Nil | Cons of 'a * 'a list
|}]

type lazy_option = int option Lazy.t
[%%expect{|
shape of lazy_option:
  {imm = [0]; blocks = [0 size [1]; forcing; lazy; forward];}
type lazy_option = int option Lazy.t
|}]

type abstract [@@shape [int; constructor 0; constructor 1 ~size:2]]
[%%expect{|
shape of abstract: {imm = Any; blocks = [0; 1 size [2]];}
type abstract
|}]
```

`conflicts.ml` exercises (mostly) incorrect type declarations, to check that they are rejected as they must:

```
type v =
  | Bool of bool
  | Maybe_int of int option [@unboxed]
[%%expect{|
Lines 1-3, characters 0-38:
1 | type v =
2 |   | Bool of bool
3 |   | Maybe_int of int option [@unboxed]
Error: Constructors Maybe_int and Bool have overlapping representations.
         shape of Maybe_int: {imm = [0]; blocks = [0 size [1]];}
         shape of Bool: {imm = []; blocks = [0 size [1]];}
|}]
```

`unboxed.ml` exercises corner cases created by unboxed constructors,
in particular those related to separability, for example:

```ocaml
type 'a fst = Fst of 'b [@unboxed] constraint 'a = 'b * 'c
type t = Foo of (int * bool) fst [@unboxed]
[%%expect{|
shape of _ fst: {imm = Any; blocks = Any;}
type 'a fst = Fst of 'b [@unboxed] constraint 'a = 'b * 'c
shape of t: {imm = Any; blocks = [];}
type t = Foo of (int * bool) fst [@unboxed]
|}]
```

Finally. `signatures.ml` checks that the module system prevents assigning a narrower shape through module subtyping:

```
(* We expect this to fail, the shape annotation is too restrictive. *)
type t = A | B [@@shape [imm 0]]
[%%expect {|
Line 1, characters 0-32:
1 | type t = A | B [@@shape [imm 0]]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this type declaration, the actual head shape does not match the expected type shape.
|}] (* expected result *)
```

### `testsuite/tests/unboxed-constructors`

`matching.ml` records the code generated when pattern-matching on unboxed constructors. For example, one case that uses `Obj.tag` is as follows:

```
module NoImmSparseTags = struct
  type t = A of int | B of int * int | C of string [@unboxed]

  let f = function
    | A i -> i
    | B (a, b) -> a + b
    | C s -> String.length s
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "NoImmSparseTags/416"
  (let
    (f/2 =
       (function param/2 : int
         (let (tag/0 =a (caml_obj_tag param/2))
           (if (!= tag/0 1)
             (if (>= tag/0 252) (string.length param/2)
               (field_int 0 param/2))
             (+ (field_int 0 param/2) (field_int 1 param/2))))))
    (makeblock 0 f/2)))
module NoImmSparseTags :
  sig
    type t = A of int | B of int * int | C of string [@unboxed]
    val f : t -> int
  end
|}]
```

`example_zarith.ml` contains a Zarith—inspired example of a type of either OCaml immediate integers or a FFI-provided implementation of big-numbers as Custom blocks. It then checks that the code generated for an addition function does not contain any boxing:

```
let add z1 z2 =
  match z1, z2 with
  | Big gmp1, Big gmp2 ->
      Big (Gmp.add gmp1 gmp2)
  | Big gmp, Small n | Small n, Big gmp ->
      Big (Gmp.add_int gmp n)
  | Small n1, Small n2 ->
      if add_would_overflow n1 n2 then
        Big (Gmp.add_int (Gmp.of_int n1) n2)
      else
        Small (n1 + n2)
[%%expect {|
(let
  (add/0 =
     (function z1/0 z2/0
       (catch
         (if (isint z1/0)
           (if (isint z2/0)
             (if (int_add_would_overflow z1/0 z2/0)
               (gmp_add_int (gmp_of_int z1/0) z2/0) (+ z1/0 z2/0))
             (exit 2 z2/0 z1/0))
           (if (isint z2/0) (exit 2 z1/0 z2/0) (gmp_add z1/0 z2/0)))
        with (2 gmp/0 n/0[int]) (gmp_add_int gmp/0 n/0))))
  (apply (field_mut 1 (global Toploop!)) "add" add/0))
Line 1:
Error: The external function "int_add_would_overflow" is not available
|}] (* a missing external error is expected, the point is to look at the [add] code. *)
```

Finally, `example_native_compute` contains an unboxed datatype that mixes variant types and (unboxed) functions, inspired by Rocq's native-compute:

```
type t =
  | Var of int [@unboxed]
  | App of t * t
  | Lam of (t -> t) [@unboxed]

let app t v =
  match t with
  | Lam f -> f v
  | ne -> App (ne, v)
[%%expect {|
(let
  (app/0 =
     (function t/0 v/0
       (catch
         (if (isint t/0) (exit 2)
           (if (>= (caml_obj_tag t/0) 247) (apply t/0 v/0) (exit 2)))
        with (2) (makeblock 0 t/0 v/0))))
  (apply (field_mut 1 (global Toploop!)) "app" app/0))
val app : t -> t -> t = <fun>
|}]
```

## Future work

We know about some improvements that we are interested in working on eventually, but we believe are out of scope for this PR.

### Compilation strategy

The pattern-matching compilation strategy could be improved in some cases, at the cost of substantial changes to our current implementation. Consider

```ocaml
type t =
  | Int of int [@unboxed]
  | String of string [@unboxed]

and u =
  | T of t [@unboxed]
  | Float of float [@unboxed]
```

A matching of the form
```ocaml
match arg with
| Float _ -> 0
| T (Int _) -> 1
| T (String _) -> 2
```

will generate two nested switches, morally:

```
match-shape arg with
| Block(double_tag, _) -> 0
| Imm(_) | Block(string_tag, _) ->
  match-shape arg with
  | Imm(_) -> 1
  | Block(Obj.string_tag) -> 2
```

This is the exact same generated-code shape that we would have without unboxing, except that one dereference is gone. The first switch is now more expensive as the now-unboxed input values are not in a small consecutive interval of tags.

It would of course be better to generate a single switch instead. We see two approaches to do this:

1. We could "merge" some switches after the fact.
2. We could remove unboxed constructors much earlier in the pattern-matching compiler, before matrix decomposition, so that morally `T (Int _)` is handled as having a single head constructor `T ∘ Int`.

We are not considering either of those ambitious changes a priority, because:
1. We expect the performance wins from unboxing (if any) to come from tighter memory representations;
   the pattern-matching branches are comparatively unexpansive
2. In practice most examples of unboxed constructors we have in mind do *not* hit this bad situation.

In particular, if you unboxed a constructor with only immediate values, then the generated code will split the query space by checking `Obj.is_int`, which is the optimal strategy in any case (what the native compiler will do for the merged switch as well). For example, these produce exactly the same native code:

```ocaml
(* what we get today *)
match-shape arg with
| Imm(_) -> -1
| Block(0, _) | Block(1, _) ->
  match-shape arg with
  | Block(0, _) -> 0
  | (Block(1, _) -> 1

(* what switch-merging would give *)
match-shape arg with
| Imm(_) -> -1
| Block(0, _) -> 0
| Block(1, _) -> 1
```

The two forms get compiled to the following

```ocaml
if Obj.is_int arg then -1
else match-tag arg with
  | 0 -> 0
  | 1 -> 1
```


### faster `Obj.tag` version for pattern-matching

The `Obj.tag` primitive supports arbitrary OCaml values, so it must perform noticeably more checks than what the pattern-matching compiler generates internally. We use `Obj.tag` in our compilation strategy in the following situation:

- we are compiling in bytecode
- we are compiling a case-split on blocks that include tags above `Obj.last_non_constant_constructor_tag`
  (for example, a value mixing regular variants and strings or functions, as is now allowed)

Generating a `switch` instruction in bytecode in those cases would take at least 250 words for the switch table, which is a bit much. But because we only call `Obj.tag` in this situation, we could use an optimized unsafe primitive that already assumes that its argument is a block. We also never distinguish the lazy tags (`lazy`, `forcing`, `forward`), so we could avoid any synchronization with the thunk-forcing machinery.
