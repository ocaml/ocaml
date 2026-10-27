(* TEST
   flags="-dheadshape";
   expect;
*)

(* A simple example. *)
type imm = Imm of int [@unboxed]
type valid = Nonconst of unit | Imm of imm [@unboxed]
[%%expect{|
shape of imm: {imm = Any; blocks = [];}
type imm = Imm of int [@unboxed]
shape of valid: {imm = Any; blocks = [0 size [1]];}
type valid = Nonconst of unit | Imm of imm [@unboxed]
|}]


(* More complex cases arise when unboxing types that are equivalent to the identity type:
     type 'a id = Id of 'a [@unboxed]

   The head shape of this parametrized declaration is [{imm = Any;
   blocks = Any}], but specific instances like [int t] or
   [(bool * string) t] have more precise head shapes.

   Note that identity types are morally the only types ['a t] with
   such a dependency -- where the head shape of [foo t] depends on the
   head shape of [foo]. Indeed, you cannot write
   {[
     type 'a t =
       | Arg of 'a [@unboxed]
       | Other of 'a bar
   ]}
   as there is a shape conflict between the two constructors.

   Note: you can write
   {[
     type 'a t =
       | Arg of 'a [@unboxed]
       | Immediate of int [@unboxed]
     constraint 'a = 'b * 'c 
   ]}
   which has shape [{imm = Any; blocks = [0 size [2]]}, but then again
   the shape of [foo t] is the same for all valid [foo] types, as they
   must all be pairs.

   If/when we would add kind/layout/shape restrictions on type parameters, then we
   could have more interesting types with such a dependency, for example:
   {[
     type ('a : shape {imm = Any; blocks = [0]}) t =
       | Arg of 'a [@unboxed]
       | String of string [@unboxed]
   ]}
   Here [empty t] and [int t] would have more precise shapes than
   the largest possible shape for ['a t].
*)

(* The tree identity types below should be equivalent in terms of head
   shapes. *)
type 'a id1 = 'a
type 'a id2 = Id of 'a [@@unboxed] (* global unboxed variant *)
type 'a id3 = Id of 'a [@unboxed]  (* the Id constructor is unboxed *)
[%%expect{|
shape of _ id1: {imm = Any; blocks = Any;}
type 'a id1 = 'a
shape of _ id2: {imm = Any; blocks = Any;}
type 'a id2 = Id of 'a [@@unboxed]
shape of _ id3: {imm = Any; blocks = Any;}
type 'a id3 = Id of 'a [@unboxed]
|}]

(* The definitions below should have the same head shapes as [int]. *)
type t0 = int 
[%%expect{|
shape of t0: {imm = Any; blocks = [];}
type t0 = int
|}]

type t1 = int id1
[%%expect{|
shape of t1: {imm = Any; blocks = [];}
type t1 = int id1
|}]

type t2 = int id2
[%%expect{|
shape of t2: {imm = Any; blocks = [];}
type t2 = int id2
|}]

type t3 = int id3
[%%expect{|
shape of t3: {imm = Any; blocks = [];}
type t3 = int id3
|}]

type t22 = int id2 id2
[%%expect{|
shape of t22: {imm = Any; blocks = [];}
type t22 = int id2 id2
|}]

type t33 = int id3 id3
[%%expect{|
shape of t33: {imm = Any; blocks = [];}
type t33 = int id3 id3
|}]

(* test polymorphic record fields *)
type empty_list = { empty : 'a . 'a list } [@@unboxed]
type test = Foo of empty_list [@unboxed]
[%%expect{|
shape of empty_list: {imm = [0]; blocks = [0 size [2]];}
type empty_list = { empty : 'a. 'a list; } [@@unboxed]
shape of test: {imm = [0]; blocks = [0 size [2]];}
type test = Foo of empty_list [@unboxed]
|}]

(* test instantiation of constrained types *)
type 'a fst = Fst of 'b [@unboxed] constraint 'a = 'b * 'c
type t = Foo of (int * bool) fst [@unboxed]
[%%expect{|
shape of _ fst: {imm = Any; blocks = Any;}
type 'a fst = Fst of 'b [@unboxed] constraint 'a = 'b * 'c
shape of t: {imm = Any; blocks = [];}
type t = Foo of (int * bool) fst [@unboxed]
|}]
