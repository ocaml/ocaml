(* TEST
   * expect *)
type 'a tree =
| Leaf of 'a
| Node of 'a tree * 'a tree
[%%expect{|
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
|}]

module Ambiguous = struct
  let[@tail_mod_cons] rec map f = function
  | Leaf v -> Leaf (f v)
  | Node (left, right) ->
    Node (map f left, map f right)
end
[%%expect{|
Line 5, characters 4-34:
5 |     Node (map f left, map f right)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@tail_mod_cons]: this constructor application may be TMC-transformed
       in several different ways. Please disambiguate by adding an explicit
       [@tailcall] attribute to the call that should be made tail-recursive,
       or a [@tailcall false] attribute on calls that should not be
       transformed.
|}]

module Positive_disambiguation = struct
  let[@tail_mod_cons] rec map f = function
  | Leaf v -> Leaf (f v)
  | Node (left, right) ->
    Node (map f left, (map [@tailcall]) f right)
end
[%%expect{|
module Positive_disambiguation :
  sig val map : ('a -> 'b) -> 'a tree -> 'b tree end
|}]

module Negative_disambiguation = struct
  let[@tail_mod_cons] rec map f = function
  | Leaf v -> Leaf (f v)
  | Node (left, right) ->
    Node ((map [@tailcall false]) f left, map f right)
end
[%%expect{|
module Negative_disambiguation :
  sig val map : ('a -> 'b) -> 'a tree -> 'b tree end
|}]

module Positive_and_negative_disambiguation = struct
  (* in-depth disambiguations *)
  type 'a t =
    | N
    | C of 'a t * ('a t * 'a t)

  let[@tail_mod_cons] rec map1 f l =
    match l with
    | N -> N
    | C (a, (b, c)) ->
        C ((map1 [@tailcall]) f a, ((map1 [@tailcall false]) f b, map1 f c))

  let[@tail_mod_cons] rec map2 f l =
    match l with
    | N -> N
    | C (a, (b, c)) ->
        C ((map2 [@tailcall false]) f a, ((map2 [@tailcall]) f b, map2 f c))
end
[%%expect {|
module Positive_and_negative_disambiguation :
  sig
    type 'a t = N | C of 'a t * ('a t * 'a t)
    val map1 : 'a -> 'b t -> 'c t
    val map2 : 'a -> 'b t -> 'c t
  end
|}]

module Long_before_and_after = struct
  type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree * 'a tree * 'a tree * 'a tree
  let[@tail_mod_cons] rec map f = function
    | Leaf v -> Leaf (f v)
    | Node (t1, t2, t3, t4, t5) ->
        (* manual unfolding *)
        Node (map f t1, map f t2, (map[@tailcall]) f t3, map f t4, map f t5)

  let () =
    assert (map succ (Node (Leaf 0, Leaf 1, Leaf 2, Leaf 3, Leaf 4))
                    = Node (Leaf 1, Leaf 2, Leaf 3, Leaf 4, Leaf 5))
end
[%%expect {|
module Long_before_and_after :
  sig
    type 'a tree =
        Leaf of 'a
      | Node of 'a tree * 'a tree * 'a tree * 'a tree * 'a tree
    val map : ('a -> 'b) -> 'a tree -> 'b tree
  end
|}]

