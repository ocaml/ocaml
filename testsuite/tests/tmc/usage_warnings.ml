(* TEST
   * expect *)

(* build-up *)
let[@tail_mod_cons] rec append xs ys =
  match xs with
  | [] -> ys
  | x :: xs -> x :: append xs ys
[%%expect {|
val append : 'a list -> 'a list -> 'a list = <fun>
|}]

(* incorrect version: this cannot work *)
let[@tail_mod_cons] rec flatten = function
  | [] -> []
  | xs :: xss -> append xs (flatten xss)
[%%expect {|
Line 3, characters 17-40:
3 |   | xs :: xss -> append xs (flatten xss)
                     ^^^^^^^^^^^^^^^^^^^^^^^
Warning 72 [tmc-breaks-tailcall]: This call
is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be transformed into a tail call.
Please either mark the called function with the [@tail_mod_cons]
attribute, or mark this call with the [@tailcall false] attribute
to make its non-tailness explicit.
Lines 1-3, characters 34-40:
1 | ..................................function
2 |   | [] -> []
3 |   | xs :: xss -> append xs (flatten xss)
Warning 71 [unused-tmc-attribute]: This function is marked @tail_mod_cons
but is never applied in TMC position.
val flatten : 'a list list -> 'a list = <fun>
|}]

(* correct version *)
let[@tail_mod_cons] rec flatten = function
  | [] -> []
  | xs :: xss ->
      let[@tail_mod_cons] rec append_flatten xs xss =
        match xs with
        | [] -> flatten xss
        | x :: xs -> x :: append_flatten xs xss
      in append_flatten xs xss
[%%expect {|
val flatten : 'a list list -> 'a list = <fun>
|}]

(* incorrect version *)
let[@tail_mod_cons] rec flatten = function
  | [] -> []
  | xs :: xss ->
      let rec append_flatten xs xss =
        match xs with
        | [] -> flatten xss
        | x :: xs ->
            (* incorrect: this call to append_flatten is not transformed *)
            x :: append_flatten xs xss
      in append_flatten xs xss
[%%expect {|
Line 10, characters 9-30:
10 |       in append_flatten xs xss
              ^^^^^^^^^^^^^^^^^^^^^
Warning 72 [tmc-breaks-tailcall]: This call
is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be transformed into a tail call.
Please either mark the called function with the [@tail_mod_cons]
attribute, or mark this call with the [@tailcall false] attribute
to make its non-tailness explicit.
Lines 1-10, characters 34-30:
 1 | ..................................function
 2 |   | [] -> []
 3 |   | xs :: xss ->
 4 |       let rec append_flatten xs xss =
 5 |         match xs with
 6 |         | [] -> flatten xss
 7 |         | x :: xs ->
 8 |             (* incorrect: this call to append_flatten is not transformed *)
 9 |             x :: append_flatten xs xss
10 |       in append_flatten xs xss
Warning 71 [unused-tmc-attribute]: This function is marked @tail_mod_cons
but is never applied in TMC position.
val flatten : 'a list list -> 'a list = <fun>
|}]

(* incorrect version: the call to append-flatten is not transformed *)
let rec flatten = function
  | [] -> []
  | xs :: xss ->
      let[@tail_mod_cons] rec append_flatten xs xss =
        match xs with
        | [] ->
            (* incorrect: if flatten does not have a TMC version,
               this call is not tail-recursive in the TMC version of
               append-flatten, so this version is in fact equivalent
               to the "cannot work" version above: the "append" part
               runs in constant stack space, but the "flatten" part is
               not tail-recursive. *)
            flatten xss
        | x :: xs ->
            x :: append_flatten xs xss
      in append_flatten xs xss
[%%expect {|
Line 13, characters 12-23:
13 |             flatten xss
                 ^^^^^^^^^^^
Warning 72 [tmc-breaks-tailcall]: This call
is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be transformed into a tail call.
Please either mark the called function with the [@tail_mod_cons]
attribute, or mark this call with the [@tailcall false] attribute
to make its non-tailness explicit.
val flatten : 'a list list -> 'a list = <fun>
|}]



module Tail_calls_to_non_specialized_functions = struct
(* This module contains regression tests for some delicate warning behavior:
   if the list_id call below goes to a non-specialized function,
   it gets the "use [@tailcall false]" warning, but it is in tailcall
   position in the direct-style version, so it could also get the
   "invalid [@tailcall false] assumption" warning. *)

  (* *not* TMC-specialized *)
  let list_id = function
    | [] -> []
    | x :: xs -> x :: xs

  let[@tail_mod_cons] rec filter_1 f li =
    match li with
    | [] -> []
    | x :: xs ->
        if f x
        then x :: filter_1 f xs
        else
          list_id
            (* no [@tailcall false]: this should warn that
               the call becomes non-tailcall in the TMC version. *)
            (filter_1 f xs)

  let[@tail_mod_cons] rec filter_2 f li =
    match li with
    | [] -> []
    | x :: xs ->
        if f x
        then x :: filter_2 f xs
        else
          (list_id[@tailcall false])
            (* [@tailcall false]: this should *not* warn that
               the call is in fact in tail position in the direct version. *)
            (filter_2 f xs)
end
[%%expect {|
Lines 20-23, characters 10-27:
20 | ..........list_id
21 |             (* no [@tailcall false]: this should warn that
22 |                the call becomes non-tailcall in the TMC version. *)
23 |             (filter_1 f xs)
Warning 72 [tmc-breaks-tailcall]: This call
is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be transformed into a tail call.
Please either mark the called function with the [@tail_mod_cons]
attribute, or mark this call with the [@tailcall false] attribute
to make its non-tailness explicit.
module Tail_calls_to_non_specialized_functions :
  sig
    val list_id : 'a list -> 'a list
    val filter_1 : ('a -> bool) -> 'a list -> 'a list
    val filter_2 : ('a -> bool) -> 'a list -> 'a list
  end
|}]

module All_annotations_correctly_used = struct
  type 'a t =
    | N of 'a
    | Graft of int
    | Tau of 'a t
    | C of 'a t * 'a t

  let[@inline never] rec graft n =
    graft n

  let[@tail_mod_cons] rec map f l =
    (* this function should never warn *)
    match l with
    | N v -> N (f v)
    | Graft n ->
        if n >= 0
        then (graft[@tailcall false]) n
        else Tau ((graft[@tailcall false]) n)
    | Tau t -> (map[@tailcall]) f t
    | C (a, b) ->
        let[@tail_mod_cons] map' l = map f l in
        C (map' a, (map' [@tailcall]) b)
end
[%%expect {|
module All_annotations_correctly_used :
  sig
    type 'a t = N of 'a | Graft of int | Tau of 'a t | C of 'a t * 'a t
    val graft : 'a -> 'b
    val map : ('a -> 'b) -> 'a t -> 'b t
  end
|}]

module All_annotations_flipped = struct
  type 'a t =
    | N of 'a
    | Graft of int
    | Tau of 'a t
    | C of 'a t * 'a t

  let[@inline never] rec graft n =
    graft n

  let[@tail_mod_cons] rec map_wrong f l =
    match l with
    | N v -> N (f v)
    | Graft n ->
        if n >= 0
        then (graft[@tailcall]) (* this should warn *) n
        else Tau ((graft[@tailcall]) (* this should also warn *) n)
    | Tau t ->
        (map_wrong[@tailcall false])
          (* this attribute disables the TMC call here,
             so it does generate non-tail code:
             the annotation is erased in direct-style, kept in DPS,
             and the generated code must not warn. *)
          f t
    | C (a, b) ->
        let[@tail_mod_cons] map' l = map_wrong f l in
        C (map' a,
           (map' [@tailcall false])
             (* this attribute results in the other map' being selected for TMC,
                no warning here. *)
             b)
end
[%%expect {|
Line 16, characters 13-56:
16 |         then (graft[@tailcall]) (* this should warn *) n
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 72 [tmc-breaks-tailcall]: This call
is in tail-modulo-cons position in a TMC function,
but the function called is not itself specialized for TMC,
so the call will not be transformed into a tail call.
Please either mark the called function with the [@tail_mod_cons]
attribute, or mark this call with the [@tailcall false] attribute
to make its non-tailness explicit.
Line 17, characters 17-67:
17 |         else Tau ((graft[@tailcall]) (* this should also warn *) n)
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall
Line 16, characters 13-56:
16 |         then (graft[@tailcall]) (* this should warn *) n
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall
Line 17, characters 17-67:
17 |         else Tau ((graft[@tailcall]) (* this should also warn *) n)
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall
module All_annotations_flipped :
  sig
    type 'a t = N of 'a | Graft of int | Tau of 'a t | C of 'a t * 'a t
    val graft : 'a -> 'b
    val map_wrong : ('a -> 'b) -> 'a t -> 'b t
  end
|}]
