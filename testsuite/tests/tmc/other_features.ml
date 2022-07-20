(* TEST
   * expect *)

module Non_recursive_let_bad = struct
  type 'a t =
    | N of 'a
    | C of 'a t * 'a t

  let[@tail_mod_cons] rec map f l =
    match l with
    | N v -> N (f v)
    | C (a, b) ->
        let map' l = map f l in
        C (map' a, (map' [@tailcall]) b)
end
[%%expect {|
Lines 6-11, characters 30-40:
 6 | ..............................f l =
 7 |     match l with
 8 |     | N v -> N (f v)
 9 |     | C (a, b) ->
10 |         let map' l = map f l in
11 |         C (map' a, (map' [@tailcall]) b)
Warning 71 [unused-tmc-attribute]: This function is marked @tail_mod_cons
but is never applied in TMC position.
Line 11, characters 19-39:
11 |         C (map' a, (map' [@tailcall]) b)
                        ^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall
Line 11, characters 19-39:
11 |         C (map' a, (map' [@tailcall]) b)
                        ^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall
module Non_recursive_let_bad :
  sig
    type 'a t = N of 'a | C of 'a t * 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end
|}]


module Non_recursive_let_good = struct
  type 'a t =
    | N of 'a
    | C of 'a t * 'a t

  let[@tail_mod_cons] rec map f l =
    match l with
    | N v -> N (f v)
    | C (a, b) ->
        let[@tail_mod_cons] map' l = map f l in
        C (map' a, (map' [@tailcall]) b)
end
[%%expect {|
module Non_recursive_let_good :
  sig
    type 'a t = N of 'a | C of 'a t * 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end
|}]
