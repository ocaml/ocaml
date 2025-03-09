(* TEST *)

open Printf
open Effect
open Effect.Deep

(** {1 Iterators and generators} *)

(** Naive primality test. *)

let isprime n =
  let rec try_divide i =
    if i * i > n then true else
    if n mod i = 0 then false else
    try_divide (i + 2) in
  n mod 2 <> 0 && try_divide 3

(** Iterate [f] over all primes. *)

let iter_primes (f: int -> unit) : unit =
  for n = 2 to max_int do
    if isprime n then f n
  done

(** Produce the sequence of all primes. *)

let seq_primes : int Seq.t =
  let rec gen n : int Seq.t =
    if isprime n then (fun () -> Seq.Cons(n, gen (n + 1))) else gen (n + 1)
  in gen 2

(** Implementing [gen_primes] from [iter_prime], using control inversion. *)

type _ eff += Next_prime : int -> unit eff

let gen_primes : int Seq.t =
  match iter_primes (fun n -> perform (Next_prime n)) with
  | () -> Seq.empty
  | effect Next_prime n, k -> fun () -> Seq.Cons(n, continue k ())

let same_sequences (s1: 'a Seq.t) (s2: 'a Seq.t) =
  Seq.for_all2 (=) s1 s2

let _ =
  assert (same_sequences (Seq.take 100 seq_primes) (Seq.take 100 gen_primes))

(** More general transformation from iterator to sequence. *)

let iterator_to_sequence
       (type elt) (type collection)
       (iter: (elt -> unit) -> collection -> unit) : collection -> elt Seq.t =
  let module I2S = struct
    type _ eff += Next : elt -> unit eff
    let gen coll =
      match iter (fun elt -> perform (Next elt)) coll with
      | () -> Seq.empty
      | effect Next elt, k -> fun () -> Seq.Cons(elt, continue k ())
  end in I2S.gen

(** Application: the "same fringe" problem. *)

let same_fringe
    (iter1: ('elt -> unit) -> 'coll1 -> unit)
    (iter2: ('elt -> unit) -> 'coll2 -> unit)
    coll1 coll2 =
  same_sequences (iterator_to_sequence iter1 coll1)
                 (iterator_to_sequence iter2 coll2)

module IntSet = Set.Make(Int)

let _ =
  assert (same_fringe List.iter IntSet.iter
              [1; 2; 3]
              (IntSet.add 2 (IntSet.add 1 (IntSet.singleton 3))))
