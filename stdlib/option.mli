(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Option values.

    Option values explicitly indicate the presence or absence of a value. If the
    value exists, then the option will be [Some value]; otherwise, it will be
    [None].

    Option values are typically used with pattern matching to access the
    contents:

    {[
    let divide num denom =
      if denom = 0 then
        None
      else
        Some (num / denom)

    let () =
      match divide 2 4 with
      | None -> print_endline "Given 0!"
      | Some v -> Printf.printf "The result is %d." v
    ]}

    @since 4.08 *)

(** {1:options Options} *)

type 'a t = 'a option = None | Some of 'a (**)
(** The type for option values. Either [None] or a value [Some v]. *)

val none : 'a option
(** [none] is [None]. *)

val some : 'a -> 'a option
(** [some v] is [Some v]. *)

val value : 'a option -> default:'a -> 'a
(** [value o ~default] is [v] if [o] is [Some v] and [default] otherwise.

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.value ~default:5 o = 3);

    let o = None in
    assert (Option.value ~default:5 o = 5);
    ]} *)

val get : 'a option -> 'a
(** [get o] is [v] if [o] is [Some v] and raise otherwise.

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.get o = 3);

    let o = None in
    assert (Option.get o = 5); (* raises [Invalid_argument] *)
    ]}

    @raise Invalid_argument if [o] is [None]. *)

val bind : 'a option -> ('a -> 'b option) -> 'b option
(** [bind o f] is [f v] if [o] is [Some v] and [None] if [o] is [None].

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.bind o (fun x -> Some (succ x)) = Some 4);
    assert (Option.bind o (fun _ -> None) = None);

    let o = None in
    assert (Option.bind o (fun x -> Some (succ x)) = None);
    assert (Option.bind o (fun _ -> None) = None);
    ]} *)

val join : 'a option option -> 'a option
(** [join oo] is [Some v] if [oo] is [Some (Some v)] and [None] otherwise.

    {b Examples}
    {[
    let oo = Some (Some 3) in
    assert (Option.join oo = Some 3);

    let oo = Some None in
    assert (Option.join oo = None);

    let oo = None in
    assert (Option.join oo = None);
    ]} *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** [map f o] is [None] if [o] is [None] and [Some (f v)] if [o] is [Some v].

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.map succ o = Some 4);

    let o = None in
    assert (Option.map succ o = None);
    ]} *)

val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a
(** [fold ~none ~some o] is [none] if [o] is [None] and [some v] if [o] is
    [Some v].

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.fold ~none:0 ~some:succ o = 4);
    assert (Option.(fold ~none ~some) o = (Some 3));

    let o = None in
    assert (Option.fold ~none:0 ~some:succ o = 0);
    assert (Option.(fold ~none ~some) o = None);
    ]} *)

val iter : ('a -> unit) -> 'a option -> unit
(** [iter f o] is [f v] if [o] is [Some v] and [()] otherwise.

    {b Examples}
    {[
    let count = ref 0 in
    let set_count x = count := x in

    let o = Some 3 in
    Option.iter set_count o;
    assert (!count = 3);

    let o = None in
    Option.iter set_count o;
    assert (!count = 3);
    ]} *)

(** {1:preds Predicates and comparisons} *)

val is_none : 'a option -> bool
(** [is_none o] is [true] if and only if [o] is [None].

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.is_none o = false);

    let o = None in
    assert (Option.is_none o = true);
    ]} *)

val is_some : 'a option -> bool
(** [is_some o] is [true] if and only if [o] is [Some o].

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.is_some o = true);

    let o = None in
    assert (Option.is_some o = false);
    ]} *)

val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
(** [equal eq o0 o1] is [true] if and only if [o0] and [o1] are both [None]
    or if they are [Some v0] and [Some v1] and [eq v0 v1] is [true]. *)

val compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
(** [compare cmp o0 o1] is a total order on options using [cmp] to compare
    values wrapped by [Some _]. [None] is smaller than [Some _] values. *)

(** {1:convert Converting} *)

val to_result : none:'e -> 'a option -> ('a, 'e) result
(** [to_result ~none o] is [Ok v] if [o] is [Some v] and [Error none]
    otherwise.

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.to_result ~none:6 o = Ok 3);

    let o = None in
    assert (Option.to_result ~none:6 o = Error 6);
    ]} *)

val to_list : 'a option -> 'a list
(** [to_list o] is [[]] if [o] is [None] and [[v]] if [o] is [Some v].

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.to_list o = [3]);

    let o = None in
    assert (Option.to_list o = []);
    ]} *)

val to_seq : 'a option -> 'a Seq.t
(** [to_seq o] is [o] as a sequence. [None] is the empty sequence and
    [Some v] is the singleton sequence containing [v].

    {b Examples}
    {[
    let o = Some 3 in
    assert (Option.to_seq o |> List.of_seq = [3]);

    let o = None in
    assert (Option.to_seq o |> List.of_seq = []);
    ]} *)
