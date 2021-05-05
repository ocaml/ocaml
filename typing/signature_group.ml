(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, Inria Paris                        *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Fold on a signature by syntactic group of items *)

(** Classes and class types generate ghosts signature items, we group them
    together before printing *)
type sig_item =
  {
    src: Types.signature_item;
    post_ghosts: Types.signature_item list
    (** ghost classes types are post-declared *);
  }
let flatten x = x.src :: x.post_ghosts

type core_rec_group =
  | Not_rec of sig_item
  | Rec_group of sig_item list

let rec_items = function
  | Not_rec x -> [x]
  | Rec_group x -> x

(** Private row types are manifested as a sequence of definitions
    preceding a recursive group, we collect them and separate them from the
    syntatic recursive group. *)
type rec_group =
  { pre_ghosts: Types.signature_item list; group:core_rec_group }

let take n seq =
  let rec aux l rem n seq =
    if n = 0 then List.rev l, rem, seq else
      match seq () with
      | Seq.Nil -> assert false
      | Seq.Cons((x,rem),next) ->
          aux (x::l) rem (n-1) next
  in
  aux [] [] n seq

let rec partial_lists l () = match l with
  | [] -> Seq.Nil
  | a :: q -> Seq.Cons((a,q), partial_lists q)

let rec item_seq seq () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons((x,q), seq) ->
    match x with
    | Types.Sig_class _ as src ->
        let ghosts, q, seq = take 3 seq in
        (* a class declaration for [c] is followed by the ghost
           declarations of class type [c], and types [c] and [#c] *)
        Seq.Cons(({ src; post_ghosts=ghosts},q), item_seq seq)
    | Types.Sig_class_type _ as src ->
        let ghosts, q, seq = take 2 seq in
        (* a class type declaration for [ct] is followed by the ghost
           declarations of types [ct] and [#ct] *)
        Seq.Cons(({src; post_ghosts = ghosts},q), item_seq seq)
    | Types.(Sig_module _ | Sig_value _ | Sig_type _ | Sig_typext _
            | Sig_modtype _ as src) ->
        Seq.Cons(({src; post_ghosts=[]},q), item_seq seq)


let recursive_sigitem = function
  | Types.Sig_type(ident, _, rs, _)
  | Types.Sig_class(ident,_,rs,_)
  | Types.Sig_class_type (ident,_,rs,_)
  | Types.Sig_module(ident, _, _, rs, _) -> Some (ident,rs)
  | Types.(Sig_value _ | Sig_modtype _ | Sig_typext _ )  -> None

let group_seq x =
  let cons_group q pre group seq =
    let group = Rec_group (List.rev group) in
    Seq.Cons(({ pre_ghosts=List.rev pre; group },q), seq)
  in
  let rec not_in_group pre seq () = match seq () with
    | Seq.Nil ->
        assert (pre=[]);
        Seq.Nil
    | Seq.Cons((elt,q), seq) ->
        match recursive_sigitem elt.src with
        | Some (id, _) when Btype.is_row_name (Ident.name id) ->
            not_in_group (elt.src::pre) seq ()
        | None | Some (_, Types.Trec_not) ->
            let sgroup = { pre_ghosts=List.rev pre; group=Not_rec elt } in
            Seq.Cons((sgroup,q), not_in_group [] seq)
        | Some (id, Types.(Trec_first | Trec_next) )  ->
            in_group q ~pre ~ids:[id] ~group:[elt] seq ()
  and in_group q ~pre ~ids ~group seq () = match seq () with
    | Seq.Nil ->
        cons_group [] pre group (fun () -> Seq.Nil)
    | Seq.Cons((elt,qnext),next) ->
        match recursive_sigitem elt.src with
        | Some (id, Types.Trec_next) ->
            in_group qnext ~pre ~ids:(id::ids) ~group:(elt::group) next ()
        | None | Some (_, Types.(Trec_not|Trec_first)) ->
            cons_group q pre group (not_in_group [] seq)
  in
  not_in_group [] x

let full_seq l = l |> partial_lists |> item_seq |> group_seq
let seq l = Seq.map fst (full_seq l)
let iter f l = Seq.iter f (seq l)
let fold f acc l = Seq.fold_left f acc (seq l)
