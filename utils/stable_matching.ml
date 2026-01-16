(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Malo Monin, projet Cambium, Inria Paris                 *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type left_index = int
type right_index = int
type rank = int
type layer = { left_candidates: left_index list; pref:rank }



type ('a,'v) matches = {
  left : 'a list;
  pairs : ('v * 'v) list;
  right : 'a list;
}

module Item = struct
  type ('v, 'k) t = {
    name: string;
    item: 'v;
    kind: 'k;
  }

  let name f = f.name
  let item f = f.item
  let kind i = i.kind
end
type nonrec ('v,'k) item_matches =  (('v,'k) Item.t, 'v) matches

type ord = Keep | Eq | Change

let order x y = if x < y then Keep else if x = y then Eq else Change

type unstable_matching = {
  first:left_index * right_index;
  second: left_index * right_index;
  current_rank: rank * rank;
  optimal: rank * rank
}

let symmetric_strong_stable_match ~distance ((l,r),(l',r')) =
  let l_r_to_l'_r = order (distance l r) (distance l' r) in
  let l'_r'_to_l_r' = order (distance l' r') (distance l r') in
  match l_r_to_l'_r, l'_r'_to_l_r' with
  | Keep, _ | _, Keep | Eq, Eq -> Ok ()
  | Change, Eq | Change, Change | Eq, Change ->
      Error {
        first = l,r;
        second =  l',r';
        current_rank = distance l r, distance l' r';
        optimal = distance l' r, distance l r'
      }

let symmetric_weak_stable_match ~distance ((l,r),(l',r')) =
  let l_r_to_l'_r = order (distance l r) (distance l' r) in
  let l'_r'_to_l_r' = order (distance l' r') (distance l r') in
  match l_r_to_l'_r, l'_r'_to_l_r' with
  | Change, Change ->
      Error {
        first = l,r;
        second =  l',r';
        current_rank = distance l r, distance l' r';
        optimal = distance l' r, distance l r'
      }
  | _ -> Ok ()



let edit_distance ~cutoff name i r =
  let cutoff = cutoff name in
  let r = String.edit_distance ~limit:(1 + cutoff) name @@ Item.name r in
  if r > cutoff then None else Some (i,r)

let simple_preferences ~distance left name =
  let a =
    Array.of_seq
    @@ Seq.filter_map Fun.id
    @@ Seq.mapi (distance name)
    @@ Array.to_seq left in
  let () = Array.sort (fun (_,n) (_,n') -> Int.compare n n') a in
  a

let rec group_by current acc pos a () =
  if pos >= Array.length a then
    match acc with
    | [] -> Seq.Nil
    | _ -> Seq.Cons ({ left_candidates=acc; pref=current }, Seq.empty)
  else
    let x, dist = a.(pos) in
    if dist = current then
      group_by current (x::acc) (pos+1) a ()
    else if acc = [] then
      group_by dist [x] (pos+1) a ()
    else
      Seq.Cons (
        {left_candidates=acc; pref=current}, group_by dist [x] (pos+1) a
      )
let group_by a = group_by 0 [] 0 a

let gen_stable_matches variant ~distance matches  =
  let s = List.to_seq matches.pairs in
  let s = Seq.product s s in
  let find_error ppair =
    match variant ~distance ppair with
    | Error e -> Some e
    | Ok () -> None
  in
  match Seq.find_map find_error s with
  | Some e -> Error e
  | None -> Ok ()

let stable_matches ~distance matches =
  gen_stable_matches symmetric_weak_stable_match ~distance matches

let strong_stable_matches ~distance matches =
  gen_stable_matches symmetric_strong_stable_match ~distance matches


(** An implementation (in [diff]) of Zoltan Kiraly's "New Algorithm," presented
    in "Linear Time Local Approximation Algorithm for Maximum Stable Marriage":
    https://www.mdpi.com/1999-4893/6/3/471. It computes a 3/2-approximation of
    a maximum stable marriage in linear time (linear in the sum of the lengths
    of the preference lists). *)
module Stable_marriage_diff = struct

  (* This implementation does not use the same semantics as the original paper.
     Below is a conversion from the paper's terms to the implementation's terms:
     - woman: left
     - man: right
     - engaged (woman / man): paired
     - maiden (woman): unpaired
     - active (man): active
     - lad: first phase
     - bachelor: second phase
     - old bachelor: closed
     - uncertain (man): has other choices
     - flighty (woman): has a weak pair *)

  type distance = int

  module Tie_list = struct
    (* List of element tied at a given distance in the global preference list *)
    type t =
      | First_round of { front: int list; second_round:int list }
       (* During the first round, the list of ties is split in two:
         - [front], possibly unpaired left element
         - [second_round] certainly paired left elements
      *)

      | Second_round of int list

    let first_round front second_round = First_round { front; second_round }
    let in_first_round = function
      | First_round _ -> true
      | Second_round _ -> false
    let next tl = match tl with
      | Second_round [] -> None
      | Second_round (a::q) -> Some(a, Second_round q)
      | First_round ({ front = a :: front; _ } as dq) ->
          Some (a, First_round { dq with front })
      | First_round { front = []; second_round } ->
          match List.rev second_round with
          | [] -> None
          | a :: front -> Some (a, Second_round front)
    let delay_to_second_round tl x = match tl with
      | First_round dq ->
          First_round { dq with second_round = x :: dq.second_round }
      | Second_round _ -> tl
    let replace_front x = function
      | First_round dq -> First_round { dq with front = x :: dq.front }
      | Second_round l -> Second_round (x :: l)
    let of_list front = First_round { front; second_round = [] }
  end

  type left_state =
    | Left_unpaired
    | Left_paired of int * distance

  type right_phase =
    | First
    | Second

  type active_right_state = {
    mutable previous_layers : layer list;
        (** Invariant: this list is not empty in the first phase . *)
    mutable current_layer : Tie_list.t;
    mutable current_distance : distance;
    mutable paired: bool;
    mutable phase: right_phase;
    mutable next_layers : layer Seq.t;
  }

  type ('a,'b) state =
    { left: 'a array; right: 'b array; mutable reactivated:int list }

  let is_never_paired state j = match state.left.(j) with
    | Left_unpaired -> true
    | _ -> false

  let rec has_alternative_choices ~compatible state ir r =
    let cl = r.current_layer in
    match cl with
    | Second_round _ | First_round { front = [] | [_]; _ }-> false
    | First_round ({ front = a :: b :: q ; _ } as cl) ->
        if not (compatible b ir) then begin
          r.current_layer <- First_round { cl with front = a :: q};
          has_alternative_choices ~compatible state ir r
        end else
          is_never_paired state b ||
          let current_layer =
            Tie_list.First_round {
              front = a :: q;
              second_round = b :: cl.second_round
            }
          in
          r.current_layer <- current_layer;
          has_alternative_choices ~compatible state ir r

  let rec skip_paired state dq =
    assert (Tie_list.in_first_round dq);
    match Tie_list.next dq with
    | None -> assert false
    | Some (first,others) ->
        if is_never_paired state first then
          first, others
        else skip_paired state (Tie_list.delay_to_second_round others first)

  let has_weak_pair ~compatible state j =
    match state.left.(j) with
    | Left_unpaired -> false
    | Left_paired (i, _) ->
        match state.right.(i) with
        | None -> assert false
        | Some r -> has_alternative_choices ~compatible state i r

  let phase state i =
    Option.map (fun x -> x.phase) state.right.(i)

  let prepare_tie_list state { left_candidates=i; pref=d} r =
    let first, later = List.partition (is_never_paired state) i in
    let tie_list = Tie_list.first_round first later in
    match state.right.(r) with
    | None -> ()
    | Some r ->
        r.current_distance <- d;
        r.current_layer <- tie_list

  let second_phase state ir r =
    let layers = List.rev r.previous_layers in
    r.previous_layers <- [];
    r.phase <- Second;
    match layers with
    | [] -> assert false
    | layer :: q ->
        prepare_tie_list state layer ir;
        r.next_layers <- List.to_seq q

  let next_layer state ir r = match r.next_layers () with
    | Seq.Nil ->
        begin match r.phase with
        | First -> second_phase state ir r; true
        | Second -> false
        end
    | Seq.Cons(layer, next_layers) ->
        r.previous_layers <- layer :: r.previous_layers;
        r.next_layers <- next_layers;
        prepare_tie_list state layer ir;
        true

  let rec get_left_candidate ~compatible state ir r =
    assert (r.paired = false);
    if has_alternative_choices ~compatible state ir r then
      let f, others = skip_paired state r.current_layer in
      r.current_layer <- others;
      Some f
    else match Tie_list.next r.current_layer with
      | Some (f,others) ->
          r.current_layer <- others;
          Some f
      | None ->
          if next_layer state ir r then
            get_left_candidate ~compatible state ir r
          else None

  let rec get_compatible_left_candidate ~compatible state ir r =
    match get_left_candidate ~compatible state ir r with
    | None -> None
    | Some l as c ->
        if compatible l ir then c
        else
          get_compatible_left_candidate ~compatible state ir r

  let reject state i =
    match state.right.(i) with
    | None -> ()
    | Some right ->
        right.paired <- false;
        match Tie_list.next right.current_layer with
        | None -> state.right.(i) <- None
        | Some (f,others) ->
            let tie_list = Tie_list.delay_to_second_round others f in
            right.current_layer <- tie_list;
            state.reactivated <- i :: state.reactivated

  let accepted_proposal ~compatible state i j d =
    has_weak_pair ~compatible state j ||
    match state.left.(j) with
    | Left_unpaired -> true
    | Left_paired (i', d') ->
        d < d' ||
        d = d' &&
        match phase state i, phase state i' with
        | Some Second, Some First -> true
        | _ -> false

  let pair state i j d =
    begin match state.right.(i) with
    | None -> ()
    | Some r ->
      r.paired <- true;
      r.current_layer <- Tie_list.replace_front j r.current_layer
    end;
    match state.left.(j) with
    | Left_unpaired -> state.left.(j) <- Left_paired (i, d)
    | Left_paired (i', _) ->
        reject state i';
        state.left.(j) <- Left_paired (i, d)

  let init_right_state ~preferences right_size =
    Array.init right_size
      (fun r ->
         let sequence = preferences r in
         match sequence () with
         | Seq.Nil -> None
         | Seq.Cons (layer, tail) ->
             Some {
               paired = false;
               phase = First;
               current_distance = layer.pref;
               current_layer = Tie_list.of_list layer.left_candidates;
               previous_layers = [layer];
               next_layers = tail;
             }
      )


  let rec proposals ~compatible state i right =
    match get_compatible_left_candidate ~compatible state i right with
    | None -> ()
    | Some j ->
        if accepted_proposal ~compatible state i j right.current_distance then
          pair state i j right.current_distance
        else
          proposals ~compatible state i right

  let matches ~compatible ~preferences ~size:(lsize,rsize) =
    let left_state = Array.make lsize Left_unpaired in
    let preferences r = group_by (preferences r) in
    let right_state = init_right_state ~preferences rsize in
    let state = { left=left_state; reactivated = []; right=right_state } in
    let rec loop = function
      | [] ->
          begin  match state.reactivated with
          | [] -> ()
          | l -> state.reactivated <- []; loop l
          end
      | i :: l ->
        match state.right.(i) with
          | None -> loop l
          | Some right ->
              proposals ~compatible state i right;
              loop l
    in
    loop (List.init rsize Fun.id);
    let left, pairs = Seq.partition_map (fun (l, status) ->
        match status with
        | Left_unpaired -> Either.Left l
        | Left_paired (r,_) ->
            Either.Right (l, r)
      ) (Array.to_seqi left_state)
    in
    let unpaired (r,st) =
      match st with
      | Some rs -> if rs.paired then None else Some r
      | None -> Some r
    in
    {
      left = List.of_seq left;
      pairs = List.of_seq pairs;
      right = List.of_seq (Seq.filter_map unpaired @@ Array.to_seqi state.right)
    }

  let diff ~preferences ~compatible left right =
    let preferences = preferences left in
    let size =  Array.length left, Array.length right in
    let matches = matches ~compatible ~preferences ~size in
    let item_pair (l,r) = Item.item left.(l), Item.item right.(r) in
    {
      left = List.map (fun l -> left.(l)) matches.left;
      right = List.map (fun r -> right.(r)) matches.right;
      pairs = List.map item_pair matches.pairs;
    }

end

let rec cut_at before pos l =
  if pos <= 0 then List.rev before, l
  else match l with
    | [] -> List.rev before, []
    | a :: q -> cut_at (a::before) (pos-1) q

let matches = Stable_marriage_diff.matches

let fuzzy_match_names ~compatibility ~max_right_items ~cutoff left right =
  let right_pairing, right_rest = cut_at [] max_right_items right in
  let left = Array.of_list left in
  let right = Array.of_list right_pairing in
    let compatible i j =
      compatibility (Item.kind left.(i)) (Item.kind right.(j))
    in
    let preferences left r =
      simple_preferences ~distance:(edit_distance ~cutoff)
        left (Item.name right.(r))
    in
    let matches =
      Stable_marriage_diff.diff ~preferences ~compatible left right
    in
    { matches with right = matches.right @ right_rest }
