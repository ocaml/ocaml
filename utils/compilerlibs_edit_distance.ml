(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Gabriel Radanne, projet Cambium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b, 'c, 'd) change =
  | Delete of 'a
  | Insert of 'b
  | Keep of 'a * 'b * 'c
  | Change of 'a * 'b * 'd

type ('a, 'b, 'c, 'd) patch = ('a, 'b, 'c, 'd) change list

let map f g = function
  | Delete x -> Delete (f x)
  | Insert x -> Insert (g x)
  | Keep (x,y,k) -> Keep (f x, g y, k)
  | Change (x,y,k) -> Change (f x, g y, k)

let min_assoc_list l =
  let rec aux m0 res0 = function
    | [] -> m0, res0
    | (m', res') :: t ->
        let m'', res'' = if m0 <= m' then m0,res0 else m',res' in
        aux m'' res'' t
  in
  match l with
  | [] -> invalid_arg "min_assoc_list: empty list"
  | (m,res)::l -> aux m res l

let compute_matrix ~weight ~cutoff ~test ~update state0 a1 a2 =
  let l1 = Array.length a1 in
  let l2 = Array.length a2 in
  assert (0 < cutoff && cutoff < max_int - l1 - 1);
  let m = Array.make_matrix (l1 + 1) (l2 + 1) (cutoff + 1) in
  let state = Array.make_matrix (l1 + 1) (l2 + 2) state0 in
  let add i j ~x ~s =
    state.(i).(j) <- s;
    m.(i).(j) <- x;
  in
  let results = Hashtbl.create (l1+l2) in

  add 0 0 ~x:0 ~s:state0;
  for i = 1 to l1 do
    let diff = Delete a1.(i-1) in
    add i 0 ~x:(weight diff + m.(i-1).(0)) ~s:(update diff state.(i-1).(0))
  done;
  for j = 1 to l2 do
    let diff = Insert a2.(j-1) in
    add 0 j ~x:(weight diff + m.(0).(j-1)) ~s:(update diff state.(0).(j-1))
  done;
  if abs (m.(l1).(0) - m.(0).(l2)) > cutoff then None
  else begin
    for i = 1 to l1 do
      for j = max 1 (i - cutoff - 1) to min l2 (i + cutoff + 1) do
        let propositions = [
          (let diff = Delete a1.(i-1) in
           weight diff + m.(i-1).(j), (diff, update diff state.(i-1).(j))
          );
          (let diff = Insert a2.(j-1) in
           weight diff + m.(i).(j-1), (diff, update diff state.(i).(j-1))
          );
          (let newres = test state.(i-1).(j-1) a1.(i-1) a2.(j-1) in
           let diff = match newres with
             | Ok ok -> Keep (a1.(i-1), a2.(j-1), ok)
             | Error err -> Change (a1.(i-1), a2.(j-1), err)
           in
           weight diff + m.(i-1).(j-1), (diff, update diff state.(i-1).(j-1))
          );
        ]
        in
        let best, (newres, newstate) = min_assoc_list propositions in
        Hashtbl.add results (i-1,j-1) newres;
        add i j ~x:best ~s:newstate
      done;
    done;
    Some results
  end

let construct_patch a1 a2 results =
  let rec aux acc (i, j) =
    if i > 0 && j > 0 then
      let d = Hashtbl.find results (i-1, j-1) in
      let next = match d with
        | Keep _ | Change _ -> (i-1, j-1)
        | Delete _ -> (i-1, j)
        | Insert _ -> (i, j-1)
      in
      aux (d::acc) next
    else if j > 0 && i = 0 then
      aux (Insert a2.(j-1) ::acc) (i, j-1)
    else if i > 0 && j = 0 then
      aux (Delete a1.(i-1) ::acc) (i-1, j)
    else
      acc
  in
  aux [] (Array.length a1, Array.length a2)

let diff ~weight ~cutoff ~test ~update state a1 a2 =
  Option.map
    (construct_patch a1 a2)
    (compute_matrix ~weight ~cutoff ~test ~update state a1 a2)
