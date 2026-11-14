(* TEST *)

(* https://github.com/ocaml/ocaml/issues/15259

   When its argument raises, [Hashtbl.filter_map_inplace] must leave the
   table consistent: the bindings already removed are gone, and
   [Hashtbl.length] is the number of bindings left in the table. *)

(* All the keys share one bucket, which is traversed starting from the
   most recently added binding. *)
module H = Hashtbl.Make (struct
  type t = int
  let equal = Int.equal
  let hash _ = 0
end)

let bindings h =
  List.sort compare (List.of_seq @@ H.to_seq h)

(* The reproducer from the issue: 2 is removed at the head of the bucket,
   then the function raises on 1. *)
let () =
  let h = H.create 16 in
  H.add h 0 0; H.add h 1 1; H.add h 2 2;
  for _ = 1 to 4 do
    (try H.filter_map_inplace
           (fun k _ -> if k = 2 then None else failwith "boom") h
     with Failure _ -> ())
  done;
  assert (H.length h = 2);
  assert (bindings h = [(0, 0); (1, 1)])

(* Removal after a binding that is kept: 1 is updated, 2 is removed, then
   the function raises on 3. *)
let () =
  let h = H.create 16 in
  H.add h 3 3; H.add h 2 2; H.add h 1 1;
  (try H.filter_map_inplace
         (fun k _ ->
            match k with
            | 1 -> Some 10
            | 2 -> None
            | _ -> failwith "boom") h
   with Failure _ -> ());
  assert (H.length h = 2);
  assert (bindings h = [(1, 10); (3, 3)])
