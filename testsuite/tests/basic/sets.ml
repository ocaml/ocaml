(* TEST *)

module IntSet = Set.Make(struct type t = int let compare x y = x-y end)

let even = List.fold_right IntSet.add [0; -2; 2; 4; 6; -10] IntSet.empty

let odd = List.fold_right IntSet.add [9; -7; 5; 1; -3] IntSet.empty

let _ =
  for i = -10 to 10 do
    Printf.printf "%d  %B  %B\n" i (IntSet.mem i even) (IntSet.mem i odd)
  done

module PowerSet(BaseSet: Set.S)
               (SetOrd: functor(S: Set.S) -> Set.OrderedType) =
  Set.Make(SetOrd(BaseSet))

module IntSetSet = PowerSet(IntSet)(functor (S: Set.S) -> S)

let setofset = List.fold_right IntSetSet.add [even; odd] IntSetSet.empty

let _ =
  List.iter
    (fun s -> Printf.printf "%B\n" (IntSetSet.mem s setofset))
    [IntSet.empty; even; odd; IntSet.union even odd]

let _ =
  let l = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let set = List.fold_right IntSet.add l IntSet.empty in
  let l1 = IntSet.fold (fun elt accu -> elt :: accu) set [] in
  let l2 = IntSet.fold_descending (fun elt accu -> elt :: accu) set [] in
  Printf.printf "%B\n" (List.rev l = l1);
  Printf.printf "%B\n" (l = l2)

let _ = exit 0
