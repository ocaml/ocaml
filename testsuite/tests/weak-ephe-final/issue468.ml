(* TEST *)

let tree_size = try int_of_string Sys.argv.(1) with _ -> 9
let iterations = try int_of_string Sys.argv.(2) with _ -> 1000
let num_domains = try int_of_string Sys.argv.(3) with _ -> 4

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
  if d = 0 then Node(Empty, Empty)
  else let d = d - 1 in Node(make d, make d)

let rec check = function Empty -> 0 | Node(l, r) -> 1 + check l + check r

let work () =
  for _ = 0 to iterations do
    let v = make tree_size in
    Gc.finalise (fun v -> ignore @@ check v) v
  done

let _ =
  let domains = Array.init (num_domains - 1) (fun _ -> Domain.spawn(work)) in
  work ();
  Array.iter Domain.join domains