(* TEST
* hasunix
include unix
** native
** bytecode
*)

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
  if d = 0 then Node(Empty, Empty)
  else let d = d - 1 in Node(make d, make d)

(* you need to use Gc.quick_stat, because Gc.stat forces a major cycle *)
let major_collections () =
  (Gc.quick_stat ()).major_collections

(* test to force domain to do a full GC while another is waiting *)
let _ =
  let sem = Semaphore.Binary.make false in
  let d = Domain.spawn (fun _ -> Semaphore.Binary.acquire sem) in
  Gc.full_major ();
  let n = major_collections () in
  ignore (make 22);
  assert ((major_collections ()) > n);
  Semaphore.Binary.release sem;
  Domain.join d;
  print_endline "wait OK"

(* test to force domain to do a full GC while another is blocking *)
let _ =
  let _ = Domain.spawn (fun _ ->
    Unix.sleep 10000
  ) in
  Gc.full_major ();
  let n = major_collections () in
  ignore (make 24);
  assert ((major_collections ()) > n);
  print_endline "sleep OK"
