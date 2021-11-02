(* TEST
*)

(* This test is only relevant to the old ephemeron API *)
[@@@alert "-old_ephemeron_api"]

let debug = false

open Printf
open Ephemeron

let empty = ref 0
let make_ra ~size = Array.init size (fun _ -> ref 1) [@@inline never]
let make_ephes ~size = Array.init size (fun _ -> Ephemeron.K1.create ()) [@@inline never]

let test ~size ~slice =
  let keys1 = make_ra ~size in
  let keys2 = make_ra ~size in
  let datas1 = make_ra ~size in
  let datas2 = make_ra ~size in
  let ephe1 = make_ephes ~size in
  let ephe2 = make_ephes ~size in
  if debug then Gc.set { (Gc.get ()) with Gc.verbose = 0x3 };
  (** Fill ephe.(i )from key.(i) to data.(i) *)
  for i=0 to size-1 do Ephemeron.K1.set_key  ephe1.(i) keys1.(i); done;
  for i=0 to size-1 do Ephemeron.K1.set_data ephe1.(i) datas1.(i); done;
  for i=0 to size-1 do Ephemeron.K1.set_key  ephe2.(i) keys2.(i); done;
  for i=0 to size-1 do Ephemeron.K1.set_data ephe2.(i) datas2.(i); done;
  (** Push everything in the major heap *)
  if debug then Printf.eprintf "Start minor major\n%!";
  Gc.minor ();
  Gc.major ();
  if debug then Printf.eprintf "start emptying\n%!";
  for i=0 to size-1 do keys1.(i) <- empty; done;
  for i=0 to size-1 do datas1.(i) <- empty; done;
  (** The emptying is done during a major so keys and data are kept alive by the
     assignments. Restart a new major *)
  Gc.major ();
  if debug then Printf.eprintf "Start checking state\n%!";
  (** Fill the ephemeron with an alive key *)
  if debug then Printf.eprintf "Start replacing dead key into alive one\n%!";
  (* Printf.eprintf "put in set (2) %i\n%!" (Gc.major_slice (10*4*slice*6)); *)
  for i=0 to size-1 do
    ignore (Gc.major_slice (4));
    if debug then Printf.eprintf "@%!";
    Ephemeron.K1.blit_data ephe1.(i) ephe2.(i);
    if debug && 0 = i mod (size / 10) then Printf.eprintf "done %5i/%i\n%!" i size;
  done;
  if debug then   Printf.eprintf "end\n%!";
  (** Finish all, assertion in clean phase should not find a dangling data *)
  Gc.full_major ();
  let r = ref 0 in
  if debug then
    for i=0 to size-1 do
      if Ephemeron.K1.check_data ephe2.(size-1-i) then incr r;
      if 0 = i mod (size / 10) then Printf.eprintf "done %5i/%i %i\n%!" i size !r;
    done;
  (* keep the arrays alive *)
  assert (Array.length keys1 = size);
  assert (Array.length keys2 = size);
  assert (Array.length datas1 = size);
  assert (Array.length datas2 = size);
  assert (Array.length ephe1 = size);
  assert (Array.length ephe2 = size)
[@@inline never]

let () =
  test ~size:1000 ~slice:5;
  test ~size:1000 ~slice:10;
  test ~size:1000 ~slice:15
