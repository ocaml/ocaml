(* TEST *)

(* Testing handling of infix_tag by ephemeron *)

(* This test will have to be ported to the new ephemeron API *)
[@@@alert "-old_ephemeron_api"]

let infix n = let rec f () = n and g () = f () in g

(* Issue #9485 *)
let () =
  let w = Weak.create 1 in
  Weak.set w 0 (Some (infix 12));
  match Weak.get_copy w 0 with Some h -> ignore (h ()) | _ -> ()

(* Issue #7810 *)
let ephe x =
  let open Ephemeron.K1 in
  let e = create () in
  set_key e x;
  set_data e 42;
  Gc.full_major ();
  (x, get_data e)

let () =
  assert (ephe (ref 1000) = (ref 1000, Some 42));
  match ephe (infix 12) with
  | (h, Some 42) -> ()
  | _ -> assert false
