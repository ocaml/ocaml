(* TEST *)

(* Testing handling of infix_tag by ephemeron *)

let infix n = let rec f () = n and g () = f () in g

(* Issue #9485 *)
let () =
  let w = Weak.create 1 in
  Weak.set w 0 (Some (infix 12));
  match Weak.get_copy w 0 with Some h -> ignore (h ()) | _ -> ()

(* Issue #7810 *)
let ephe x =
  let open Ephemeron.K1 in
  let e = make x 42 in
  Gc.full_major ();
  (x, query e x)

let () =
  assert (ephe (ref 1000) = (ref 1000, Some 42));
  match ephe (infix 12) with
  | (h, Some 42) -> ()
  | _ -> assert false
