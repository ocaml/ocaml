(* TEST
 *)

open Option

(* Test functor *)
let () =
  assert ((map (fun _ -> 2) (Some 1)) = (Some 2));
  assert ((map (fun _ -> 2) None) = None);
  assert (((fun x -> x + 1) <@@> (Some 1)) = (Some 2));
  assert (((fun _ -> 3) <@@> None) = None)

(* Test applicative *)
let () =
  assert (((Some (fun _ -> 2)) <*> (Some 1)) = (Some 2));
  assert (((Some (fun _ -> 2)) <*> None) = None);
  assert ((None <*> (Some 1)) = None);
  assert ((None <*> None) = None);
  assert ((return 1) = (Some 1))

(* Test monad *)
let () =
  assert ((bind (Some 1) (fun _ -> Some 2)) = (Some 2));
  assert ((bind None (fun _ -> Some 2)) = None);
  let f x = Some (x * 3) in
  let g x = Some (x - 5) in
  let h _ = None in
  assert ((compose_after g f 2) = (Some 1));
  assert ((compose_after g h 2) = None);
  assert ((compose_after h f 2) = None);
  assert ((compose_before f g 2) = (Some 1));
  assert ((compose_before h g 2) = None);
  assert ((compose_before f h 2) = None)

let () = print_endline "OK"
