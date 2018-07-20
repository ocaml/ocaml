(* TEST
 *)

open Option

(* Test functor *)
let () =
  assert ((map (fun _ -> 2) (Some 1)) = (Some 2));
  assert ((map (fun _ -> 2) None) = None)

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
  assert ((bind None (fun _ -> Some 2)) = None)

let () = print_endline "OK"
