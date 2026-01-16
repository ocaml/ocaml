(* TEST *)

let rec c = lazy (0 + d) and d = 42;;

let () =
  Printf.printf "Cyclic thunk: %d\n" (Lazy.force c)

(* regression test for #13930 *)
let f x =
  let rec l =
    let v = lazy x in
    Gc.minor ();
    v
  in
  l

let () =
  Printf.printf "Forward shortcut regression: %d\n"
    (Lazy.force (f 84))
