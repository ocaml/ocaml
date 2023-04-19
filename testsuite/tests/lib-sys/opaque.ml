(* TEST *)

let[@inline never] float_unboxing s f =
  let x = Sys.opaque_identity (s +. 1.) in
  let mw1 = Gc.minor_words () in
  let mw2 = Gc.minor_words () in
  f x;
  let mw3 = Gc.minor_words () in
  Printf.printf "unbox: %.0f\n" ((mw3 -. mw2) -. (mw2 -. mw1))

let[@inline never] lifetimes () =
  let final = ref false in
  let go () =
    let r = ref 42 in
    Gc.finalise (fun _ -> final := true) r;
    let f1 = !final in
    Gc.full_major ();
    let f2 = !final in
    ignore (Sys.opaque_identity r);
    (f1, f2) in
  let (f1, f2) = go () in
  Gc.full_major ();
  let f3 = !final in
  Printf.printf "lifetime: %b %b %b\n" f1 f2 f3

let[@inline never] dead_alloc a =
  let mw1 = Gc.minor_words () in
  let mw2 = Gc.minor_words () in
  ignore (Sys.opaque_identity (a, a));
  let mw3 = Gc.minor_words () in
  Printf.printf "dead: %.0f\n" ((mw3 -. mw2) -. (mw2 -. mw1))


let () =
  float_unboxing 50. (fun _ -> ());
  lifetimes ();
  dead_alloc 10
