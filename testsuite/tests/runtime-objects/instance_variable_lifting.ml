(* TEST *)

(* This test checks that a specific transformation takes place.
   If it fails by printing unexpected output, it means that
   the optimisation has been lost.
   If it fails with an exception (or segfault or other things),
   then some thing has gone terribly wrong and needs to be fixed.

   The transformation is about lifting immutable instance variable
   reads out of closures, to prevent these closures from capturing
   the whole self object when they only need a few of the fields.
   See also [self_capture.ml] in this directory for an actual case
   where this matters.
*)

class c = object
  val x : int = 0
  val mutable y : int = 1
  method mx = let () = () in fun () -> x
  method my = let () = () in fun () -> y
  method update_y v = y <- v
end

type liveness = Live | Dead

(* Equivalent to [work (alloc ())], but also checks
   if the result of [alloc] is still alive at the end. *)
let[@inline never] check_liveness ~alloc ~work =
  let r = ref Live in
  (* Switch context to a different function to make sure
     no roots are kept to temporary values *)
  let[@local never][@inline never] isolate () =
    let v = alloc () in
    Gc.finalise_last (fun () -> r := Dead) v;
    work v
  in
  let result = isolate () in
  Gc.full_major ();
  result, !r

let () =
  let get_x, live =
    check_liveness ~alloc:(fun () -> new c)
      ~work:(fun obj -> obj#mx)
  in
  assert (get_x () = 0);
  begin match live with
  | Dead -> ()
  | Live ->
    print_endline
      "Missed optimisation: Object 1 should have been collected"
  end;
  let (get_y, update_y), live =
    check_liveness ~alloc:(fun () -> new c)
      ~work:(fun obj -> (obj#my, obj#update_y))
  in
  assert (get_y () = 1);
  begin match live with
  | Live -> ()
  | Dead -> failwith "Object 2 should be still alive"
  end;
  update_y 42;
  assert (get_y () = 42)
