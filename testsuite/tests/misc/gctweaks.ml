(* TEST *)

let params () =
  String.split_on_char ',' (Sys.runtime_parameters ())

let has_param p =
  List.exists (String.starts_with ~prefix:("X"^p)) (params ())

let () =
  (match Gc.Tweak.get "blorp" with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  (match Gc.Tweak.set "blorp" 100 with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  assert (not (has_param "mark_stack_prune_factor"));
  let def = Gc.Tweak.get "mark_stack_prune_factor" in
  Gc.Tweak.set "mark_stack_prune_factor" 100;
  assert (List.mem "Xmark_stack_prune_factor=100" (params ()));
  Printf.printf "%d\n" (Gc.Tweak.get "mark_stack_prune_factor");
  (match Gc.Tweak.list_active () with
   | ["mark_stack_prune_factor", 100] -> ()
   | _ -> assert false);
  Gc.Tweak.set "mark_stack_prune_factor" def;
  assert (not (has_param "mark_stack_prune_factor"));
  assert (Gc.Tweak.list_active () = []);
  Printf.printf "ok\n";
  ()
