(* TEST *)

(* This test checks that we're not propagating the approximation of
   a lazy block down the path where it has been shortcut *)

let[@inline never][@local never] run () =
  let l = lazy (Sys.opaque_identity None) in
  let check () =
    match Lazy.force l with
    | None -> print_endline "Ok"
    | Some _ -> print_endline "Error"
  in
  (* First force the lazy value *)
  check ();
  (* Then trigger a minor GC to shortcut the evaluated lazy value *)
  Gc.minor ();
  (* Now check that the code above can cope with [l] being an immediate
     instead of a pointer *)
  check ()

let () = run ()
