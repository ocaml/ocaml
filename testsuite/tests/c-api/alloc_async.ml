(* TEST
   modules = "alloc_async_stubs.c"
*)

external test : int ref -> unit = "stub"

let f () =
  let r = ref 42 in
  Gc.finalise (fun s -> r := !s) (ref 17);
  Printf.printf "OCaml, before: %d\n%!" !r;
  test r;
  Printf.printf "OCaml, after: %d\n%!" !r;
  ignore (Sys.opaque_identity (ref 100));
  Printf.printf "OCaml, after alloc: %d\n%!" !r;
  ()

let () = (f [@inlined never]) ()
