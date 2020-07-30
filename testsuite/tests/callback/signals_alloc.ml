(* TEST
   include unix
   modules = "callbackprim.c"
   * libunix
   ** bytecode
   ** native
*)
external raise_sigusr1 : unit -> unit = "raise_sigusr1"

let do_test () =
  let seen_states = Array.make 5 (-1) in
  let pos = ref 0 in
  let sighandler signo =
    (* These two instructions are duplicated everywhere, but we cannot
       encapsulate them in a function, because function calls check
       for signals in bytecode mode. *)
    seen_states.(!pos) <- 3; pos := !pos + 1;
  in
  seen_states.(!pos) <- 0; pos := !pos + 1;
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle sighandler);
  seen_states.(!pos) <- 1; pos := !pos + 1;
  raise_sigusr1 ();
  seen_states.(!pos) <- 2; pos := !pos + 1;
  let _ = Sys.opaque_identity (ref 1) in
  seen_states.(!pos) <- 4; pos := !pos + 1;
  Sys.set_signal Sys.sigusr1 Sys.Signal_default;
  Array.iter (Printf.printf "%d") seen_states;
  print_newline ()

let () =
  for _ = 0 to 10 do do_test () done;
  Printf.printf "OK\n"
