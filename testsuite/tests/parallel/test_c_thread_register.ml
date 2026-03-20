(* TEST
 modules = "test_c_thread_register_cstubs.c";
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* Spawns a external thread from C and register it to the OCaml runtime
   using caml_c_thread_register *)

external spawn_thread : (unit -> unit) -> unit = "spawn_thread"

(* Spawns a external thread from C and register it to the OCaml runtime in
   given domain using caml_c_thread_register_in_domain *)

external spawn_thread_specific_domain : int -> (unit -> unit) -> unit
  = "spawn_thread_specific_domain"

(* Spawns a external thread from C and register it to the OCaml runtime twice,
   in given domains using caml_c_thread_register_in_domain *)

external spawn_thread_specific_domain_twice : int ->
  int -> (unit -> unit) -> unit = "spawn_thread_specific_domain_twice"

let print_domain () =
  Printf.printf "Thread running in domain %d\n%!" (Domain.self () :> int)

let run_in_domain f =
  let d = Domain.spawn (fun () ->
    begin
      f ();
      Thread.delay 0.25
    end)
  in
  let t = Thread.create (fun () -> Thread.delay 0.5) () in
  Thread.join t;
  Domain.join d

(* This test assumes that no other domains are spawned, besides the ones
   spawned explitcly by [run_in_domain], during execution. This allows us
   to refer to the unique IDs directly and not have to use [Domain.self]. *)
let _ =
  (* Test caml_c_thread_register (which must always register in domain 0) *)
  run_in_domain (fun _ -> spawn_thread print_domain);

  (* Test caml_c_thread_register_in_domain, registering in domain 0. *)
  run_in_domain (fun _ -> spawn_thread_specific_domain 0 print_domain);

  (* Test caml_c_thread_register_in_domain, registering in the temporary
     domain, which has unique ID 3. *)
  run_in_domain (fun () -> spawn_thread_specific_domain 3 print_domain);

  (* Test caml_c_thread_register_in_domain, attempting to register in previous
     existent domain 3 which no longer exists. *)
  run_in_domain (fun () -> spawn_thread_specific_domain 3 print_domain);

  (* Test caml_c_thread_register_in_domain, registering in domain that has
     never existed. *)
  run_in_domain (fun () -> spawn_thread_specific_domain 6 print_domain);

  (* Test caml_c_thread_register_in_domain, registering in domain and
     then another domain. *)
  run_in_domain (fun () -> spawn_thread_specific_domain_twice 6 0 print_domain)
