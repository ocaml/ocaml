(* TEST
 modules = "getpid.c";
 script = "sh ${test_source_directory}/has-afunix.sh";
 include systhreads;
 hassysthreads;
 target-windows;
 script;
 {
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   bytecode;
 }{
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   native;
 }
*)

external get_current_process_id : unit -> int
  = "caml_test_get_current_process_id"

(* Check that data flows in both directions. *)

let check_pair fd0 fd1 =
  let exchange src dst =
    let msg0 = Bytes.of_string "42" and msg1 = Bytes.of_string "??" in
    assert (Unix.write src msg0 0 (Bytes.length msg0) = Bytes.length msg0);
    assert (Unix.read dst msg1 0 (Bytes.length msg1) = Bytes.length msg1);
    assert (msg0 = msg1)
  in
  exchange fd0 fd1;
  exchange fd1 fd0

let () =
  let fd0, fd1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  check_pair fd0 fd1;
  Unix.close fd0;
  Unix.close fd1;
  print_endline "Ok"

(* Only PF_UNIX is supported by the emulation. *)

let () =
  match Unix.socketpair Unix.PF_INET Unix.SOCK_STREAM 0 with
  | _ -> print_endline "PF_INET: unexpected success"
  | exception Unix.Unix_error (Unix.EAFNOSUPPORT, "socketpair", _) ->
      print_endline "Ok"

(* Check that a file left over with the name of the next socket (e.g.,
   by a killed process with the same pid) is neither fatal nor
   deleted. The names are generated from the pid and a counter. *)

let () =
  let temp_dir =
    (* Same lookup order as GetTempPath *)
    match Sys.getenv_opt "TMP", Sys.getenv_opt "TEMP" with
    | Some dir, _ | None, Some dir -> dir
    | None, None -> Filename.get_temp_dir_name ()
  in
  let stale =
    Filename.concat temp_dir
      (Printf.sprintf "ocaml_sp_%08x_%08x" (get_current_process_id ()) 1)
  in
  close_out (open_out stale);
  let fd0, fd1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  check_pair fd0 fd1;
  Unix.close fd0;
  Unix.close fd1;
  assert (Sys.file_exists stale);
  Sys.remove stale;
  print_endline "Ok"

(* Check that there is (almost certainly) no race condition in the
   PF_UNIX emulation code when several threads create socket pairs
   concurrently. If the same socket name in the filesystem is re-used,
   there will be an EADDRINUSE error. *)

let () =
  let nthreads = 8 and iterations = 64 in
  let failures = Atomic.make 0 in
  let worker () =
    for _ = 1 to iterations do
      match Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 with
      | fd0, fd1 ->
          check_pair fd0 fd1;
          Unix.close fd0;
          Unix.close fd1
      | exception Unix.Unix_error (err, fn, _) ->
          Atomic.incr failures;
          Printf.printf "%s: %s\n%!" fn (Unix.error_message err)
    done
  in
  List.init nthreads (fun _ -> Thread.create worker ())
  |> List.iter Thread.join;
  if Atomic.get failures = 0 then print_endline "Ok"
