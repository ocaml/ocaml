(* TEST
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
