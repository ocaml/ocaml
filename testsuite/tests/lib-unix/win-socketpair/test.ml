(* TEST
 script = "sh ${test_source_directory}/has-afunix.sh";
 libwin32unix;
 include unix;
 hasunix;
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

(* Check that there is (almost certainly) no race condition in the
   PF_UNIX emulation code. If the same socket name in the filesystem
   is re-used, there will be an EADDRINUSE error. *)

let () =
  let n = 16 in
  for i = 0 to n do begin
      let fd0, fd1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      (* Unix.close fd0; Unix.close fd1 *)
      ignore fd0;
      ignore fd1
    end done

(* Check that bidirectional reading/writing works. *)

let () =
  [Unix.PF_UNIX]
  |> List.iter @@ fun socket_domain ->
  let fd0, fd1 = Unix.socketpair socket_domain Unix.SOCK_STREAM 0 in

  let msg0 = Bytes.of_string "42" and msg1 = Bytes.of_string "??" in
  ignore (Unix.write fd0 msg0 0 (Bytes.length msg0));
  ignore (Unix.read fd1 msg1 0 (Bytes.length msg1));
  assert (msg0 = msg1);

  let msg0 = Bytes.of_string "42" and msg1 = Bytes.of_string "??" in
  ignore (Unix.write fd1 msg0 0 (Bytes.length msg0));
  ignore (Unix.read fd0 msg1 0 (Bytes.length msg1));
  assert (msg0 = msg1);

  Unix.close fd0;
  Unix.close fd1
