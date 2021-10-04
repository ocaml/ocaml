(* TEST

* libwin32unix
   script = "sh ${test_source_directory}/has-afunix.sh"
** hassysthreads
    include systhreads
*** script
**** bytecode
output = "${test_build_directory}/program-output"
stdout = "${output}"
**** native
output = "${test_build_directory}/program-output"
stdout = "${output}"

 *)

let peer id fd =
  let msg = Bytes.of_string (Printf.sprintf "%d" id) in
  ignore (Unix.write fd msg 0 (Bytes.length msg));
  ignore (Unix.read fd msg 0 (Bytes.length msg));
  let expected = Bytes.of_string (Printf.sprintf "%d" (if id = 0 then 1 else 0)) in
  if msg = expected then
    Printf.printf "Ok\n%!"
  else
    Printf.printf "%d: %s\n%!" id (Bytes.to_string msg);
  flush_all ()

let () =
  let fd0, fd1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let t0, t1 = Thread.create (peer 0) fd0, Thread.create (peer 1) fd1 in
  Thread.join t0; Thread.join t1;
  Unix.close fd0; Unix.close fd1
