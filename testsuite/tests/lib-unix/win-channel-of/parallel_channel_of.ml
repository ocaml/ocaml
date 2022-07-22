(* TEST
modules = "fd_of_channel.c"
* libwin32unix
include unix
** bytecode
** native
*)

external fd_of_in_channel: in_channel -> int = "caml_fd_of_channel"

let getfd barrier descr () =
  while not (Atomic.get barrier) do Domain.cpu_relax() done;
  fd_of_in_channel (Unix.in_channel_of_descr descr)

let getfd_parallel descr =
  let barrier = Atomic.make false in
  let d1 = Domain.spawn (getfd barrier descr)
  and d2 = Domain.spawn (getfd barrier descr) in
  Unix.sleepf 0.05;
  Atomic.set barrier true;
  let fd1 = Domain.join d1
  and fd2 = Domain.join d2 in
  (fd1, fd2)

let test () =
  let descr = Unix.(openfile "tmp.txt" [O_RDWR; O_CREAT; O_TRUNC] 0o600) in
  let (fd1, fd2) = getfd_parallel descr in
  let (fd3, fd4) = getfd_parallel descr in
  Unix.close descr;
  Sys.remove "tmp.txt";
  assert (fd1 = fd2);
  assert (fd3 = fd4);
  assert (fd3 = fd1)

let _ =
  for _i = 1 to 50 do test() done
