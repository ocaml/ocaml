(* TEST
* libwin32unix
include unix
** has_symlink
*** bytecode
*** native
*)

let create_symlink barrier src dst () =
  while not (Atomic.get barrier) do Domain.cpu_relax() done;
  Unix.symlink ~to_dir:false src dst

let create_symlink_parallel src name1 name2 =
  let barrier = Atomic.make false in
  let d1 = Domain.spawn (create_symlink barrier src name1)
  and d2 = Domain.spawn (create_symlink barrier src name2) in
  Unix.sleepf 0.05;
  Atomic.set barrier true;
  Domain.join d1;
  Domain.join d2

let test () =
  let descr = Unix.(openfile "tmp.txt" [O_RDWR; O_CREAT; O_TRUNC] 0o600) in
  create_symlink_parallel "tmp.txt" "link1.txt" "link2.txt";
  create_symlink_parallel "tmp.txt" "link3.txt" "link4.txt";
  assert (Unix.write_substring descr "test" 0 4 = 4);
  let in_ = In_channel.open_text "link4.txt" in
  assert (In_channel.input_all in_ = "test");
  In_channel.close in_;
  Unix.close descr;
  Sys.remove "tmp.txt";
  Sys.remove "link1.txt";
  Sys.remove "link2.txt";
  Sys.remove "link3.txt";
  Sys.remove "link4.txt";
;;

let _ =
  if Unix.has_symlink () then
    for _ = 1 to 50 do test () done
