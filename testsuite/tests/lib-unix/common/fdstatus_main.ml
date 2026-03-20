external process_and_close_fd : int -> string -> unit = "caml_process_fd"
external delete_on_close : string -> unit = "caml_win32_delete_on_close"

let () =
  if Sys.win32 then
    (* Ensure the ancestor process has definitely terminated (and therefore
       closed its handles to tmp.txt) *)
    let wait_until file =
      if Sys.file_exists file then
        let fd = Unix.openfile file [O_RDWR] 0o600 in
        Unix.lockf fd Unix.F_LOCK 0;
        Unix.close fd;
        Sys.remove file
    in
    wait_until "lock.txt"

let () =
  (* Windows virus scanning can easily get in the way here on slower VMs. When
     the file is closed, systems such as Windows Defender may trigger and open
     the file, preventing its deletion. There are various mitigations - the
     typical one is to use a retry-loop for a couple of seconds, but given that
     this really is a temporary file and we already have a C stub for the fd
     checking, we use a slightly different approach and instead open the file
     using FILE_FLAG_DELETE_ON_CLOSE and let Windows take care of removing the
     file - after Windows Defender at al have "finished" with it *)
  if Sys.argv.(1) = "execv" && Sys.win32 then
    delete_on_close "tmp.txt";
  for i = 2 to (Array.length Sys.argv) -1
  do
    process_and_close_fd (i - 1) Sys.argv.(i);
  done;
  (* For the execv version of the test, clean-up tmp.txt - for the
     Unix.create_process version, this is done by cloexec.ml *)
  if Sys.argv.(1) = "execv" && not Sys.win32 then
    Sys.remove "tmp.txt"
