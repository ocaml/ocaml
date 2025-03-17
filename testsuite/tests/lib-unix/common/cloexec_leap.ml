let status_checker = "fdstatus.exe"

let _ =
  let args = Array.copy Sys.argv in
  let image = Filename.concat Filename.current_dir_name status_checker in
  args.(0) <- status_checker;
  if Sys.argv.(1) = "execv" then
    let () =
      (* As in cloexec.ml, on Windows take out a write lock on a file so that
         fdstatus_main.ml can be sure that both ancestor processes have actually
         terminated before it tries to delete tmp.txt *)
      if Sys.win32 then
        let lock =
          Unix.(openfile "lock2.txt" [O_WRONLY; O_CREAT;
                                      O_TRUNC; O_CLOEXEC] 0o600) in
        Unix.lockf lock Unix.F_LOCK 0 in
      Unix.execv image args
  else
    let pid =
      Unix.create_process image args Unix.stdin Unix.stdout Unix.stderr in
    ignore (Unix.waitpid [] pid)
