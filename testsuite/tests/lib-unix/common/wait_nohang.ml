(* TEST

files = "reflector.ml"

* hasunix
** setup-ocamlc.byte-build-env
program = "${test_build_directory}/wait_nohang.byte"
*** ocamlc.byte
program = "${test_build_directory}/reflector.exe"
all_modules = "reflector.ml"
**** ocamlc.byte
include unix
program = "${test_build_directory}/wait_nohang.byte"
all_modules= "wait_nohang.ml"
***** check-ocamlc.byte-output
****** run
******* check-program-output

** setup-ocamlopt.byte-build-env
program = "${test_build_directory}/wait_nohang.opt"
*** ocamlopt.byte
program = "${test_build_directory}/reflector.exe"
all_modules = "reflector.ml"
**** ocamlopt.byte
include unix
program = "${test_build_directory}/wait_nohang.opt"
all_modules= "wait_nohang.ml"
***** check-ocamlopt.byte-output
****** run
******* check-program-output

*)

let refl =
  Filename.concat Filename.current_dir_name "reflector.exe"

let () =
  let (fd_exit, fd_entrance) = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process refl [|"refl"; "-i2o"|] 
                        fd_exit Unix.stdout Unix.stderr in
  Unix.close fd_exit;
  let (pid1, status1) = Unix.waitpid [WNOHANG] pid in
  assert (pid1 = 0 && status1 = WEXITED 0);
  Unix.close fd_entrance;
  let rec busywait () =
    let (pid2, status2) = Unix.waitpid [WNOHANG] pid in
    if pid2 = 0 then begin
      Unix.sleepf 0.001; busywait()
    end else begin
      assert (status2 = WEXITED 0); print_endline "OK"
    end
  in busywait()
