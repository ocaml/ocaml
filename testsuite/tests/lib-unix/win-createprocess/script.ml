(* TEST
 include unix;
 hasunix;
 target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* Test argument passing and quoting to a .cmd/.bat script *)

open Printf

let create_file name contents =
  Out_channel.with_open_text name (fun oc -> output_string oc contents)

let run prog args =
  try
    let pid = Unix.(create_process prog args stdin stdout stderr) in
    ignore (Unix.waitpid [] pid)
  with Failure s ->
    printf "Failure %s\n%!" s

let chars = {xxx| !"#$%&'()*+,-./0:;<=>?@A[\]^_`a{|}~|xxx}

let test_char arg =
  printf "Testing \"%s\"\n%!" arg;
  run ".\\echo.cmd" [|"echo.cmd"; arg; "X"|]

let _ =
  create_file "echo.cmd"
    "@echo off\necho %2 \"%~1\"\n";
  create_file "my (script).bat"
    "@echo off\necho Hello world!\n";
  String.iter
    (fun c -> test_char (String.make 1 c))
    chars;
  printf "Extra tests\n%!";
  run "./echo.cmd" [|"echo"; "1"; "2" |];
  run "my (script).bat" [|"dir"|]
