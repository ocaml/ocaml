(* TEST
unset FOO
unset FOO2
include unix
flags += "-strict-sequence -w +A-70 -warn-error +A"
modules = "stubs.c"
* libwin32unix
** bytecode
** native
*)

external set_environment_variable: string -> string -> unit
  = "stub_SetEnvironmentVariable"

let find_env s =
  let env = Unix.environment () in
  let rec loop i =
    if i >= Array.length env then
      None
    else begin
      let e = env.(i) in
      let pos = String.index e '=' in
      if String.sub e 0 pos = s then
        Some (String.sub e (pos+1) (String.length e - pos - 1))
      else
        loop (i+1)
    end
  in
  loop 0

let print title = function
  | None ->
      Printf.printf "%s -> None\n%!" title
  | Some s ->
      Printf.printf "%s -> Some %S\n%!" title s

let () =
  set_environment_variable "FOO" "BAR";
  Unix.putenv "FOO2" "BAR2";
  print "Sys.getenv FOO" (Sys.getenv_opt "FOO");
  print "Unix.environment FOO" (find_env "FOO");
  print "Sys.getenv FOO2" (Sys.getenv_opt "FOO2")
