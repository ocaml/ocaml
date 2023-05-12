(* TEST_BELOW
(* Blank lines added here to preserve locations. *)













*)

let () =
  Printexc.record_backtrace true;
  let c = open_out "foo" in
  close_out c;
  try
    while true do
      open_in "foo" |> ignore
    done
  with Sys_error _ ->
    (* The message is platform-specific, so convert the exception to Exit *)
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace Exit bt

(* TEST
 flags += "-g";
 exit_status = "2";
 {
   ocamlrunparam += ",b=0";
   reference = "${test_source_directory}/pr2195-nolocs.byte.reference";
   bytecode;
 }{
   ocamlrunparam += ",b=1";
   reference = "${test_source_directory}/pr2195-nolocs.byte.reference";
   bytecode;
 }{
   ocamlrunparam += ",b=2";
   reference = "${test_source_directory}/pr2195-locs.byte.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/pr2195.opt.reference";
   compare_programs = "false";
   native;
 }
*)
