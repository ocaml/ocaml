(* TEST

   arguments = "${test_build_directory}/testfile.tmp";
*)

(* Test that output to a closed out_channel triggers an exception every
   time, not just the first time. *)

let () =
  let oc = open_out_bin Sys.argv.(1) in
  close_out oc;
  begin match output_byte oc 0 with
  | exception Sys_error _ -> ()
  | () -> assert false
  end;
  begin match output_byte oc 0 with
  | exception Sys_error _ -> ()
  | () -> assert false
  end
