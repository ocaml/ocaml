(* TEST
 include ocamlcommon;
 include ocamlbytecomp;
 readonly_files = "test_module.ml test_module.mli trigger_lazy.ml";
 setup-ocamlc.byte-build-env;

 (* First compilation: .mli and .ml separately *)
 flags = "-g";
 all_modules = "test_module.mli";
 compile_only = "true";
 ocamlc.byte;
 all_modules = "test_module.ml";
 ocamlc.byte;
 src = "test_module.cmo";
 dst = "test_module_solo.cmo";
 copy;


 (* Second compilation: all files together *)
 all_modules = "trigger_lazy.ml test_module.mli test_module.ml";
 ocamlc.byte;
 src = "test_module.cmo";
 dst = "test_module_with_mli.cmo";
 copy;

 (* Now run the comparison test *)
 {
   compile_only = "false";
   all_modules = "determinism.ml";
   program = "${test_build_directory}/determinism.exe";
   ocamlc.byte;
   run;
   check-program-output;
 }
*)

(* Test that .cmo files are identical except for debug info
   when compiled separately vs together with .mli *)

let load_cmo_and_debug filename =
  let ic = open_in_bin filename in
  (* Skip magic number *)
  let _ = really_input_string ic (String.length Config.cmo_magic_number) in
  (* Read compilation unit *)
  let compunit_pos = input_binary_int ic in
  seek_in ic compunit_pos;
  let cu : Cmo_format.compilation_unit = input_value ic in

  (* Load debug data if present *)
  let debug_data =
    if cu.cu_debug > 0 then begin
      seek_in ic cu.cu_debug;
      let events : Instruct.debug_event list = input_value ic in
      let dirs : string list = input_value ic in
      Some (events, dirs)
    end else
      None
  in
  close_in ic;
  (cu, debug_data)

let () =
  let (cu1, debug1) = load_cmo_and_debug "test_module_solo.cmo" in
  let (cu2, debug2) = load_cmo_and_debug "test_module_with_mli.cmo" in

  (* Check that all non-debug-content fields are identical *)
  if cu1.cu_name <> cu2.cu_name
     || cu1.cu_imports <> cu2.cu_imports
     || cu1.cu_required_compunits <> cu2.cu_required_compunits
     || cu1.cu_primitives <> cu2.cu_primitives
     || cu1.cu_force_link <> cu2.cu_force_link
     || cu1.cu_debug <> cu2.cu_debug  (* offset should be the same *)
  then
    failwith "Non-debug fields differ!";

  (* Check if the files are byte-for-byte identical *)
  let load_file filename =
    let ic = open_in_bin filename in
    let size = in_channel_length ic in
    let bytes = Bytes.create size in
    really_input ic bytes 0 size;
    close_in ic;
    bytes
  in
  let bytes1 = load_file "test_module_solo.cmo" in
  let bytes2 = load_file "test_module_with_mli.cmo" in
  if bytes1 = bytes2 then
    print_endline "CMO files are identical"
  else
    print_endline "CMO files differ";

  if cu1.cu_debug > 0 && cu2.cu_debug > 0 then begin
    let debug_section1 = Bytes.sub bytes1 cu1.cu_debug cu1.cu_debugsize in
    let debug_section2 = Bytes.sub bytes2 cu2.cu_debug cu2.cu_debugsize in
    if debug_section1 = debug_section2 then
      print_endline "Debug events are identical"
    else
      print_endline "Debug events differ"
  end else if cu1.cu_debug = 0 && cu2.cu_debug = 0 then
    print_endline "No debug data in either file"
  else
    print_endline "Debug data presence mismatch"
