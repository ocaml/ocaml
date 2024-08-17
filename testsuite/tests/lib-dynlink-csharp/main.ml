(* TEST
 include dynlink;
 readonly_files = "entry.c main.cs plugin.ml";
 csharp-compiler;
 set csharp_cmd = "${csc} ${csc_flags} /out:main.exe main.cs";
 shared-libraries;
 {
   setup-ocamlc.byte-build-env;
   module = "plugin.ml";
   ocamlc.byte;
   module = "";
   flags = "-output-obj";
   program = "main.dll";
   all_modules = "main.ml entry.c";
   ocamlc.byte;
   script = "${csharp_cmd}";
   script;
   program = "./main.exe";
   run;
   reference = "${test_source_directory}/main.bytecode.reference";
   check-program-output;
 }{
   compiler_directory_suffix = "-dll";
   setup-ocamlc.byte-build-env;
   module = "plugin.ml";
   ocamlc.byte;
   module = "";
   flags = "-output-obj";
   program = "main_obj.${objext}";
   all_modules = "entry.c main.ml";
   ocamlc.byte;
   script = "${mkdll} -maindll -o main.dll main_obj.${objext} entry.${objext} \
     ${ocamlsrcdir}/runtime/libcamlrun.${libext} ${bytecc_libs}";
   script;
   script = "${csharp_cmd}";
   script;
   program = "./main.exe";
   run;
   reference = "${test_source_directory}/main.bytecode.reference";
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   program = "plugin.cmxs";
   flags = "-shared";
   all_modules = "plugin.ml";
   ocamlopt.byte;
   flags = "-output-obj";
   program = "main.dll";
   all_modules = "entry.c main.ml";
   ocamlopt.byte;
   script = "${csharp_cmd}";
   script;
   program = "./main.exe";
   run;
   reference = "${test_source_directory}/main.native.reference";
   check-program-output;
 }{
   compiler_directory_suffix = "-dll";
   setup-ocamlopt.byte-build-env;
   program = "plugin.cmxs";
   flags = "-shared";
   all_modules = "plugin.ml";
   ocamlopt.byte;
   flags = "-output-obj";
   program = "main_obj.${objext}";
   all_modules = "entry.c main.ml";
   ocamlopt.byte;
   script = "${mkdll} -maindll -o main.dll main_obj.${objext} entry.${objext} \
     ${ocamlsrcdir}/runtime/libasmrun.${libext} ${nativecc_libs}";
   script;
   script = "${csharp_cmd}";
   script;
   program = "./main.exe";
   run;
   reference = "${test_source_directory}/main.native.reference";
   check-program-output;
 }
*)

let load s =
  Printf.printf "Loading %s\n%!" s;
  try
    Dynlink.loadfile s
  with Dynlink.Error e ->
    print_endline (Dynlink.error_message e)

(* Callback must be linked to load Unix dynamically *)
let _ = Callback.register
let _ = Stdlib.Bigarray.float32

let () =
  ignore (Hashtbl.hash 42.0);
  print_endline "Main is running.";
  Dynlink.allow_unsafe_modules true;
  let plugin_name = Dynlink.adapt_filename "plugin.cmo" in
  load plugin_name;
  print_endline "OK."
