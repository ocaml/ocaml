(* TEST
 arch_amd64;
 native-compiler;
 {
   flags = "-S -fno-fma";
   reference = "${test_source_directory}/target_features.called.reference";
   native;
 }{
   flags = "-S -ftarget-level x86-64-v3 -fno-fma";
   reference = "${test_source_directory}/target_features.called.reference";
   native;
 }{
   flags = "-S -fno-fma -ftarget-level x86-64-v3";
   reference = "${test_source_directory}/target_features.called.reference";
   native;
 }{
   set OCAMLPARAM = "_,target-features=no-fma";
   flags = "-S -ffma";
   reference = "${test_source_directory}/target_features.called.reference";
   native;
 }{
   script = "sh ${test_source_directory}/has_fma.sh";
   script;
   {
     flags = "-S -ftarget-level x86-64 -ffma";
     reference = "${test_source_directory}/target_features.fused.reference";
     native;
   }{
     set OCAMLPARAM = "target-features=x86-64,_";
     flags = "-S -ffma";
     reference = "${test_source_directory}/target_features.fused.reference";
     native;
   }
 }
*)

let fma = Sys.opaque_identity Float.fma

let dot a b =
  let acc = ref 0. in
  for i = 0 to Array.length a - 1 do
    acc := Float.fma a.(i) b.(i) !acc
  done;
  !acc

let () =
  let o = Sys.opaque_identity in
  Printf.printf "%h\n" (Float.fma (o 2.) (o 3.) (o 1.));
  Printf.printf "%h\n" (Float.fma (o 1.) (o 2.) (o 3.));
  Printf.printf "%h\n"
    (Float.fma (o 0x1.0000000000001p0) (o 0x1.fffffffffffffp-1) (o (-1.)));
  Printf.printf "%h\n" (Float.fma (o 0x1p1023) (o 2.) (o (-0x1p1023)));
  Printf.printf "%h\n" (Float.fma (o (-0.)) (o 1.) (o 0.));
  Printf.printf "%h\n" (fma (o 2.) (o 3.) (o 1.));
  Printf.printf "%h\n"
    (dot [| 1.; 0x1.0000000000001p0 |] [| -1.; 0x1.fffffffffffffp-1 |])
