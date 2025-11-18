(* TEST
 flags = "-g";
 setup-ocamlopt.byte-build-env;
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 script = "sh ${test_source_directory}/multi_obj_link.sh ${ocamlopt}";
 script;
*)

(* Module A for multi-object linking test *)
let add x y = x + y

let multiply x y = x * y
