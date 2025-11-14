(* TEST
   native-compiler;
   script = "sh ${test_source_directory}/inspect_dwarf.sh comprehensive_dwarf ${ocamlopt} ${arch}";
   script;
   script = "sh ${test_source_directory}/validate_arch_registers.sh comprehensive_dwarf ${ocamlopt} ${arch}";
   script;
*)

(* Comprehensive DWARF test program *)

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let add x y = x + y

let test_locals () =
  let a = 10 in
  let b = 20 in
  let c = add a b in
  factorial c

let () =
  let result = test_locals () in
  Printf.printf "Result: %d\n" result
