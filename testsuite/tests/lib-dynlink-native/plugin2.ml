(*external ex: int -> int = "caml_ex"*)

let () =
  Api.reg_mod "Plugin2";
(*  let i = ex 3 in*)
  List.iter (fun i -> Printf.printf "%i\n" i) Plugin.facts;
  Printf.printf "XXX\n";
  raise Exit
