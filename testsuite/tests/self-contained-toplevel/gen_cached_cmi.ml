let () =
  let cmi = Cmi_format.read_cmi "foo.cmi" in
  let data = Marshal.to_string cmi [] in
  let filename = Sys.argv.(1) in
  let oc = open_out filename in
  Printf.fprintf oc "let foo = %S\n" data;
  close_out oc
