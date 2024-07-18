let create_file name contents =
  Out_channel.with_open_text name (fun oc -> output_string oc contents)

let _ =
  (* File name in NFC *)
  create_file "été.ml" "let x = 1\n";
  (* File name in NFD *)
  create_file "\u{0063}\u{0327}a.ml"  "let x = 2\n"
