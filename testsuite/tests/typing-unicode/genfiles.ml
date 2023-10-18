let create_file name contents =
  Out_channel.with_open_text name (fun oc -> output_string oc contents)

(* File names in NFC *)

let _ =
  create_file "été.ml" "let x = 1\n";
  create_file "ça.ml"  "let x = 2\n"
