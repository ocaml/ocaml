try
  print_endline
    (String.concat ""
      [ "build_path_prefix=\"";
        Build_path_prefix_map.encode_prefix Sys.argv.(1);
        "\"" ])
with Invalid_argument _ -> ()
;;
