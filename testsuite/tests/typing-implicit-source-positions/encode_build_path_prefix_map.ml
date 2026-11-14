try
  print_endline
    (String.concat ""
      [ "build_path_prefix_map=\"app/foo=";
        Build_path_prefix_map.encode_prefix Sys.argv.(1);
        "\"" ])
with Invalid_argument _ -> ()
;;
