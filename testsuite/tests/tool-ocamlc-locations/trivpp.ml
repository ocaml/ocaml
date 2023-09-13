let file = Sys.argv.(1)

let () = Printf.printf "# 2 \"%s\"\n" file

let () =
  let content = In_channel.with_open_bin file In_channel.input_all in
  print_string content;
  flush stdout
