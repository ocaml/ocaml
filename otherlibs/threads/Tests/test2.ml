let print_message c =
  for i = 1 to 10000 do
    print_char c; flush stdout
  done

let _ =
  let t1 = Thread.create print_message 'a' in
  let t2 = Thread.create print_message 'b' in
  Thread.join t1; Thread.join t2; exit 0
