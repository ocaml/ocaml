open Event

let ch = (new_channel() : string channel)

let rec sender msg =
  sync (send ch msg);
  sender msg

let rec receiver name =
  print_string (name ^ ": " ^ sync (receive ch) ^ "\n");
  flush stdout;
  receiver name

let _ =
  Thread.new sender "hello";
  Thread.new sender "world";
  Thread.new receiver "A";
  Thread.new receiver "B";
  read_line();
  exit 0


