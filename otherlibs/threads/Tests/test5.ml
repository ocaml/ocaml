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
  Thread.create sender "hello";
  Thread.create sender "world";
  Thread.create receiver "A";
  receiver "B";
  exit 0
