open Event

let add_ch = new_channel()
let sub_ch = new_channel()
let read_ch = new_channel()

let rec accu n =
  print_string "?"; flush stdout;
  select [
    wrap (receive add_ch) (fun x -> print_string "+"; flush stdout; accu (n+x));
    wrap (receive sub_ch) (fun x -> print_string "-"; flush stdout; accu (n-x));
    wrap (send read_ch n) (fun () -> print_string "="; flush stdout; accu n)
  ]

let rec adder value =
  print_string "!"; flush stdout; sync(send add_ch value); adder value

let rec subber value =
  print_string "@"; flush stdout; sync(send sub_ch value); subber value

let read () =
  print_int(sync(receive read_ch)); print_newline()

let main () =
  Thread.create accu 0;
  Thread.create adder 1;
  Thread.create subber 1;
  while true do read() done

let _ = Printexc.catch main ()

  
