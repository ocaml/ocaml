open Unix

let engine number address =
  print_int number; print_string "> connecting"; print_newline();
  let (ic, oc) = open_connection (ADDR_INET(address, 80)) in
  print_int number; print_string "> connected"; print_newline();
  output_string oc "GET / HTTP1.0\r\n\r\n"; flush oc;
  try
    while true do
      let s = input_line ic in
      print_int number; print_string ">"; print_string s; print_newline()
    done
  with End_of_file ->
    close_out oc

let main() =
  let addresses = Array.create (Array.length Sys.argv - 1) inet_addr_any in
  for i = 1 to Array.length Sys.argv - 1 do
    addresses.(i - 1) <- (gethostbyname Sys.argv.(i)).h_addr_list.(0)
  done;
  let processes = Array.create (Array.length addresses) (Thread.self()) in
  for i = 0 to Array.length addresses - 1 do
    processes.(i) <- Thread.create (engine i) addresses.(i)
  done;
  for i = 0 to Array.length processes - 1 do
    Thread.join processes.(i)
  done

let _ = Printexc.catch main (); exit 0

    
