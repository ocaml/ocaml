open ThreadIO

(* Test a file copy function *)

let test msg producer consumer src dst =
  print_string msg; print_newline();
  let ic = open_in src in
  let oc = open_out dst in
  let (in_fd, out_fd) = ThreadUnix.pipe() in
  let ipipe = Unix.in_channel_of_descr in_fd in
  let opipe = Unix.out_channel_of_descr out_fd in
  let prod = Thread.new producer (ic, opipe) in
  let cons = Thread.new consumer (ipipe, oc) in
  Thread.join prod;
  Thread.join cons;
  if ThreadUnix.system ("cmp " ^ src ^ " " ^ dst) = Unix.WEXITED 0
  then print_string "passed"
  else print_string "FAILED";
  print_newline()

(* File copy with constant-sized chunks *)

let copy_file sz (ic, oc) =
  let buffer = String.create sz in
  let rec copy () =
    let n = input ic buffer 0 sz in
    if n = 0 then () else begin
      output oc buffer 0 n;
      copy ()
    end in
  copy();
  close_in ic;
  close_out oc

(* File copy with random-sized chunks *)

let copy_random sz (ic, oc) =
  let buffer = String.create sz in
  let rec copy () =
    let s = 1 + Random.int sz in
    let n = input ic buffer 0 s in
    if n = 0 then () else begin
      output oc buffer 0 n;
      copy ()
    end in
  copy();
  close_in ic;
  close_out oc

(* File copy line per line *)

let copy_line (ic, oc) =
  try
    while true do
      output_string oc (input_line ic); output_char oc '\n'
    done
  with End_of_file ->
    close_in ic;
    close_out oc

(* Create long lines of text *)

let make_lines ofile =
  let oc = open_out ofile in
  for i = 1 to 256 do
    output_string oc (String.make (i*64) '.'); output_char oc '\n'
  done;
  close_out oc

(* The test *)

let main() =
  test "256-byte chunks, 256-byte chunks"
       (copy_file 256) (copy_file 256) "/vmunix" "/tmp/testio";
  test "4096-byte chunks, 4096-byte chunks"
       (copy_file 4096) (copy_file 4096) "/vmunix" "/tmp/testio";
  test "65536-byte chunks, 65536-byte chunks"
       (copy_file 65536) (copy_file 65536) "/vmunix" "/tmp/testio";
  test "256-byte chunks, 4096-byte chunks"
       (copy_file 256) (copy_file 4096) "/vmunix" "/tmp/testio";
  test "4096-byte chunks, 256-byte chunks"
       (copy_file 4096) (copy_file 256) "/vmunix" "/tmp/testio";
  test "4096-byte chunks, 65536-byte chunks"
       (copy_file 4096) (copy_file 65536) "/vmunix" "/tmp/testio";
  test "263-byte chunks, 4011-byte chunks"
       (copy_file 263) (copy_file 4011) "/vmunix" "/tmp/testio";
  test "613-byte chunks, 1027-byte chunks"
       (copy_file 613) (copy_file 1027) "/vmunix" "/tmp/testio";
  test "0...8192 byte chunks"
       (copy_random 8192) (copy_random 8192) "/vmunix" "/tmp/testio";
  test "line per line, short lines"
       copy_line copy_line "/etc/hosts" "/tmp/testio";
  make_lines "/tmp/lines";
  test "line per line, short and long lines"
       copy_line copy_line "/tmp/lines" "/tmp/testio";
  Sys.remove "/tmp/lines";
  Sys.remove "/tmp/testio";
  exit 0

let _ = Unix.handle_unix_error main (); exit 0
