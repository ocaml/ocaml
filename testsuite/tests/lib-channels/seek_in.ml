(* TEST *)

let () =
  let oc = open_out_bin "data.txt" in
  output_string oc "0\r\n1\r\n";
  close_out oc;
  (* Open in text mode to trigger EOL conversion under Windows *)
  let ic = open_in "data.txt" in
  ignore (input_line ic);
  seek_in ic 3;
  (* Normally we should be looking at "1\r\n", which will be read as
     "1" under Windows because of EOL conversion and "1\r" otherwise.
     What goes wrong with the old implementation of seek_in is that
     we have "0\n\1\n" in the channel buffer and have read "0\n" already,
     so we think we are at position 2, and the seek to position 3
     just advances by one in the buffer, pointing to "\n" instead of "1\n". *)
  let l = input_line ic in
  close_in ic;
  assert (l = "1" || l = "1\r")
