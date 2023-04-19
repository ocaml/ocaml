(* TEST *)

(* Test that inputting bytes from a closed in_channel triggers an exception *)

(* The number of bytes we'll rewind after closing; a value
   between 1 and IO_BUFFER_SIZE *)
let nb_bytes = 3

let () =
  let ic = open_in_bin Sys.argv.(0) in
  seek_in ic nb_bytes;
  close_in ic;
  seek_in ic 0;
  assert (
    try
      ignore (input_byte ic);
      false
    with
    | Sys_error _ -> true
    | _           -> false)
