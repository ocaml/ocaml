(* TEST *)

(* Test that inputting bytes from a closed in_channel triggers an exception *)

(* The number of bytes we'll rewind after closing; a value
   between 1 and IO_BUFFER_SIZE *)
let nb_bytes = 3

let () =
  let ic = open_in_bin Sys.argv.(0) in
  seek_in ic nb_bytes;
  close_in ic;
  assert (
    try
      seek_in ic 0;
      ignore (input_byte ic);
      false
    with
    | Sys_error _ -> true
    | _           -> false)

(* A variant of #11878, which #11965 failed to fix. *)
let () =
  let ic = open_in_bin Sys.argv.(0) in
  close_in ic;
  begin try
    seek_in ic (-1);
    ignore (input_byte ic);
    assert false;
  with
  | Sys_error _ -> ()
  end
