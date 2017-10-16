open Printf
open Bigarray

let mapped_file = Filename.temp_file "bigarray" ".data"
let reference_file = Filename.temp_file "bigarray" ".ref"
let text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.\n"

let test () =
  let oc = open_out_bin reference_file in
  output_string oc text;
  close_out oc;
  let fd =
   Unix.openfile mapped_file
                 [Unix.O_RDWR; Unix.O_TRUNC; Unix.O_CREAT] 0o666 in
  let b =
    Unix.map_file fd int8_unsigned c_layout true [| String.length text |] in
  let a =
    array1_of_genarray b in
  String.iteri (fun i c -> a.{i} <- Char.code c) text;
  Unix.flush_mapped_file b (if Sys.os_type = "Unix" then true else false);
  let rc1 = Sys.command (sprintf "cmp %s %s" mapped_file reference_file) in
  printf "Test 1: %s\n" (if rc1 = 0 then "success" else "failure");
  Unix.close fd;
  let rc2 = Sys.command (sprintf "cmp %s %s" mapped_file reference_file) in
  printf "Test 2: %s\n" (if rc2 = 0 then "success" else "failure")

  [@@inline never]

let _ =
  test ();
  (* Force garbage collection of the mapped bigarrays above, otherwise
     Win32 doesn't let us erase the file. *)
  Gc.full_major();
  Sys.remove mapped_file;
  Sys.remove reference_file
