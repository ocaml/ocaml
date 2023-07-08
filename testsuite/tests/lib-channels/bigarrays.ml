(* TEST *)

let filename = "test.out"

let bigarray_of_string s =
  Bigarray.Array1.init Bigarray.char Bigarray.c_layout (String.length s)
    (String.get s)

let string_of_bigarray buf =
  String.init (Bigarray.Array1.dim buf) (Bigarray.Array1.get buf)

let () =
  let oc = Out_channel.open_bin filename in
  let str = ">hello, world<" in
  let buf = bigarray_of_string str in
  Out_channel.output_bigarray oc buf 1 (String.length str - 2);
  Out_channel.close oc;
  let ic = In_channel.open_bin filename in
  let buf = bigarray_of_string (String.map (fun _ -> 'X') str) in
  assert (Option.is_some (In_channel.really_input_bigarray ic buf 1 (String.length str - 2)));
  print_endline (string_of_bigarray buf)
