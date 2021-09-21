(* TEST
readonly_files = "input_all.ml"
*)

let data_file =
  "data.txt"

let random_string size =
  String.init size (fun _ -> Char.chr (Random.int 256))

(* various sizes, binary mode *)

let check size =
  let data = random_string size in
  Out_channel.with_open_bin data_file (fun oc -> Out_channel.output_string oc data);
  let read_data = In_channel.with_open_bin data_file In_channel.input_all in
  assert (data = read_data)

let () =
  List.iter check [ 0; 1; 65536; 65536 + 1; 2 * 65536 ]

(* binary mode; non-zero starting position *)

let data_size = 65536

let check midpoint =
  let data = random_string data_size in
  Out_channel.with_open_bin data_file
    (fun oc -> Out_channel.output_string oc data);
  let contents =
    In_channel.with_open_bin data_file
      (fun ic ->
         let s1 = Option.get (In_channel.really_input_string ic midpoint) in
         let s2 = In_channel.input_all ic in
         s1 ^ s2
      )
  in
  assert (contents = data)

let () =
  List.iter check [0; 1; 100; data_size]

(* text mode *)

(* translates LF into CRLF in-place in [fn] *)
let unix2dos fn =
  let s = In_channel.with_open_text fn In_channel.input_all in
  Out_channel.with_open_text fn
    (fun oc -> Out_channel.output_string oc s)

let source_fn =
  "input_all.ml"

let source =
  In_channel.with_open_bin source_fn In_channel.input_all

let () =
  unix2dos source_fn

let check midpoint =
  let contents =
    In_channel.with_open_text source_fn
      (fun ic ->
         let s1 = Option.get (In_channel.really_input_string ic midpoint) in
         let s2 = In_channel.input_all ic in
         s1 ^ s2
      )
  in
  assert (contents = source)

let () =
  List.iter check [0; 1; String.length source]
