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

(* translates into LF *)
let dos2unix inp out =
  let s = In_channel.with_open_text inp In_channel.input_all in
  Out_channel.with_open_bin out
    (fun oc -> Out_channel.output_string oc s)

(* translates into CRLF *)
let unix2dos inp out =
  let s = In_channel.with_open_text inp In_channel.input_all in
  Out_channel.with_open_text out
    (fun oc -> Out_channel.output_string oc s)

let source_fn =
  "input_all.ml"

let source_fn_lf =
  source_fn ^ ".lf"

let source_fn_crlf =
  source_fn ^ ".crlf"

let () =
  dos2unix source_fn source_fn_lf

let () =
  unix2dos source_fn source_fn_crlf

let raw_contents =
  In_channel.with_open_bin source_fn_lf
    (fun ic -> Stdlib.really_input_string ic (Stdlib.in_channel_length ic))

let check midpoint =
  let contents =
    In_channel.with_open_text source_fn_crlf
      (fun ic ->
         let s1 = Option.get (In_channel.really_input_string ic midpoint) in
         let s2 = In_channel.input_all ic in
         s1 ^ s2
      )
  in
  assert (contents = raw_contents)

let () =
  List.iter check [0; 1; String.length raw_contents]
