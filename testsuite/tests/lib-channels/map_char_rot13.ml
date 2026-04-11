(* TEST *)

let text =
  {|Bugfix versions are published if we discover issues that significantly impede
the use of the initially released version. In that situation, it is not uncommon
that we backport safe bug fixes that were integrated in the trunk after the
release.|}

let rot13 c =
  match c with
  | 'a'..'z' ->
      let c = Char.code c in
      let c = ((c - Char.code 'a' + 13) mod 26) + Char.code 'a' in
      Char.chr c
  | 'A'..'Z' ->
      let c = Char.code c in
      let c = ((c - Char.code 'A' + 13) mod 26) + Char.code 'A' in
      Char.chr c
  | _ -> c

let () =
  let buf = Buffer.create 256 in
  let oc = Out_channel.map_char rot13 (Out_channel.of_buffer buf) in
  Out_channel.output_string oc text;
  Out_channel.close oc;
  (* Read back through a rot13 in_channel (rot13 is involutive) *)
  let ic = In_channel.map_char rot13 (In_channel.of_string (Buffer.contents buf)) in
  let result = In_channel.input_all ic in
  In_channel.close ic;
  assert (result = text);
  print_string "ok\n"
