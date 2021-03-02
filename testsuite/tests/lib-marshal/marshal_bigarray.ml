(* TEST *)

let () =
  let small = 0xfffe and large = 0xffff in
  let marshalled dim =
    let ba = Bigarray.(Array1.create int8_unsigned c_layout dim) in
    Marshal.to_string ba []
  in
  (* Bigarray dimension marshalling scheme: use an extra 8 bytes
     to marshal dimensions >=0xffff to avoid overflow *)
  assert
    (((String.length (marshalled large) - String.length (marshalled small))
      - (large - small))
     = 8)
