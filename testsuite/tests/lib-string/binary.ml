(* TEST
*)

let err x =
  match Lazy.force x with
  | exception Invalid_argument _ -> ()
  | _ -> assert false

let () =
  let b = "\003\002\001\004\255" in
  lazy (String.get_int8 b 5) |> err;
  lazy (String.get_uint8 b 5) |> err;
  assert(String.get_int8 b 0 = 3);
  assert(String.get_int8 b 1 = 2);
  assert(String.get_int8 b 2 = 1);
  assert(String.get_int8 b 3 = 4);
  assert(String.get_int8 b 4 = -1);
  assert(String.get_uint8 b 0 = 3);
  assert(String.get_uint8 b 1 = 2);
  assert(String.get_uint8 b 2 = 1);
  assert(String.get_uint8 b 3 = 4);
  assert(String.get_uint8 b 4 = 255);
  for i = 0 to 255 do
    let s = Bytes.(let b = create 1 in set_uint8 b 0 i; unsafe_to_string b) in
    assert (String.get_uint8 s 0 = i);
  done;
  for i = -128 to 127 do
    let s = Bytes.(let b = create 1 in set_int8 b 0 i; unsafe_to_string b) in
    assert (String.get_int8 s 0 = i);
  done

let () =
  let b = "\xcd\xab\x12" in
  assert(String.get_uint16_le b 0 = 0xabcd);
  assert(String.get_uint16_le b 1 = 0x12ab);
  assert(String.get_int16_le b 0 = 0xabcd - 0x10000);
  assert(String.get_int16_le b 1 = 0x12ab);
  assert(String.get_uint16_be b 1 = 0xab12);
  assert(String.get_int16_be b 1 = 0xab12 - 0x10000);
  for i = 0 to String.length b - 2 do
    let x = String.get_int16_ne b i in
    let f = if Sys.big_endian then String.get_int16_be else String.get_int16_le in
    assert (x = f b i);

    let x = String.get_uint16_ne b i in
    let f = if Sys.big_endian then String.get_uint16_be else String.get_uint16_le in
    assert (x = f b i)
  done;
  lazy (String.get_int16_le b 2) |> err;
  lazy (String.get_int16_ne b 2) |> err;
  lazy (String.get_int16_be b 2) |> err;
  lazy (String.get_uint16_le b 2) |> err;
  lazy (String.get_uint16_ne b 2) |> err;
  lazy (String.get_uint16_be b 2) |> err;
  for i = 0 to 0xffff do
    let s = Bytes.(let b = create 3 in set_uint16_le b 0 i; unsafe_to_string b) in
    assert (String.get_uint16_le s 0 = i);
    let s = Bytes.(let b = create 3 in set_uint16_be b 0 i; unsafe_to_string b) in
    assert (String.get_uint16_be s 0 = i);
    let s = Bytes.(let b = create 3 in set_uint16_ne b 0 i; unsafe_to_string b) in
    assert (String.get_uint16_ne s 0 = i);
    assert (
      (if Sys.big_endian then String.get_uint16_be else String.get_uint16_le)
        s 0 = i);
  done;
  for i = -0x8000 to 0x7fff do
    let s = Bytes.(let b = create 3 in set_int16_le b 0 i; unsafe_to_string b) in
    assert (String.get_int16_le s 0 = i);
    let s = Bytes.(let b = create 3 in set_int16_be b 0 i; unsafe_to_string b) in
    assert (String.get_int16_be s 0 = i);
    let s = Bytes.(let b = create 3 in set_int16_ne b 0 i; unsafe_to_string b) in
    assert (String.get_int16_ne s 0 = i);
    assert (
      (if Sys.big_endian then String.get_int16_be else String.get_int16_le)
        s 0 = i);
  done

let () =
  let b = "\xef\xcd\xab\x89\x01\x00" in
  assert (String.get_int32_le b 0 = 0x89abcdefl);
  assert (String.get_int32_be b 0 = 0xefcdab89l);
  assert (String.get_int32_le b 1 = 0x0189abcdl);
  assert (String.get_int32_be b 1 = 0xcdab8901l);

  for i = 0 to String.length b - 4 do
    let x = String.get_int32_ne b i in
    let f =
      if Sys.big_endian then String.get_int32_be else String.get_int32_le
    in
    assert (x = f b i);
  done;
  lazy (String.get_int32_le b 3) |> err;
  lazy (String.get_int32_ne b 3) |> err;
  lazy (String.get_int32_be b 3) |> err;
  ()


let () =
  let b = "\xfe\xdc\xba\x98\x76\x54\x32\x10\x01\x00" in
  assert (String.get_int64_le b 0 = 0x1032547698badcfeL);
  assert (String.get_int64_be b 0 = 0xfedcba9876543210L);
  assert (String.get_int64_le b 1 = 0x011032547698badcL);
  assert (String.get_int64_be b 1 = 0xdcba987654321001L);

  for i = 0 to String.length b - 8 do
    let x = String.get_int64_ne b i in
    let f =
      if Sys.big_endian then String.get_int64_be else String.get_int64_le
    in
    assert (x = f b i);
  done;

  lazy (String.get_int64_le b 3) |> err;
  lazy (String.get_int64_ne b 3) |> err;
  lazy (String.get_int64_be b 3) |> err;
  ()
