(* TEST
*)

let err x =
  match Lazy.force x with
  | exception Invalid_argument _ -> ()
  | _ -> assert false

let () =
  let b = Bytes.make 5 (Char.chr 0) in
  Bytes.set_int8 b 3 260;
  Bytes.set_int8 b 2 1;
  Bytes.set_int8 b 1 2;
  Bytes.set_int8 b 0 3;
  Bytes.set_int8 b 4 (-1);
  assert (Bytes.to_string b = "\003\002\001\004\255");
  lazy (Bytes.set_int8 b 5 0) |> err;
  lazy (Bytes.get_int8 b 5) |> err;
  lazy (Bytes.set_uint8 b 5 0) |> err;
  lazy (Bytes.get_uint8 b 5) |> err;
  assert(Bytes.get_int8 b 0 = 3);
  assert(Bytes.get_int8 b 1 = 2);
  assert(Bytes.get_int8 b 2 = 1);
  assert(Bytes.get_int8 b 3 = 4);
  assert(Bytes.get_int8 b 4 = -1);
  assert(Bytes.get_uint8 b 0 = 3);
  assert(Bytes.get_uint8 b 1 = 2);
  assert(Bytes.get_uint8 b 2 = 1);
  assert(Bytes.get_uint8 b 3 = 4);
  assert(Bytes.get_uint8 b 4 = 255);
  for i = 0 to 255 do
    Bytes.set_uint8 b 0 i;
    assert (Bytes.get_uint8 b 0 = i);
  done;
  for i = -128 to 127 do
    Bytes.set_int8 b 0 i;
    assert (Bytes.get_int8 b 0 = i);
  done

let () =
  let b = Bytes.make 3 (Char.chr 0) in
  Bytes.set_int16_le b 1 0x1234;
  Bytes.set_int16_le b 0 0xabcd;
  assert (Bytes.to_string b = "\xcd\xab\x12");
  assert(Bytes.get_uint16_le b 0 = 0xabcd);
  assert(Bytes.get_uint16_le b 1 = 0x12ab);
  assert(Bytes.get_int16_le b 0 = 0xabcd - 0x10000);
  assert(Bytes.get_int16_le b 1 = 0x12ab);
  assert(Bytes.get_uint16_be b 1 = 0xab12);
  assert(Bytes.get_int16_be b 1 = 0xab12 - 0x10000);
  for i = 0 to Bytes.length b - 2 do
    let x = Bytes.get_int16_ne b i in
    let f = if Sys.big_endian then Bytes.get_int16_be else Bytes.get_int16_le in
    assert (x = f b i);

    let x = Bytes.get_uint16_ne b i in
    let f = if Sys.big_endian then Bytes.get_uint16_be else Bytes.get_uint16_le in
    assert (x = f b i)
  done;
  lazy (Bytes.set_int16_le b 2 0) |> err;
  lazy (Bytes.set_int16_ne b 2 0) |> err;
  lazy (Bytes.set_int16_be b 2 0) |> err;
  lazy (Bytes.get_int16_le b 2) |> err;
  lazy (Bytes.get_int16_ne b 2) |> err;
  lazy (Bytes.get_int16_be b 2) |> err;
  lazy (Bytes.set_uint16_le b 2 0) |> err;
  lazy (Bytes.set_uint16_ne b 2 0) |> err;
  lazy (Bytes.set_uint16_be b 2 0) |> err;
  lazy (Bytes.get_uint16_le b 2) |> err;
  lazy (Bytes.get_uint16_ne b 2) |> err;
  lazy (Bytes.get_uint16_be b 2) |> err;
  for i = 0 to 0xffff do
    Bytes.set_uint16_le b 0 i;
    assert (Bytes.get_uint16_le b 0 = i);
    Bytes.set_uint16_be b 0 i;
    assert (Bytes.get_uint16_be b 0 = i);
    Bytes.set_uint16_ne b 0 i;
    assert (Bytes.get_uint16_ne b 0 = i);
    assert (
      (if Sys.big_endian then Bytes.get_uint16_be else Bytes.get_uint16_le)
        b 0 = i);
  done;
  for i = -0x8000 to 0x7fff do
    Bytes.set_int16_le b 0 i;
    assert (Bytes.get_int16_le b 0 = i);
    Bytes.set_int16_be b 0 i;
    assert (Bytes.get_int16_be b 0 = i);
    Bytes.set_int16_ne b 0 i;
    assert (Bytes.get_int16_ne b 0 = i);
    assert (
      (if Sys.big_endian then Bytes.get_int16_be else Bytes.get_int16_le)
        b 0 = i);
  done

let () =
  let b = Bytes.make 6 (Char.chr 0) in
  Bytes.set_int32_le b 1 0x01234567l;
  Bytes.set_int32_le b 0 0x89abcdefl;
  assert (Bytes.to_string b = "\xef\xcd\xab\x89\x01\x00");
  assert (Bytes.get_int32_le b 0 = 0x89abcdefl);
  assert (Bytes.get_int32_be b 0 = 0xefcdab89l);
  assert (Bytes.get_int32_le b 1 = 0x0189abcdl);
  assert (Bytes.get_int32_be b 1 = 0xcdab8901l);

  Bytes.set_int32_be b 1 0x01234567l;
  Bytes.set_int32_be b 0 0x89abcdefl;
  assert (Bytes.to_string b = "\x89\xab\xcd\xef\x67\x00");

  Bytes.set_int32_ne b 0 0x01234567l;
  assert(Bytes.get_int32_ne b 0 = 0x01234567l);
  if Sys.big_endian then
    assert (Bytes.to_string b = "\x01\x23\x45\x67\x67\x00")
  else
    assert (Bytes.to_string b = "\x67\x45\x23\x01\x67\x00");
  Bytes.set_int32_ne b 0 0xffffffffl;
  assert(Bytes.get_int32_ne b 0 = 0xffffffffl);


  for i = 0 to Bytes.length b - 4 do
    let x = Bytes.get_int32_ne b i in
    let f =
      if Sys.big_endian then Bytes.get_int32_be else Bytes.get_int32_le
    in
    assert (x = f b i);
  done;
  lazy (Bytes.set_int32_le b 3 0l) |> err;
  lazy (Bytes.set_int32_ne b 3 0l) |> err;
  lazy (Bytes.set_int32_be b 3 0l) |> err;
  lazy (Bytes.get_int32_le b 3) |> err;
  lazy (Bytes.get_int32_ne b 3) |> err;
  lazy (Bytes.get_int32_be b 3) |> err;
  ()


let () =
  let b = Bytes.make 10 (Char.chr 0) in
  Bytes.set_int64_le b 1 0x0123456789abcdefL;
  Bytes.set_int64_le b 0 0x1032547698badcfeL;
  assert (Bytes.to_string b = "\xfe\xdc\xba\x98\x76\x54\x32\x10\x01\x00");
  assert (Bytes.get_int64_le b 0 = 0x1032547698badcfeL);
  assert (Bytes.get_int64_be b 0 = 0xfedcba9876543210L);
  assert (Bytes.get_int64_le b 1 = 0x011032547698badcL);
  assert (Bytes.get_int64_be b 1 = 0xdcba987654321001L);

  Bytes.set_int64_be b 1 0x0123456789abcdefL;
  Bytes.set_int64_be b 0 0x1032547698badcfeL;
  assert (Bytes.to_string b = "\x10\x32\x54\x76\x98\xba\xdc\xfe\xef\x00");

  Bytes.set_int64_ne b 0 0x0123456789abcdefL;
  assert(Bytes.get_int64_ne b 0 = 0x0123456789abcdefL);
  if Sys.big_endian then
    assert (Bytes.to_string b = "\x01\x23\x45\x67\x89\xab\xcd\xef\xef\x00")
  else
    assert (Bytes.to_string b = "\xef\xcd\xab\x89\x67\x45\x23\x01\xef\x00");
  Bytes.set_int64_ne b 0 0xffffffffffffffffL;
  assert(Bytes.get_int64_ne b 0 = 0xffffffffffffffffL);

  for i = 0 to Bytes.length b - 8 do
    let x = Bytes.get_int64_ne b i in
    let f =
      if Sys.big_endian then Bytes.get_int64_be else Bytes.get_int64_le
    in
    assert (x = f b i);
  done;

  lazy (Bytes.set_int64_le b 3 0L) |> err;
  lazy (Bytes.set_int64_ne b 3 0L) |> err;
  lazy (Bytes.set_int64_be b 3 0L) |> err;
  lazy (Bytes.get_int64_le b 3) |> err;
  lazy (Bytes.get_int64_ne b 3) |> err;
  lazy (Bytes.get_int64_be b 3) |> err;
  ()
