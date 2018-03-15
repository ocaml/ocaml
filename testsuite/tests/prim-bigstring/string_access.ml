(* TEST
*)

external caml_bytes_get_16 : bytes -> int -> int = "%caml_bytes_get16"
external caml_bytes_get_32 : bytes -> int -> int32 = "%caml_bytes_get32"
external caml_bytes_get_64 : bytes -> int -> int64 = "%caml_bytes_get64"

external caml_bytes_set_16 : bytes -> int -> int -> unit =
  "%caml_bytes_set16"
external caml_bytes_set_32 : bytes -> int -> int32 -> unit =
  "%caml_bytes_set32"
external caml_bytes_set_64 : bytes -> int -> int64 -> unit =
  "%caml_bytes_set64"

let s = Bytes.make 10 '\x00'
let empty_s = Bytes.create 0

let assert_bound_check2 f v1 v2 =
  try
    ignore(f v1 v2);
    assert false
  with
     | Invalid_argument _ -> ()

let assert_bound_check3 f v1 v2 v3 =
  try
    ignore(f v1 v2 v3);
    assert false
  with
     | Invalid_argument _ -> ()

let () =
  assert_bound_check2 caml_bytes_get_16 s (-1);
  assert_bound_check2 caml_bytes_get_16 s 9;
  assert_bound_check2 caml_bytes_get_32 s (-1);
  assert_bound_check2 caml_bytes_get_32 s 7;
  assert_bound_check2 caml_bytes_get_64 s (-1);
  assert_bound_check2 caml_bytes_get_64 s 3;

  assert_bound_check3 caml_bytes_set_16 s (-1) 0;
  assert_bound_check3 caml_bytes_set_16 s 9 0;
  assert_bound_check3 caml_bytes_set_32 s (-1) 0l;
  assert_bound_check3 caml_bytes_set_32 s 7 0l;
  assert_bound_check3 caml_bytes_set_64 s (-1) 0L;
  assert_bound_check3 caml_bytes_set_64 s 3 0L;

  assert_bound_check2 caml_bytes_get_16 empty_s 0;
  assert_bound_check2 caml_bytes_get_32 empty_s 0;
  assert_bound_check2 caml_bytes_get_64 empty_s 0;

  assert_bound_check3 caml_bytes_set_16 empty_s 0 0;
  assert_bound_check3 caml_bytes_set_32 empty_s 0 0l;
  assert_bound_check3 caml_bytes_set_64 empty_s 0 0L

external bswap16: int -> int = "%bswap16"
external bswap32: int32 -> int32 = "%bswap_int32"
external bswap64: int64 -> int64 = "%bswap_int64"

let swap16 x =
  if Sys.big_endian
  then bswap16 x
  else x

let swap32 x =
  if Sys.big_endian
  then bswap32 x
  else x

let swap64 x =
  if Sys.big_endian
  then bswap64 x
  else x

let () =
  caml_bytes_set_16 s 0 (swap16 0x1234);
  Printf.printf "%x %x %x\n%!"
                (swap16 (caml_bytes_get_16 s 0))
                (swap16 (caml_bytes_get_16 s 1))
                (swap16 (caml_bytes_get_16 s 2));
  caml_bytes_set_16 s 0 (swap16 0xFEDC);
  Printf.printf "%x %x %x\n%!"
                (swap16 (caml_bytes_get_16 s 0))
                (swap16 (caml_bytes_get_16 s 1))
                (swap16 (caml_bytes_get_16 s 2))

let () =
  caml_bytes_set_32 s 0 (swap32 0x12345678l);
  Printf.printf "%lx %lx %lx\n%!"
                (swap32 (caml_bytes_get_32 s 0))
                (swap32 (caml_bytes_get_32 s 1))
                (swap32 (caml_bytes_get_32 s 2));
  caml_bytes_set_32 s 0 (swap32 0xFEDCBA09l);
  Printf.printf "%lx %lx %lx\n%!"
                (swap32 (caml_bytes_get_32 s 0))
                (swap32 (caml_bytes_get_32 s 1))
                (swap32 (caml_bytes_get_32 s 2))

let () =
  caml_bytes_set_64 s 0 (swap64 0x1234567890ABCDEFL);
  Printf.printf "%Lx %Lx %Lx\n%!"
                (swap64 (caml_bytes_get_64 s 0))
                (swap64 (caml_bytes_get_64 s 1))
                (swap64 (caml_bytes_get_64 s 2));
  caml_bytes_set_64 s 0 (swap64 0xFEDCBA0987654321L);
  Printf.printf "%Lx %Lx %Lx\n%!"
                (swap64 (caml_bytes_get_64 s 0))
                (swap64 (caml_bytes_get_64 s 1))
                (swap64 (caml_bytes_get_64 s 2))
