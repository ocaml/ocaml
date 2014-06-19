
external caml_string_get_16 : string -> int -> int = "%caml_string_get16"
external caml_string_get_32 : string -> int -> int32 = "%caml_string_get32"
external caml_string_get_64 : string -> int -> int64 = "%caml_string_get64"

external caml_string_set_16 : string -> int -> int -> unit =
  "%caml_string_set16"
external caml_string_set_32 : string -> int -> int32 -> unit =
  "%caml_string_set32"
external caml_string_set_64 : string -> int -> int64 -> unit =
  "%caml_string_set64"

let s = String.make 10 '\x00'
let empty_s = ""

let assert_bound_check2 f v1 v2 =
  try
    ignore(f v1 v2);
    assert false
  with
     | Invalid_argument("index out of bounds") -> ()

let assert_bound_check3 f v1 v2 v3 =
  try
    ignore(f v1 v2 v3);
    assert false
  with
     | Invalid_argument("index out of bounds") -> ()

let () =
  assert_bound_check2 caml_string_get_16 s (-1);
  assert_bound_check2 caml_string_get_16 s 9;
  assert_bound_check2 caml_string_get_32 s (-1);
  assert_bound_check2 caml_string_get_32 s 7;
  assert_bound_check2 caml_string_get_64 s (-1);
  assert_bound_check2 caml_string_get_64 s 3;

  assert_bound_check3 caml_string_set_16 s (-1) 0;
  assert_bound_check3 caml_string_set_16 s 9 0;
  assert_bound_check3 caml_string_set_32 s (-1) 0l;
  assert_bound_check3 caml_string_set_32 s 7 0l;
  assert_bound_check3 caml_string_set_64 s (-1) 0L;
  assert_bound_check3 caml_string_set_64 s 3 0L;

  assert_bound_check2 caml_string_get_16 empty_s 0;
  assert_bound_check2 caml_string_get_32 empty_s 0;
  assert_bound_check2 caml_string_get_64 empty_s 0;

  assert_bound_check3 caml_string_set_16 empty_s 0 0;
  assert_bound_check3 caml_string_set_32 empty_s 0 0l;
  assert_bound_check3 caml_string_set_64 empty_s 0 0L



let () =
  caml_string_set_16 s 0 0x1234;
  Printf.printf "%x %x %x\n%!"
                (caml_string_get_16 s 0)
                (caml_string_get_16 s 1)
                (caml_string_get_16 s 2);
  caml_string_set_16 s 0 0xFEDC;
  Printf.printf "%x %x %x\n%!"
                (caml_string_get_16 s 0)
                (caml_string_get_16 s 1)
                (caml_string_get_16 s 2)

let () =
  caml_string_set_32 s 0 0x12345678l;
  Printf.printf "%lx %lx %lx\n%!"
                (caml_string_get_32 s 0)
                (caml_string_get_32 s 1)
                (caml_string_get_32 s 2);
  caml_string_set_32 s 0 0xFEDCBA09l;
  Printf.printf "%lx %lx %lx\n%!"
                (caml_string_get_32 s 0)
                (caml_string_get_32 s 1)
                (caml_string_get_32 s 2)

let () =
  caml_string_set_64 s 0 0x1234567890ABCDEFL;
  Printf.printf "%Lx %Lx %Lx\n%!"
                (caml_string_get_64 s 0)
                (caml_string_get_64 s 1)
                (caml_string_get_64 s 2);
  caml_string_set_64 s 0 0xFEDCBA0987654321L;
  Printf.printf "%Lx %Lx %Lx\n%!"
                (caml_string_get_64 s 0)
                (caml_string_get_64 s 1)
                (caml_string_get_64 s 2)
