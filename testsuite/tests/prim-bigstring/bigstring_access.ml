(* TEST *)

open Bigarray
type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

external caml_bigstring_get_16 :
      bigstring -> int -> int = "%caml_bigstring_get16"
external caml_bigstring_get_32 :
      bigstring -> int -> int32 = "%caml_bigstring_get32"
external caml_bigstring_get_64 :
      bigstring -> int -> int64 = "%caml_bigstring_get64"

external caml_bigstring_set_16 :
      bigstring -> int -> int -> unit = "%caml_bigstring_set16"
external caml_bigstring_set_32 :
      bigstring -> int -> int32 -> unit = "%caml_bigstring_set32"
external caml_bigstring_set_64 :
      bigstring -> int -> int64 -> unit = "%caml_bigstring_set64"
external caml_pointer_set_8 :
      Obj.Pointer.t -> int -> int -> unit = "%caml_ptr_set8"
external caml_pointer_set_16 :
      Obj.Pointer.t -> int -> int -> unit = "%caml_ptr_set16"
external caml_pointer_set_32 :
      Obj.Pointer.t -> int -> int32 -> unit = "%caml_ptr_set32"
external caml_pointer_set_64 :
      Obj.Pointer.t -> int -> int64 -> unit = "%caml_ptr_set64"
external caml_pointer_load_8 :
      Obj.Pointer.t -> int -> int = "%caml_ptr_load8"
external caml_pointer_load_16 :
      Obj.Pointer.t -> int -> int = "%caml_ptr_load16"
external caml_pointer_load_32 :
      Obj.Pointer.t -> int -> int32 = "%caml_ptr_load32"
external caml_pointer_load_64 :
      Obj.Pointer.t -> int -> int64 = "%caml_ptr_load64"

let caml_bigstring_set_16_unsafe (b: bigstring) (i: int) v: unit =
  let ptr = Bigarray.Genarray.ptr (Obj.magic b) in
  caml_pointer_set_16 ptr i v

let caml_bigstring_set_32_unsafe (b: bigstring) (i: int) v: unit =
  let ptr = Bigarray.Genarray.ptr (Obj.magic b) in
  caml_pointer_set_32 ptr i v

let caml_bigstring_set_64_unsafe (b: bigstring) (i: int) v: unit =
  let ptr = Bigarray.Genarray.ptr (Obj.magic b) in
  caml_pointer_set_64 ptr i v

let caml_bigstring_get_16_unsafe (b: bigstring) (i: int): int =
  let ptr = Bigarray.Genarray.ptr (Obj.magic b) in
  caml_pointer_load_16 ptr i

let caml_bigstring_get_32_unsafe (b: bigstring) (i: int): int32 =
  let ptr = Bigarray.Genarray.ptr (Obj.magic b) in
  caml_pointer_load_32 ptr i

let caml_bigstring_get_64_unsafe (b: bigstring) (i: int): int64 =
  let ptr = Bigarray.Genarray.ptr (Obj.magic b) in
  caml_pointer_load_64 ptr i

let caml_bigstring_get_16 b i =
  let safe = caml_bigstring_get_16 b i in
  let unsafe = caml_bigstring_get_16_unsafe b i in
  assert (safe = unsafe);
  safe

let caml_bigstring_get_32 b i =
  let safe = caml_bigstring_get_32 b i in
  let unsafe = caml_bigstring_get_32_unsafe b i in
  assert (safe = unsafe);
  safe

let caml_bigstring_get_64 b i =
  let safe = caml_bigstring_get_64 b i in
  let unsafe = caml_bigstring_get_64_unsafe b i in
  assert (safe = unsafe);
  safe

let bigstring_of_string s =
  let a = Array1.create char c_layout (String.length s) in
  for i = 0 to String.length s - 1 do
    a.{i} <- s.[i]
  done;
  a

let s = bigstring_of_string (String.make 10 '\x00')
let empty_s = bigstring_of_string ""

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
  assert_bound_check2 caml_bigstring_get_16 s (-1);
  assert_bound_check2 caml_bigstring_get_16 s 9;
  assert_bound_check2 caml_bigstring_get_32 s (-1);
  assert_bound_check2 caml_bigstring_get_32 s 7;
  assert_bound_check2 caml_bigstring_get_64 s (-1);
  assert_bound_check2 caml_bigstring_get_64 s 3;

  assert_bound_check3 caml_bigstring_set_16 s (-1) 0;
  assert_bound_check3 caml_bigstring_set_16 s 9 0;
  assert_bound_check3 caml_bigstring_set_32 s (-1) 0l;
  assert_bound_check3 caml_bigstring_set_32 s 7 0l;
  assert_bound_check3 caml_bigstring_set_64 s (-1) 0L;
  assert_bound_check3 caml_bigstring_set_64 s 3 0L;

  assert_bound_check2 caml_bigstring_get_16 empty_s 0;
  assert_bound_check2 caml_bigstring_get_32 empty_s 0;
  assert_bound_check2 caml_bigstring_get_64 empty_s 0;

  assert_bound_check3 caml_bigstring_set_16 empty_s 0 0;
  assert_bound_check3 caml_bigstring_set_32 empty_s 0 0l;
  assert_bound_check3 caml_bigstring_set_64 empty_s 0 0L

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
  for i = 0 to 1 do
    let setter_16 = [| caml_bigstring_set_16; caml_bigstring_set_16_unsafe |].(i) in
    let setter_32 = [| caml_bigstring_set_32; caml_bigstring_set_32_unsafe |].(i) in
    let setter_64 = [| caml_bigstring_set_64; caml_bigstring_set_64_unsafe |].(i) in
    setter_64 s 0 0L;
    setter_16 s 0 (swap16 0x1234);
    Printf.printf "%x %x %x\n%!"
                  (swap16 (caml_bigstring_get_16 s 0))
                  (swap16 (caml_bigstring_get_16 s 1))
                  (swap16 (caml_bigstring_get_16 s 2));
    setter_16 s 0 (swap16 0xFEDC);
    Printf.printf "%x %x %x\n%!"
                  (swap16 (caml_bigstring_get_16 s 0))
                  (swap16 (caml_bigstring_get_16 s 1))
                  (swap16 (caml_bigstring_get_16 s 2));

    setter_32 s 0 (swap32 0x12345678l);
    Printf.printf "%lx %lx %lx\n%!"
                  (swap32 (caml_bigstring_get_32 s 0))
                  (swap32 (caml_bigstring_get_32 s 1))
                  (swap32 (caml_bigstring_get_32 s 2));
    setter_32 s 0 (swap32 0xFEDCBA09l);
    Printf.printf "%lx %lx %lx\n%!"
                  (swap32 (caml_bigstring_get_32 s 0))
                  (swap32 (caml_bigstring_get_32 s 1))
                  (swap32 (caml_bigstring_get_32 s 2));

    setter_64 s 0 (swap64 0x1234567890ABCDEFL);
    Printf.printf "%Lx %Lx %Lx\n%!"
                  (swap64 (caml_bigstring_get_64 s 0))
                  (swap64 (caml_bigstring_get_64 s 1))
                  (swap64 (caml_bigstring_get_64 s 2));
    setter_64 s 0 (swap64 0xFEDCBA0987654321L);
    Printf.printf "%Lx %Lx %Lx\n%!"
                  (swap64 (caml_bigstring_get_64 s 0))
                  (swap64 (caml_bigstring_get_64 s 1))
                  (swap64 (caml_bigstring_get_64 s 2))
  done
