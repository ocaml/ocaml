
external get_8 : nativeint -> int = "%load8"
external get_16 : nativeint -> int = "%load16"
external get_32 : nativeint -> int32 = "%load32"
external get_64 : nativeint -> int64 = "%load64"

external set_8 : nativeint -> int -> unit = "%store8"
external set_16 : nativeint -> int -> unit = "%store16"
external set_32 : nativeint -> int32 -> unit = "%store32"
external set_64 : nativeint -> int64 -> unit = "%store64"

external malloc : int -> nativeint = "caml_malloc"

let malloc n =
  match malloc n with
  | 0n -> raise Out_of_memory
  | ptr -> ptr

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

let ptr = malloc 10

let ( + ) ptr n = Nativeint.add ptr (Nativeint.of_int n)

let () =
  for i = 0 to 9 do
    set_8 (ptr + i) i
  done;
  Array.init 10 (fun i -> string_of_int (get_8 (ptr + i)))
  |> Array.to_list
  |> String.concat " "
  |> print_endline

let () =
  for i = 0 to 9 do
    set_8 (ptr + i) 0
  done;
  Array.init 10 (fun i -> string_of_int (get_8 (ptr + i)))
  |> Array.to_list
  |> String.concat " "
  |> print_endline

let () =
  set_16 ptr (swap16 0x1234);
  Printf.printf "%x %x %x\n%!"
                (swap16 (get_16 (ptr + 0)))
                (swap16 (get_16 (ptr + 1)))
                (swap16 (get_16 (ptr + 2)));
  set_16 ptr (swap16 0xFEDC);
  Printf.printf "%x %x %x\n%!"
                (swap16 (get_16 (ptr + 0)))
                (swap16 (get_16 (ptr + 1)))
                (swap16 (get_16 (ptr + 2)))

let () =
  set_32 ptr (swap32 0x12345678l);
  Printf.printf "%lx %lx %lx\n%!"
                (swap32 (get_32 (ptr + 0)))
                (swap32 (get_32 (ptr + 1)))
                (swap32 (get_32 (ptr + 2)));
  set_32 ptr (swap32 0xFEDCBA09l);
  Printf.printf "%lx %lx %lx\n%!"
                (swap32 (get_32 (ptr + 0)))
                (swap32 (get_32 (ptr + 1)))
                (swap32 (get_32 (ptr + 2)))

let () =
  set_64 ptr (swap64 0x1234567890ABCDEFL);
  Printf.printf "%Lx %Lx %Lx\n%!"
                (swap64 (get_64 (ptr + 0)))
                (swap64 (get_64 (ptr + 1)))
                (swap64 (get_64 (ptr + 2)));
  set_64 ptr (swap64 0xFEDCBA0987654321L);
  Printf.printf "%Lx %Lx %Lx\n%!"
                (swap64 (get_64 (ptr + 0)))
                (swap64 (get_64 (ptr + 1)))
                (swap64 (get_64 (ptr + 2)))
