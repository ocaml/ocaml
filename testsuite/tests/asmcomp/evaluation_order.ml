(* TEST
*)
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external caml_bytes_get_16 : bytes -> int -> int = "%caml_bytes_get16"
external caml_bytes_set_16 : bytes -> int -> int -> unit = "%caml_bytes_set16"

open Bigarray
type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

external caml_bigstring_get_16 :
  bigstring -> int -> int = "%caml_bigstring_get16"

external caml_bigstring_set_16 :
  bigstring -> int -> int -> unit = "%caml_bigstring_set16"

let bigstring_of_string s =
  let a = Array1.create char c_layout (String.length s) in
  for i = 0 to String.length s - 1 do
    a.{i} <- s.[i]
  done;
  a

let () =
  (* stringref_safe *)
  String.get (print_endline "hello"; "foo") (print_endline "world"; 0)
  |> Printf.printf "%c\n";

  (* string_load *)
  caml_bytes_get_16 (print_endline "hello"; Bytes.make 10 '\x00')
    (print_endline "world"; 0)
  |> Printf.printf "%x\n";

  (* bigstring_load *)
  caml_bigstring_get_16 (print_endline "hello";
                         bigstring_of_string (String.make 10 '\x00'))
    (print_endline "world"; 0)
  |> Printf.printf "%x\n";

  (* bytes_set *)
  caml_bytes_set_16 (print_endline "a"; Bytes.make 10 '\x00')
    (print_endline "b"; 0)
    (print_endline "c"; 0xFF);

  (* bigstring_set *)
  caml_bigstring_set_16 (print_endline "a";
                         bigstring_of_string (String.make 10 '\x00'))
    (print_endline "b"; 0)
    (print_endline "c"; 0xFF);

  (* mk_compare_ints_untagged *)
  print_int (compare (print_endline "A"; Sys.opaque_identity (2))
               (print_endline "B"; Sys.opaque_identity (3)));
  print_newline ();

  (* mk_compare_floats *)
  print_int (compare (print_endline "A"; Sys.opaque_identity (2.0))
               (print_endline "B"; Sys.opaque_identity (3.5)));
  print_newline ();

  (* bytesset_safe *)
  Bytes.set (print_endline "a"; Bytes.make 10 '\x00')
    (print_endline "b"; 0)
    (print_endline "c"; 'c');

  (* safe_div_bi *)
  Printf.printf "%nd\n"
    (Nativeint.div (print_endline "A"; Sys.opaque_identity (6n))
               (print_endline "B"; Sys.opaque_identity (3n)));

  (* arrayref_unsafe *)
  let[@inline never] test_arrayref_unsafe
    : type t . t array -> int -> (t -> string) -> unit =
    fun a i c ->
      print_endline (c (Array.unsafe_get (print_endline "A"; a) (print_endline "B"; i)))
  in
  test_arrayref_unsafe [| "1";"2";"3" |] 0 Fun.id;

  ()
