(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Byte sequence operations *)

(* WARNING: Some functions in this file are duplicated in string.ml for
   efficiency reasons. When you modify the one in this file you need to
   modify its duplicate in string.ml.
   These functions have a "duplicated" comment above their definition.
*)

external length : bytes -> int = "%bytes_length"
external string_length : string -> int = "%string_length"
external get : bytes -> int -> char = "%bytes_safe_get"
external set : bytes -> int -> char -> unit = "%bytes_safe_set"
external create : int -> bytes = "caml_create_bytes"
external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_fill : bytes -> int -> int -> char -> unit
                     = "caml_fill_bytes" [@@noalloc]
external unsafe_to_string : bytes -> string = "%bytes_to_string"
external unsafe_of_string : string -> bytes = "%bytes_of_string"

external unsafe_blit : bytes -> int -> bytes -> int -> int -> unit
                     = "caml_blit_bytes" [@@noalloc]
external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]

let make n c =
  let s = create n in
  unsafe_fill s 0 n c;
  s

let init n f =
  let s = create n in
  for i = 0 to n - 1 do
    unsafe_set s i (f i)
  done;
  s

let empty = create 0

let copy s =
  let len = length s in
  let r = create len in
  unsafe_blit s 0 r 0 len;
  r

let to_string b = unsafe_to_string (copy b)
let of_string s = copy (unsafe_of_string s)

let sub s ofs len =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.sub / Bytes.sub"
  else begin
    let r = create len in
    unsafe_blit s ofs r 0 len;
    r
  end

let sub_string b ofs len = unsafe_to_string (sub b ofs len)

(* addition with an overflow check *)
let (++) a b =
  let c = a + b in
  match a < 0, b < 0, c < 0 with
  | true , true , false
  | false, false, true  -> invalid_arg "Bytes.extend" (* overflow *)
  | _ -> c

let extend s left right =
  let len = length s ++ left ++ right in
  let r = create len in
  let (srcoff, dstoff) = if left < 0 then -left, 0 else 0, left in
  let cpylen = Int.min (length s - srcoff) (len - dstoff) in
  if cpylen > 0 then unsafe_blit s srcoff r dstoff cpylen;
  r

let fill s ofs len c =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.fill / Bytes.fill"
  else unsafe_fill s ofs len c

let blit s1 ofs1 s2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length s1 - len
             || ofs2 < 0 || ofs2 > length s2 - len
  then invalid_arg "Bytes.blit"
  else unsafe_blit s1 ofs1 s2 ofs2 len

let blit_string s1 ofs1 s2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > string_length s1 - len
             || ofs2 < 0 || ofs2 > length s2 - len
  then invalid_arg "String.blit / Bytes.blit_string"
  else unsafe_blit_string s1 ofs1 s2 ofs2 len

(* duplicated in string.ml *)
let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

(* duplicated in string.ml *)
let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let ensure_ge (x:int) y = if x >= y then x else invalid_arg "Bytes.concat"

let rec sum_lengths acc seplen = function
  | [] -> acc
  | hd :: [] -> length hd + acc
  | hd :: tl -> sum_lengths (ensure_ge (length hd + seplen + acc) acc) seplen tl

let rec unsafe_blits dst pos sep seplen = function
    [] -> dst
  | hd :: [] ->
    unsafe_blit hd 0 dst pos (length hd); dst
  | hd :: tl ->
    unsafe_blit hd 0 dst pos (length hd);
    unsafe_blit sep 0 dst (pos + length hd) seplen;
    unsafe_blits dst (pos + length hd + seplen) sep seplen tl

let concat sep = function
    [] -> empty
  | l -> let seplen = length sep in
          unsafe_blits
            (create (sum_lengths 0 seplen l))
            0 sep seplen l

let cat s1 s2 =
  let l1 = length s1 in
  let l2 = length s2 in
  let r = create (l1 + l2) in
  unsafe_blit s1 0 r 0 l1;
  unsafe_blit s2 0 r l1 l2;
  r


external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let trim s =
  let len = length s in
  let i = ref 0 in
  while !i < len && is_space (unsafe_get s !i) do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && is_space (unsafe_get s !j) do
    decr j
  done;
  if !j >= !i then
    sub s !i (!j - !i + 1)
  else
    empty

let unsafe_escape s =
  (* We perform two passes on the input sequence, one to compute the
     result size and one to write the result.

     #11508, #11509: This logic would be incorrect in presence of
     concurrent modification to the input, making the use of
     [unsafe_set] below memory-unsafe.

     Precondition: This function may be safely called on:
     - an immutable byte sequence
     - a uniquely-owned byte sequence (the function takes ownership)

     In either case we return a uniquely-owned byte sequence.
  *)
  let n = ref 0 in
  for i = 0 to length s - 1 do
    n := !n +
      (match unsafe_get s i with
       | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | _ -> 4)
  done;
  if !n = length s then s
  else begin
    let s' = create !n in
    n := 0;
    for i = 0 to length s - 1 do
      begin match unsafe_get s i with
      | ('\"' | '\\') as c ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
      | '\n' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
      | '\t' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
      | '\r' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
      | '\b' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> unsafe_set s' !n c
      | c ->
          let a = char_code c in
          unsafe_set s' !n '\\';
          incr n;
          unsafe_set s' !n (char_chr (48 + a / 100));
          incr n;
          unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
          incr n;
          unsafe_set s' !n (char_chr (48 + a mod 10));
      end;
      incr n
    done;
    s'
  end

let escaped b =
  let b = copy b in
  (* We copy our input to obtain a uniquely-owned byte sequence [b]
     to satisfy [unsafe_escape]'s precondition *)
  unsafe_escape b

let map f s =
  let l = length s in
  if l = 0 then s else begin
    let r = create l in
    for i = 0 to l - 1 do unsafe_set r i (f (unsafe_get s i)) done;
    r
  end

let mapi f s =
  let l = length s in
  if l = 0 then s else begin
    let r = create l in
    for i = 0 to l - 1 do unsafe_set r i (f i (unsafe_get s i)) done;
    r
  end

let fold_left f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let fold_right f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f (unsafe_get a i) !r
  done;
  !r

let exists p s =
  let n = length s in
  let rec loop i =
    if i = n then false
    else if p (unsafe_get s i) then true
    else loop (succ i) in
  loop 0

let for_all p s =
  let n = length s in
  let rec loop i =
    if i = n then true
    else if p (unsafe_get s i) then loop (succ i)
    else false in
  loop 0

let uppercase_ascii s = map Char.uppercase_ascii s
let lowercase_ascii s = map Char.lowercase_ascii s

let apply1 f s =
  if length s = 0 then s else begin
    let r = copy s in
    unsafe_set r 0 (f(unsafe_get s 0));
    r
  end

let capitalize_ascii s = apply1 Char.uppercase_ascii s
let uncapitalize_ascii s = apply1 Char.lowercase_ascii s

(* duplicated in string.ml *)
let starts_with ~prefix s =
  let len_s = length s
  and len_pre = length prefix in
  let rec aux i =
    if i = len_pre then true
    else if unsafe_get s i <> unsafe_get prefix i then false
    else aux (i + 1)
  in len_s >= len_pre && aux 0

(* duplicated in string.ml *)
let ends_with ~suffix s =
  let len_s = length s
  and len_suf = length suffix in
  let diff = len_s - len_suf in
  let rec aux i =
    if i = len_suf then true
    else if unsafe_get s (diff + i) <> unsafe_get suffix i then false
    else aux (i + 1)
  in diff >= 0 && aux 0

(* duplicated in string.ml *)
let rec index_rec s lim i c =
  if i >= lim then raise Not_found else
  if unsafe_get s i = c then i else index_rec s lim (i + 1) c

(* duplicated in string.ml *)
let index s c = index_rec s (length s) 0 c

(* duplicated in string.ml *)
let rec index_rec_opt s lim i c =
  if i >= lim then None else
  if unsafe_get s i = c then Some i else index_rec_opt s lim (i + 1) c

(* duplicated in string.ml *)
let index_opt s c = index_rec_opt s (length s) 0 c

(* duplicated in string.ml *)
let index_from s i c =
  let l = length s in
  if i < 0 || i > l then invalid_arg "String.index_from / Bytes.index_from" else
  index_rec s l i c

(* duplicated in string.ml *)
let index_from_opt s i c =
  let l = length s in
  if i < 0 || i > l then
    invalid_arg "String.index_from_opt / Bytes.index_from_opt"
  else
    index_rec_opt s l i c

(* duplicated in string.ml *)
let rec rindex_rec s i c =
  if i < 0 then raise Not_found else
  if unsafe_get s i = c then i else rindex_rec s (i - 1) c

(* duplicated in string.ml *)
let rindex s c = rindex_rec s (length s - 1) c

(* duplicated in string.ml *)
let rindex_from s i c =
  if i < -1 || i >= length s then
    invalid_arg "String.rindex_from / Bytes.rindex_from"
  else
    rindex_rec s i c

(* duplicated in string.ml *)
let rec rindex_rec_opt s i c =
  if i < 0 then None else
  if unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c

(* duplicated in string.ml *)
let rindex_opt s c = rindex_rec_opt s (length s - 1) c

(* duplicated in string.ml *)
let rindex_from_opt s i c =
  if i < -1 || i >= length s then
    invalid_arg "String.rindex_from_opt / Bytes.rindex_from_opt"
  else
    rindex_rec_opt s i c


(* duplicated in string.ml *)
let contains_from s i c =
  let l = length s in
  if i < 0 || i > l then
    invalid_arg "String.contains_from / Bytes.contains_from"
  else
    try ignore (index_rec s l i c); true with Not_found -> false


(* duplicated in string.ml *)
let contains s c = contains_from s 0 c

(* duplicated in string.ml *)
let rcontains_from s i c =
  if i < 0 || i >= length s then
    invalid_arg "String.rcontains_from / Bytes.rcontains_from"
  else
    try ignore (rindex_rec s i c); true with Not_found -> false


type t = bytes

let compare (x: t) (y: t) = Stdlib.compare x y
external equal : t -> t -> bool = "caml_bytes_equal" [@@noalloc]

(* duplicated in string.ml *)
let split_on_char sep s =
  let r = ref [] in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if unsafe_get s i = sep then begin
      r := sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  sub s 0 !j :: !r

(** {1 Iterators} *)

let to_seq s =
  let rec aux i () =
    if i = length s then Seq.Nil
    else
      let x = get s i in
      Seq.Cons (x, aux (i+1))
  in
  aux 0

let to_seqi s =
  let rec aux i () =
    if i = length s then Seq.Nil
    else
      let x = get s i in
      Seq.Cons ((i,x), aux (i+1))
  in
  aux 0

let of_seq i =
  let n = ref 0 in
  let buf = ref (make 256 '\000') in
  let resize () =
    (* resize *)
    let new_len = Int.min (2 * length !buf) Sys.max_string_length in
    if length !buf = new_len then failwith "Bytes.of_seq: cannot grow bytes";
    let new_buf = make new_len '\000' in
    blit !buf 0 new_buf 0 !n;
    buf := new_buf
  in
  Seq.iter
    (fun c ->
       if !n = length !buf then resize();
       set !buf !n c;
       incr n)
    i;
  sub !buf 0 !n

(** {6 Binary encoding/decoding of integers} *)

(* The get_ functions are all duplicated in string.ml *)

external unsafe_get_uint8 : bytes -> int -> int = "%bytes_unsafe_get"
external unsafe_get_uint16_ne : bytes -> int -> int = "%caml_bytes_get16u"
external get_uint8 : bytes -> int -> int = "%bytes_safe_get"
external get_uint16_ne : bytes -> int -> int = "%caml_bytes_get16"
external get_int32_ne : bytes -> int -> int32 = "%caml_bytes_get32"
external get_int64_ne : bytes -> int -> int64 = "%caml_bytes_get64"

external unsafe_set_uint8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"
external unsafe_set_uint16_ne : bytes -> int -> int -> unit
                              = "%caml_bytes_set16u"
external set_int8 : bytes -> int -> int -> unit = "%bytes_safe_set"
external set_int16_ne : bytes -> int -> int -> unit = "%caml_bytes_set16"
external set_int32_ne : bytes -> int -> int32 -> unit = "%caml_bytes_set32"
external set_int64_ne : bytes -> int -> int64 -> unit = "%caml_bytes_set64"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let unsafe_get_uint16_le b i =
  if Sys.big_endian
  then swap16 (unsafe_get_uint16_ne b i)
  else unsafe_get_uint16_ne b i

let unsafe_get_uint16_be b i =
  if Sys.big_endian
  then unsafe_get_uint16_ne b i
  else swap16 (unsafe_get_uint16_ne b i)

let get_int8 b i =
  ((get_uint8 b i) lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)

let get_uint16_le b i =
  if Sys.big_endian then swap16 (get_uint16_ne b i)
  else get_uint16_ne b i

let get_uint16_be b i =
  if not Sys.big_endian then swap16 (get_uint16_ne b i)
  else get_uint16_ne b i

let get_int16_ne b i =
  ((get_uint16_ne b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_le b i =
  ((get_uint16_le b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_be b i =
  ((get_uint16_be b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int32_le b i =
  if Sys.big_endian then swap32 (get_int32_ne b i)
  else get_int32_ne b i

let get_int32_be b i =
  if not Sys.big_endian then swap32 (get_int32_ne b i)
  else get_int32_ne b i

let get_int64_le b i =
  if Sys.big_endian then swap64 (get_int64_ne b i)
  else get_int64_ne b i

let get_int64_be b i =
  if not Sys.big_endian then swap64 (get_int64_ne b i)
  else get_int64_ne b i

let unsafe_set_uint16_le b i x =
  if Sys.big_endian
  then unsafe_set_uint16_ne b i (swap16 x)
  else unsafe_set_uint16_ne b i x

let unsafe_set_uint16_be b i x =
  if Sys.big_endian
  then unsafe_set_uint16_ne b i x else
  unsafe_set_uint16_ne b i (swap16 x)

let set_int16_le b i x =
  if Sys.big_endian then set_int16_ne b i (swap16 x)
  else set_int16_ne b i x

let set_int16_be b i x =
  if not Sys.big_endian then set_int16_ne b i (swap16 x)
  else set_int16_ne b i x

let set_int32_le b i x =
  if Sys.big_endian then set_int32_ne b i (swap32 x)
  else set_int32_ne b i x

let set_int32_be b i x =
  if not Sys.big_endian then set_int32_ne b i (swap32 x)
  else set_int32_ne b i x

let set_int64_le b i x =
  if Sys.big_endian then set_int64_ne b i (swap64 x)
  else set_int64_ne b i x

let set_int64_be b i x =
  if not Sys.big_endian then set_int64_ne b i (swap64 x)
  else set_int64_ne b i x

let set_uint8 = set_int8
let set_uint16_ne = set_int16_ne
let set_uint16_be = set_int16_be
let set_uint16_le = set_int16_le

(* UTF codecs and validations *)

let dec_invalid = Uchar.utf_decode_invalid
let[@inline] dec_ret n u = Uchar.utf_decode n (Uchar.unsafe_of_int u)

(* In case of decoding error, if we error on the first byte, we
   consume the byte, otherwise we consume the [n] bytes preceding
   the erroring byte.

   This means that if a client uses decodes without caring about
   validity it naturally replace bogus data with Uchar.rep according
   to the WHATWG Encoding standard. Other schemes are possible by
   consulting the number of used bytes on invalid decodes. For more
   details see https://hsivonen.fi/broken-utf-8/

   For this reason in [get_utf_8_uchar] we gradually check the next
   byte is available rather than doing it immediately after the
   first byte. Contrast with [is_valid_utf_8]. *)

(* UTF-8 *)

let[@inline] not_in_x80_to_xBF b = b lsr 6 <> 0b10
let[@inline] not_in_xA0_to_xBF b = b lsr 5 <> 0b101
let[@inline] not_in_x80_to_x9F b = b lsr 5 <> 0b100
let[@inline] not_in_x90_to_xBF b = b < 0x90 || 0xBF < b
let[@inline] not_in_x80_to_x8F b = b lsr 4 <> 0x8

let[@inline] utf_8_uchar_2 b0 b1 =
  ((b0 land 0x1F) lsl 6) lor
  ((b1 land 0x3F))

let[@inline] utf_8_uchar_3 b0 b1 b2 =
  ((b0 land 0x0F) lsl 12) lor
  ((b1 land 0x3F) lsl 6) lor
  ((b2 land 0x3F))

let[@inline] utf_8_uchar_4 b0 b1 b2 b3 =
  ((b0 land 0x07) lsl 18) lor
  ((b1 land 0x3F) lsl 12) lor
  ((b2 land 0x3F) lsl 6) lor
  ((b3 land 0x3F))

let get_utf_8_uchar b i =
  let b0 = get_uint8 b i in (* raises if [i] is not a valid index. *)
  let get = unsafe_get_uint8 in
  let max = length b - 1 in
  match Char.unsafe_chr b0 with (* See The Unicode Standard, Table 3.7 *)
  | '\x00' .. '\x7F' -> dec_ret 1 b0
  | '\xC2' .. '\xDF' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
      dec_ret 2 (utf_8_uchar_2 b0 b1)
  | '\xE0' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_xA0_to_xBF b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
  | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
  | '\xED' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_x9F b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
  | '\xF0' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x90_to_xBF b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      let i = i + 1 in if i > max then dec_invalid 3 else
      let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
      dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
  | '\xF1' .. '\xF3' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      let i = i + 1 in if i > max then dec_invalid 3 else
      let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
      dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
  | '\xF4' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
      let b1 = get b i in if not_in_x80_to_x8F b1 then dec_invalid 1 else
      let i = i + 1 in if i > max then dec_invalid 2 else
      let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
      let i = i + 1 in if i > max then dec_invalid 3 else
      let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
      dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
  | _ -> dec_invalid 1

let set_utf_8_uchar b i u =
  let set = unsafe_set_uint8 in
  let max = length b - 1 in
  match Uchar.to_int u with
  | u when u < 0 -> assert false
  | u when u <= 0x007F ->
      set_uint8 b i u;
      1
  | u when u <= 0x07FF ->
      let last = i + 1 in
      if last > max then 0 else
      (set_uint8 b i (0xC0 lor (u lsr 6));
       set b last (0x80 lor (u land 0x3F));
       2)
  | u when u <= 0xFFFF ->
      let last = i + 2 in
      if last > max then 0 else
      (set_uint8 b i (0xE0 lor (u lsr 12));
       set b (i + 1) (0x80 lor ((u lsr 6) land 0x3F));
       set b last (0x80 lor (u land 0x3F));
       3)
  | u when u <= 0x10FFFF ->
      let last = i + 3 in
      if last > max then 0 else
      (set_uint8 b i (0xF0 lor (u lsr 18));
       set b (i + 1) (0x80 lor ((u lsr 12) land 0x3F));
       set b (i + 2) (0x80 lor ((u lsr 6) land 0x3F));
       set b last (0x80 lor (u land 0x3F));
       4)
  | _ -> assert false

let is_valid_utf_8 b =
  let rec loop max b i =
    if i > max then true else
    let get = unsafe_get_uint8 in
    match Char.unsafe_chr (get b i) with
    | '\x00' .. '\x7F' -> loop max b (i + 1)
    | '\xC2' .. '\xDF' ->
        let last = i + 1 in
        if last > max
        || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
    | '\xE0' ->
        let last = i + 2 in
        if last > max
        || not_in_xA0_to_xBF (get b (i + 1))
        || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
    | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
        let last = i + 2 in
        if last > max
        || not_in_x80_to_xBF (get b (i + 1))
        || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
    | '\xED' ->
        let last = i + 2 in
        if last > max
        || not_in_x80_to_x9F (get b (i + 1))
        || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
    | '\xF0' ->
        let last = i + 3 in
        if last > max
        || not_in_x90_to_xBF (get b (i + 1))
        || not_in_x80_to_xBF (get b (i + 2))
        || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
    | '\xF1' .. '\xF3' ->
        let last = i + 3 in
        if last > max
        || not_in_x80_to_xBF (get b (i + 1))
        || not_in_x80_to_xBF (get b (i + 2))
        || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
    | '\xF4' ->
        let last = i + 3 in
        if last > max
        || not_in_x80_to_x8F (get b (i + 1))
        || not_in_x80_to_xBF (get b (i + 2))
        || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
    | _ -> false
  in
  loop (length b - 1) b 0

(* UTF-16BE *)

let get_utf_16be_uchar b i =
  let get = unsafe_get_uint16_be in
  let max = length b - 1 in
  if i < 0 || i > max then invalid_arg "index out of bounds" else
  if i = max then dec_invalid 1 else
  match get b i with
  | u when u < 0xD800 || u > 0xDFFF -> dec_ret 2 u
  | u when u > 0xDBFF -> dec_invalid 2
  | hi -> (* combine [hi] with a low surrogate *)
      let last = i + 3 in
      if last > max then dec_invalid (max - i + 1) else
      match get b (i + 2) with
      | u when u < 0xDC00 || u > 0xDFFF -> dec_invalid 2 (* retry here *)
      | lo ->
          let u = (((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000 in
          dec_ret 4 u

let set_utf_16be_uchar b i u =
  let set = unsafe_set_uint16_be in
  let max = length b - 1 in
  if i < 0 || i > max then invalid_arg "index out of bounds" else
  match Uchar.to_int u with
  | u when u < 0 -> assert false
  | u when u <= 0xFFFF ->
      let last = i + 1 in
      if last > max then 0 else (set b i u; 2)
  | u when u <= 0x10FFFF ->
      let last = i + 3 in
      if last > max then 0 else
      let u' = u - 0x10000 in
      let hi = (0xD800 lor (u' lsr 10)) in
      let lo = (0xDC00 lor (u' land 0x3FF)) in
      set b i hi; set b (i + 2) lo; 4
  | _ -> assert false

let is_valid_utf_16be b =
  let rec loop max b i =
    let get = unsafe_get_uint16_be in
    if i > max then true else
    if i = max then false else
    match get b i with
    | u when u < 0xD800 || u > 0xDFFF -> loop max b (i + 2)
    | u when u > 0xDBFF -> false
    | _hi ->
        let last = i + 3 in
        if last > max then false else
        match get b (i + 2) with
        | u when u < 0xDC00 || u > 0xDFFF -> false
        | _lo -> loop max b (i + 4)
  in
  loop (length b - 1) b 0

(* UTF-16LE *)

let get_utf_16le_uchar b i =
  let get = unsafe_get_uint16_le in
  let max = length b - 1 in
  if i < 0 || i > max then invalid_arg "index out of bounds" else
  if i = max then dec_invalid 1 else
  match get b i with
  | u when u < 0xD800 || u > 0xDFFF -> dec_ret 2 u
  | u when u > 0xDBFF -> dec_invalid 2
  | hi -> (* combine [hi] with a low surrogate *)
      let last = i + 3 in
      if last > max then dec_invalid (max - i + 1) else
      match get b (i + 2) with
      | u when u < 0xDC00 || u > 0xDFFF -> dec_invalid 2 (* retry here *)
      | lo ->
          let u = (((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000 in
          dec_ret 4 u

let set_utf_16le_uchar b i u =
  let set = unsafe_set_uint16_le in
  let max = length b - 1 in
  if i < 0 || i > max then invalid_arg "index out of bounds" else
  match Uchar.to_int u with
  | u when u < 0 -> assert false
  | u when u <= 0xFFFF ->
      let last = i + 1 in
      if last > max then 0 else (set b i u; 2)
  | u when u <= 0x10FFFF ->
      let last = i + 3 in
      if last > max then 0 else
      let u' = u - 0x10000 in
      let hi = (0xD800 lor (u' lsr 10)) in
      let lo = (0xDC00 lor (u' land 0x3FF)) in
      set b i hi; set b (i + 2) lo; 4
  | _ -> assert false

let is_valid_utf_16le b =
  let rec loop max b i =
    let get = unsafe_get_uint16_le in
    if i > max then true else
    if i = max then false else
    match get b i with
    | u when u < 0xD800 || u > 0xDFFF -> loop max b (i + 2)
    | u when u > 0xDBFF -> false
    | _hi ->
        let last = i + 3 in
        if last > max then false else
        match get b (i + 2) with
        | u when u < 0xDC00 || u > 0xDFFF -> false
        | _lo -> loop max b (i + 4)
  in
  loop (length b - 1) b 0
