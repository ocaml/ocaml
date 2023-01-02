(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt    *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Extensible buffers *)

(* The [inner_buffer] type ensures that the [length] and [buffer] fields are
   always synchronized, [length = Bytes.length buffer], even in presence
   of data races.
*)
type inner_buffer = {
  buffer: bytes;
  length: int;
}

type t =
 {mutable inner : inner_buffer;
  mutable position : int;
  initial_buffer : bytes}
(* Invariants: all parts of the code preserve the invariants that:
   - [inner.length = Bytes.length inner.buffer]
   In absence of data races, we also have
   - [0 <= b.position <= b.inner.length]

   Note in particular that [b.position = b.inner.length] is legal,
   it means that the buffer is full and will have to be extended
   before any further addition. *)

let create n =
 let n = if n < 1 then 1 else n in
 let n = if n > Sys.max_string_length then Sys.max_string_length else n in
 let s = Bytes.create n in
 { inner = { buffer = s; length = n}; position = 0; initial_buffer = s}

let contents b = Bytes.sub_string b.inner.buffer 0 b.position
let to_bytes b = Bytes.sub b.inner.buffer 0 b.position

let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > b.position - len
  then invalid_arg "Buffer.sub"
  else Bytes.sub_string b.inner.buffer ofs len


let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > src.position - len
             || dstoff < 0 || dstoff > (Bytes.length dst) - len
  then invalid_arg "Buffer.blit"
  else
    Bytes.blit src.inner.buffer srcoff dst dstoff len


let nth b ofs =
  let position = b.position in
  let {buffer;length} = b.inner in
  if ofs < 0 || ofs >= position || position > length then
   invalid_arg "Buffer.nth"
  else Bytes.unsafe_get buffer ofs


let length b = b.position

let clear b = b.position <- 0

let reset b =
  b.position <- 0;
  let inner =
    { buffer = b.initial_buffer; length = Bytes.length b.initial_buffer }
  in
  b.inner <- inner

(* [resize b more] ensures that [b.position + more <= b.inner.length] holds
   by dynamically extending [b.inner] if necessary -- and thus
   increasing [b.inner.length].
*)
let resize b more =
  let old_pos = b.position in
  let old_len = b.inner.length in
  let new_len = ref old_len in
  while old_pos + more > !new_len do new_len := 2 * !new_len done;
  if !new_len > Sys.max_string_length then begin
    if old_pos + more <= Sys.max_string_length
    then new_len := Sys.max_string_length
    else failwith "Buffer.add: cannot grow buffer"
  end;
  let new_buffer = Bytes.create !new_len in
  (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. *)
  Bytes.blit b.inner.buffer 0 new_buffer 0 b.position;
  b.inner <- { buffer = new_buffer; length = !new_len }

(* Note:
    Some of the functions below have a fast path when the inner
  buffer doesn't need to be extended.
    In this case, it is possible to use unsafe accesses on the
  contents of the [inner] field since its fields are immutable.
  In presence of data races, we may access the wrong inner buffer, but we
  will use this buffer safely.
  As soon as we need to resize the buffer, we fall back to safe accesses.
*)

let add_char b c =
  let pos = b.position in
  let {buffer;length} = b.inner in
  if pos >= length then (
    resize b 1;
    Bytes.set b.inner.buffer b.position c
  ) else
    Bytes.unsafe_set buffer pos c;
  b.position <- pos + 1

let uchar_utf_8_byte_length_max = 4
let uchar_utf_16_byte_length_max = 4

let rec add_utf_8_uchar b u =
  let pos = b.position in
  if pos >= b.inner.length then resize b uchar_utf_8_byte_length_max;
  let n = Bytes.set_utf_8_uchar b.inner.buffer pos u in
  if n = 0
  then (resize b uchar_utf_8_byte_length_max; add_utf_8_uchar b u)
  else (b.position <- pos + n)

let rec add_utf_16be_uchar b u =
  let pos = b.position in
  if pos >= b.inner.length then resize b uchar_utf_16_byte_length_max;
  let n = Bytes.set_utf_16be_uchar b.inner.buffer pos u in
  if n = 0
  then (resize b uchar_utf_16_byte_length_max; add_utf_16be_uchar b u)
  else (b.position <- pos + n)

let rec add_utf_16le_uchar b u =
  let pos = b.position in
  if pos >= b.inner.length then resize b uchar_utf_16_byte_length_max;
  let n = Bytes.set_utf_16le_uchar b.inner.buffer pos u in
  if n = 0
  then (resize b uchar_utf_16_byte_length_max; add_utf_16le_uchar b u)
  else (b.position <- pos + n)

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len
  then invalid_arg "Buffer.add_substring/add_subbytes";
  let position = b.position in
  let {buffer;length} = b.inner in
  let new_position = position + len in
  if new_position > length then (
    resize b len;
    Bytes.blit_string s offset b.inner.buffer b.position len
  ) else
    Bytes.unsafe_blit_string s offset buffer position len;
  b.position <- new_position

let add_subbytes b s offset len =
  add_substring b (Bytes.unsafe_to_string s) offset len

let add_string b s =
  let len = String.length s in
  let position = b.position in
  let {buffer; length} = b.inner in
  let new_position = b.position + len in
  if new_position > length then (
    resize b len;
    Bytes.blit_string s 0 b.inner.buffer b.position len;
  ) else
    Bytes.unsafe_blit_string s 0 buffer position len;
  b.position <- new_position

let add_bytes b s = add_string b (Bytes.unsafe_to_string s)

let add_buffer b bs =
  add_subbytes b bs.inner.buffer 0 bs.position

(* this (private) function could move into the standard library *)
let really_input_up_to ic buf ofs len =
  let rec loop ic buf ~already_read ~ofs ~to_read =
    if to_read = 0 then already_read
    else begin
      let r = input ic buf ofs to_read in
      if r = 0 then already_read
      else begin
        let already_read = already_read + r in
        let ofs = ofs + r in
        let to_read = to_read - r in
        loop ic buf ~already_read ~ofs ~to_read
      end
    end
  in loop ic buf ~already_read:0 ~ofs ~to_read:len


let unsafe_add_channel_up_to b ic len =
  if b.position + len > b.inner.length then resize b len;
  let n = really_input_up_to ic b.inner.buffer b.position len in
  b.position <- b.position + n;
  n

let add_channel b ic len =
  if len < 0 || len > Sys.max_string_length then   (* PR#5004 *)
    invalid_arg "Buffer.add_channel";
  let n = unsafe_add_channel_up_to b ic len in
  (* It is intentional that a consumer catching End_of_file
     will see the data written (see #6719, #7136). *)
  if n < len then raise End_of_file;
  ()

let output_buffer oc b =
  output oc b.inner.buffer 0 b.position

let closing = function
  | '(' -> ')'
  | '{' -> '}'
  | _ -> assert false

(* opening and closing: open and close characters, typically ( and )
   k: balance of opening and closing chars
   s: the string where we are searching
   start: the index where we start the search. *)
let advance_to_closing opening closing k s start =
  let rec advance k i lim =
    if i >= lim then raise Not_found else
    if s.[i] = opening then advance (k + 1) (i + 1) lim else
    if s.[i] = closing then
      if k = 0 then i else advance (k - 1) (i + 1) lim
    else advance k (i + 1) lim in
  advance k start (String.length s)

let advance_to_non_alpha s start =
  let rec advance i lim =
    if i >= lim then lim else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> advance (i + 1) lim
    | _ -> i in
  advance start (String.length s)

(* We are just at the beginning of an ident in s, starting at start. *)
let find_ident s start lim =
  if start >= lim then raise Not_found else
  match s.[start] with
  (* Parenthesized ident ? *)
  | '(' | '{' as c ->
     let new_start = start + 1 in
     let stop = advance_to_closing c (closing c) 0 s new_start in
     String.sub s new_start (stop - start - 1), stop + 1
  (* Regular ident *)
  | _ ->
     let stop = advance_to_non_alpha s (start + 1) in
     String.sub s start (stop - start), stop

(* Substitute $ident, $(ident), or ${ident} in s,
    according to the function mapping f. *)
let add_substitute b f s =
  let lim = String.length s in
  let rec subst previous i =
    if i < lim then begin
      match s.[i] with
      | '$' as current when previous = '\\' ->
         add_char b current;
         subst ' ' (i + 1)
      | '$' ->
         let j = i + 1 in
         let ident, next_i = find_ident s j lim in
         add_string b (f ident);
         subst ' ' next_i
      | current when previous == '\\' ->
         add_char b '\\';
         add_char b current;
         subst ' ' (i + 1)
      | '\\' as current ->
         subst current (i + 1)
      | current ->
         add_char b current;
         subst current (i + 1)
    end else
    if previous = '\\' then add_char b previous in
  subst ' ' 0

let truncate b len =
    if len < 0 || len > length b then
      invalid_arg "Buffer.truncate"
    else
      b.position <- len

(** {1 Iterators} *)

let to_seq b =
  let rec aux i () =
    (* Note that b.position is not a constant and cannot be lifted out of aux *)
    if i >= b.position then Seq.Nil
    else
      let x = Bytes.get b.inner.buffer i in
      Seq.Cons (x, aux (i+1))
  in
  aux 0

let to_seqi b =
  let rec aux i () =
    (* Note that b.position is not a constant and cannot be lifted out of aux *)
    if i >= b.position then Seq.Nil
    else
      let x = Bytes.get b.inner.buffer i in
      Seq.Cons ((i,x), aux (i+1))
  in
  aux 0

let add_seq b seq = Seq.iter (add_char b) seq

let of_seq i =
  let b = create 32 in
  add_seq b i;
  b

(** {6 Binary encoding of integers} *)

external unsafe_set_int8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"
external unsafe_set_int16 : bytes -> int -> int -> unit = "%caml_bytes_set16u"
external unsafe_set_int32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
external unsafe_set_int64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"
external set_int8 : bytes -> int -> int -> unit = "%bytes_safe_set"
external set_int16 : bytes -> int -> int -> unit = "%caml_bytes_set16"
external set_int32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32"
external set_int64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64"

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"


let add_int8 b x =
  let position = b.position in
  let {length; buffer} = b.inner in
  let new_position = position + 1 in
  if new_position > length then (
    resize b 1;
    set_int8 b.inner.buffer b.position x
  ) else
    unsafe_set_int8 buffer position x;
  b.position <- new_position

let add_int16_ne b x =
  let position = b.position in
  let {length; buffer} = b.inner in
  let new_position = position + 2 in
  if new_position > length then (
    resize b 2;
    set_int16 b.inner.buffer b.position x
  ) else
    unsafe_set_int16 buffer position x;
  b.position <- new_position

let add_int32_ne b x =
  let position = b.position in
  let {length; buffer} = b.inner in
  let new_position = position + 4 in
  if new_position > length then (
    resize b 4;
    set_int32 b.inner.buffer b.position x
  ) else
    unsafe_set_int32 buffer position x;
  b.position <- new_position

let add_int64_ne b x =
  let position = b.position in
  let {length; buffer} = b.inner in
  let new_position = position + 8 in
  if new_position > length then (
    resize b 8;
    set_int64 b.inner.buffer b.position x
  ) else
    unsafe_set_int64 buffer position x;
  b.position <- new_position

let add_int16_le b x =
  add_int16_ne b (if Sys.big_endian then swap16 x else x)

let add_int16_be b x =
  add_int16_ne b (if Sys.big_endian then x else swap16 x)

let add_int32_le b x =
  add_int32_ne b (if Sys.big_endian then swap32 x else x)

let add_int32_be b x =
  add_int32_ne b (if Sys.big_endian then x else swap32 x)

let add_int64_le b x =
  add_int64_ne b (if Sys.big_endian then swap64 x else x)

let add_int64_be b x =
  add_int64_ne b (if Sys.big_endian then x else swap64 x)

let add_uint8 = add_int8
let add_uint16_ne = add_int16_ne
let add_uint16_le = add_int16_le
let add_uint16_be = add_int16_be
