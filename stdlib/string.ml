(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* String operations *)

external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : string -> int -> char -> unit = "%string_safe_set"
external create : int -> string = "caml_create_string"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int -> string -> int -> int -> unit
                     = "caml_blit_string" "noalloc"
external unsafe_fill : string -> int -> int -> char -> unit
                     = "caml_fill_string" "noalloc"

let make n c =
  let s = create n in
  unsafe_fill s 0 n c;
  s

let copy s =
  let len = length s in
  let r = create len in
  unsafe_blit s 0 r 0 len;
  r

let sub s ofs len =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.sub"
  else begin
    let r = create len in
    unsafe_blit s ofs r 0 len;
    r
  end

let fill s ofs len c =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.fill"
  else unsafe_fill s ofs len c

let blit s1 ofs1 s2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length s1 - len
             || ofs2 < 0 || ofs2 > length s2 - len
  then invalid_arg "String.blit"
  else unsafe_blit s1 ofs1 s2 ofs2 len

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let concat sep l =
  match l with
    [] -> ""
  | hd :: tl ->
      let num = ref 0 and len = ref 0 in
      List.iter (fun s -> incr num; len := !len + length s) l;
      let r = create (!len + length sep * (!num - 1)) in
      unsafe_blit hd 0 r 0 (length hd);
      let pos = ref(length hd) in
      List.iter
        (fun s ->
          unsafe_blit sep 0 r !pos (length sep);
          pos := !pos + length sep;
          unsafe_blit s 0 r !pos (length s);
          pos := !pos + length s)
        tl;
      r

let split sep s =
  let sep_max = length sep - 1 in
  if sep_max < 0 then invalid_arg "String.split: empty separator" else
  let s_max = length s - 1 in
  if s_max < 0 then [""] else
  let acc = ref [] in
  let sub_start = ref 0 in
  let k = ref 0 in 
  let i = ref 0 in 
  (* We build the substrings by running from the start of [s] to the
     end with [i] trying to match the first character of [sep] in
     [s]. If this matches, we verify that the whole [sep] is matched
     using [k]. If this matches we extract a substring from the start
     of the current substring [sub_start] to [!i - 1] (the position
     before the [sep] we found).  We then continue to try to match
     with [i] by starting after the [sep] we just found, this is also
     becomes the start position of the next substring. If [i] is such
     that no separator can be found we exit the loop and make a
     substring from [sub_start] until the end of the string. *)
  while (!i + sep_max <= s_max) do 
    if unsafe_get s !i <> unsafe_get sep 0 then incr i else 
    begin 
      (* Check remaining [sep] chars match, access to unsafe s (!i + !k) is
         guaranteed by loop invariant. *)
      k := 1; 
      while (!k <= sep_max && unsafe_get s (!i + !k) = unsafe_get sep !k) 
      do incr k done;
      if !k <= sep_max then (* no match *) incr i else begin 
        let new_sub_start = !i + sep_max + 1 in
        let sub_end = !i - 1 in 
        let sub_len = sub_end - !sub_start + 1 in 
        acc := sub s !sub_start sub_len :: !acc; 
        sub_start := new_sub_start; 
        i := new_sub_start;
      end
    end
  done;
  List.rev (sub s !sub_start (s_max - !sub_start + 1) :: !acc)

let rsplit sep s = 
  let sep_max = length sep - 1 in
  if sep_max < 0 then invalid_arg "String.rsplit: empty separator" else 
  let s_max = length s - 1 in
  if s_max < 0 then [""] else
  let acc = ref [] in
  let sub_end = ref s_max in
  let k = ref 0 in
  let i = ref s_max in
  (* We build the substrings by running from the end of [s] to the
     start with [i] trying to match the last character of [sep] in
     [s]. If this matches, we verify that the whole [sep] is matched
     using [k] (we do that backwards). If this matches we extract a
     substring from the end of the current substring [sub_end] to [!i
     + 1] (the position after the [sep] we found).  We then continue
     to try to match with [i] by starting before the [sep] we just
     found, this is also becomes the end position of the next
     substring. If [i] is such that no separator can be found we exit
     the loop and make a substring from the begining of the string
     until until [sub_end]. string. *)
  while (!i >= sep_max) do
    if unsafe_get s !i <> unsafe_get sep sep_max then decr i else begin
      (* Check remaining [sep] chars match, access to unsafe_get 
         s (sep_start + !k) is guaranteed by loop invariant. *)
      let sep_start = !i - sep_max in
      k := sep_max - 1;
      while (!k >= 0 && unsafe_get s (sep_start + !k) = unsafe_get sep !k)
      do decr k done;
      if !k >= 0 then (* no match *) decr i else begin 
        let new_sub_end = sep_start - 1 in
        let sub_start = !i + 1 in 
        let sub_len = !sub_end - sub_start + 1 in
        acc := sub s sub_start sub_len :: !acc; 
        sub_end := new_sub_end;
        i := new_sub_end;
      end
    end
  done;
  sub s 0 (!sub_end + 1) :: !acc

external is_printable: char -> bool = "caml_is_printable"
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
  if !i = 0 && !j = len - 1 then
    s
  else if !j >= !i then
    sub s !i (!j - !i + 1)
  else
    ""

let escaped s =
  let n = ref 0 in
    for i = 0 to length s - 1 do
      n := !n +
        (match unsafe_get s i with
         | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
         | c -> if is_printable c then 1 else 4)
    done;
    if !n = length s then s else begin
      let s' = create !n in
        n := 0;
        for i = 0 to length s - 1 do
          begin
            match unsafe_get s i with
            | ('"' | '\\') as c ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
            | '\n' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
            | '\t' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
            | '\r' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
            | '\b' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'b'
            | c ->
                if is_printable c then
                  unsafe_set s' !n c
                else begin
                  let a = char_code c in
                  unsafe_set s' !n '\\';
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a / 100));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a mod 10))
                end
          end;
          incr n
        done;
        s'
      end

let map f s =
  let l = length s in
  if l = 0 then s else begin
    let r = create l in
    for i = 0 to l - 1 do unsafe_set r i (f(unsafe_get s i)) done;
    r
  end

let uppercase s = map Char.uppercase s
let lowercase s = map Char.lowercase s

let apply1 f s =
  if length s = 0 then s else begin
    let r = copy s in
    unsafe_set r 0 (f(unsafe_get s 0));
    r
  end

let capitalize s = apply1 Char.uppercase s
let uncapitalize s = apply1 Char.lowercase s

let rec index_rec s lim i c =
  if i >= lim then raise Not_found else
  if unsafe_get s i = c then i else index_rec s lim (i + 1) c;;

let index s c = index_rec s (length s) 0 c;;

let index_from s i c =
  let l = length s in
  if i < 0 || i > l then invalid_arg "String.index_from" else
  index_rec s l i c;;

let rec rindex_rec s i c =
  if i < 0 then raise Not_found else
  if unsafe_get s i = c then i else rindex_rec s (i - 1) c;;

let rindex s c = rindex_rec s (length s - 1) c;;

let rindex_from s i c =
  if i < -1 || i >= length s then invalid_arg "String.rindex_from" else
  rindex_rec s i c;;

let contains_from s i c =
  let l = length s in
  if i < 0 || i > l then invalid_arg "String.contains_from" else
  try ignore (index_rec s l i c); true with Not_found -> false;;

let contains s c = contains_from s 0 c;;

let rcontains_from s i c =
  if i < 0 || i >= length s then invalid_arg "String.rcontains_from" else
  try ignore (rindex_rec s i c); true with Not_found -> false;;

type t = string

let compare (x: t) (y: t) = Pervasives.compare x y
