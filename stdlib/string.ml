(* String operations *)

external length : string -> int = "ml_string_length"
external create: int -> string = "create_string"
external unsafe_get : string -> int -> char = "%string_get"
external unsafe_set : string -> int -> char -> unit = "%string_set"
external unsafe_blit : string -> int -> string -> int -> int -> unit
                = "blit_string"
external unsafe_fill : string -> int -> int -> char -> unit = "fill_string"

let get s n =
  if n < 0 or n >= length s
  then invalid_arg "String.get"
  else unsafe_get s n

let set s n c =
  if n < 0 or n >= length s
  then invalid_arg "String.set"
  else unsafe_set s n c

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
  if ofs < 0 or len < 0 or ofs + len > length s
  then invalid_arg "String.sub"
  else begin
    let r = create len in
    unsafe_blit s ofs r 0 len;
    r
  end

let fill s ofs len c =
  if ofs < 0 or len < 0 or ofs + len > length s
  then invalid_arg "String.fill"
  else unsafe_fill s ofs len c

let blit s1 ofs1 s2 ofs2 len =
  if len < 0 or ofs1 < 0 or ofs1 + len > length s1
             or ofs2 < 0 or ofs2 + len > length s2
  then invalid_arg "String.blit"
  else unsafe_blit s1 ofs1 s2 ofs2 len

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

external is_printable: char -> bool = "is_printable"

let escaped s =
  let n = ref 0 in
    for i = 0 to length s - 1 do
      n := !n +
        (match unsafe_get s i with
           '"' | '\\' | '\n' | '\t' -> 2
          | c -> if is_printable c then 1 else 4)
    done;
    if !n = length s then s else begin
      let s' = create !n in
        n := 0;
        for i = 0 to length s - 1 do
          begin
            match unsafe_get s i with
              ('"' | '\\') as c ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
            | '\n' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
            | '\t' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
            | c ->
                if is_printable c then
                  unsafe_set s' !n c
                else begin
                  let a = Char.code c in
                  unsafe_set s' !n '\\';
                  incr n;
                  unsafe_set s' !n (Char.unsafe_chr (48 + a / 100));
                  incr n;
                  unsafe_set s' !n (Char.unsafe_chr (48 + (a / 10) mod 10));
                  incr n;
                  unsafe_set s' !n (Char.unsafe_chr (48 + a mod 10))
                end
          end;
          incr n
        done;
        s'
      end
