(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*  Xavier Leroy and Pierre Weis, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

external format_int: string -> int -> string = "caml_format_int"
external format_int32: string -> int32 -> string = "caml_int32_format"
external format_nativeint: string -> nativeint -> string
                         = "caml_nativeint_format"
external format_int64: string -> int64 -> string = "caml_int64_format"
external format_float: string -> float -> string = "caml_format_float"

external format_to_string: ('a, 'b, 'c, 'd) format4 -> string = "%identity"

type index;;

external index_of_int : int -> index = "%identity";;
external int_of_index : index -> int = "%identity";;

let add_int_index i idx = index_of_int (i + int_of_index idx);;
let succ_index = add_int_index 1;;
(* Litteral position are one-based (hence pred p instead of p). *)
let index_of_litteral_position p = index_of_int (pred p);;

let bad_conversion fmt i c =
  invalid_arg
    ("printf: bad conversion %" ^ String.make 1 c ^ ", at char number " ^
     string_of_int i ^ " in format string ``" ^ fmt ^ "''");;

let incomplete_format fmt =
  invalid_arg
    ("printf: premature end of format string ``" ^ fmt ^ "''");;

(* Parses a format to return the specified length and the padding direction. *)
let parse_format fmt =
  let rec parse neg i =
    if i >= String.length fmt then (0, neg) else
    match String.unsafe_get fmt i with
    | '1'..'9' ->
        (int_of_string (String.sub fmt i (String.length fmt - i - 1)),
         neg)
    | '-' ->
        parse true (succ i)
    | _ ->
        parse neg (succ i) in
  try parse false 1 with Failure _ -> bad_conversion fmt 0 's'

(* Pad a (sub) string into a blank string of length [p],
   on the right if [neg] is true, on the left otherwise. *)
let pad_string pad_char p neg s i len =
  if p = len && i = 0 then s else
  if p <= len then String.sub s i len else
  let res = String.make p pad_char in
  if neg
  then String.blit s i res 0 len
  else String.blit s i res (p - len) len;
  res

(* Format a string given a %s format, e.g. %40s or %-20s.
   To do: ignore other flags (#, +, etc)? *)
let format_string fmt s =
  let (p, neg) = parse_format fmt in
  pad_string ' ' p neg s 0 (String.length s)

(* Extract a %format from [fmt] between [start] and [stop] inclusive.
   '*' in the format are replaced by integers taken from the [widths] list. *)
let extract_format fmt start stop widths =
  let skip_positional_spec start =
    match String.unsafe_get fmt start with
    | '0'..'9' ->
      let rec skip_int_litteral i =
        match String.unsafe_get fmt i with
        | '0'..'9' -> skip_int_litteral (succ i)
        | '$' -> succ i
        | _ -> start in
      skip_int_litteral (succ start)
    | _ -> start in
  let start = skip_positional_spec (succ start) in
  let b = Buffer.create (stop - start + 10) in
  Buffer.add_char b '%';
  let rec fill_format i widths =
    if i <= stop then
      match (String.unsafe_get fmt i, widths) with
      | ('*', h :: t) ->
        Buffer.add_string b (string_of_int h);
        let i = skip_positional_spec (succ i) in
        fill_format i t
      | ('*', []) ->
        assert false (* should not happen *)
      | (c, _) ->
        Buffer.add_char b c; fill_format (succ i) widths in
  fill_format start (List.rev widths);
  Buffer.contents b;;

let format_int_with_conv conv fmt i =
   match conv with
   | 'n' | 'N' -> fmt.[String.length fmt - 1] <- 'u'; format_int fmt i
   | _ -> format_int fmt i

(* Returns the position of the last character of the meta format
   string, starting from position [i], inside a given format [fmt].
   According to the character [conv], the meta format string is
   enclosed by the delimitors %{ and %} (when [conv = '{']) or %( and
   %) (when [conv = '(']). Hence, [sub_format] returns the index of
   the character ')' or '}' that ends the meta format, according to
   the character [conv]. *)
let sub_format incomplete_format bad_conversion conv fmt i =
  let len = String.length fmt in
  let rec sub_fmt c i =
    let close = if c = '(' then ')' else (* '{' *) '}' in
    let rec sub j =
       if j >= len then incomplete_format fmt else
       match fmt.[j] with
       | '%' -> sub_sub (succ j)
       | _ -> sub (succ j)
    and sub_sub j =
       if j >= len then incomplete_format fmt else
       match fmt.[j] with
       | '(' | '{' as c ->
         let j = sub_fmt c (succ j) in sub (succ j)
       | '}' | ')' as c ->
         if c = close then j else bad_conversion fmt i c
       | _ -> sub (succ j) in
    sub i in
  sub_fmt conv i;;

let sub_format_for_printf = sub_format incomplete_format bad_conversion;;

let iter_on_format_args fmt add_conv add_char =
  let lim = String.length fmt - 1 in

  let rec scan_flags skip i =
    if i > lim then incomplete_format fmt else
    match String.unsafe_get fmt i with
    | '*' -> scan_flags skip (add_conv skip i 'i')
    | '$' -> scan_flags skip (succ i)
    | '#' | '-' | ' ' | '+' -> scan_flags skip (succ i)
    | '_' -> scan_flags true (succ i)
    | '0'..'9'
    | '.' -> scan_flags skip (succ i)
    | _ -> scan_conv skip i
  and scan_conv skip i =
    if i > lim then incomplete_format fmt else
    match String.unsafe_get fmt i with
    | '%' | '!' -> succ i
    | 's' | 'S' | '[' -> add_conv skip i 's'
    | 'c' | 'C' -> add_conv skip i 'c'
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' -> add_conv skip i 'i'
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> add_conv skip i 'f'
    | 'B' | 'b' -> add_conv skip i 'B'
    | 'a' | 't' as conv -> add_conv skip i conv
    | 'l' | 'n' | 'L' as conv ->
        let j = succ i in
        if j > lim then add_conv skip i 'i' else begin
          match fmt.[j] with
          | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            add_char (add_conv skip i conv) 'i'
          | c -> add_conv skip i 'i' end
    | '{' as conv ->
      (* Just get a regular argument, skipping the specification. *)
      let i = add_conv skip i conv in
      let j = sub_format_for_printf conv fmt i in
      (* Add the meta specification anyway. *)
      let rec loop i =
        if i < j - 1 then loop (add_char i fmt.[i]) in
      loop i;
      scan_conv skip j
    | '(' as conv ->
      (* Use the static format argument specification instead of
         the runtime format argument value: they must have the same type
         anyway. *)
      scan_fmt (add_conv skip i conv)
    | '}' | ')' as conv -> add_conv skip i conv
    | conv -> bad_conversion fmt i conv

  and scan_fmt i =
    if i < lim then
     if fmt.[i] = '%'
     then scan_fmt (scan_flags false (succ i))
     else scan_fmt (succ i)
    else i in

  ignore (scan_fmt 0);;

(* Returns a string that summarizes the typing information that a given
   format string contains.
   It also checks the well-formedness of the format string.
   For instance, [summarize_format_type "A number %d\n"] is "%i". *)
let summarize_format_type fmt =
  let len = String.length fmt in
  let b = Buffer.create len in
  let add_char i c = Buffer.add_char b c; succ i in
  let add_conv skip i c =
    if skip then Buffer.add_string b "%_" else Buffer.add_char b '%';
    add_char i c in
  iter_on_format_args fmt add_conv add_char;
  Buffer.contents b;;

(* Computes the number of arguments of a format (including flag
   arguments if any). *)
let nargs_of_format_type fmt =
  let num_args = ref 0
  and skip_args = ref 0 in
  let add_conv skip i c =
    (* Just finishing a meta format: no additional argument to record. *)
    if c = ')' || c = '}' then succ i else
    let incr_args n = if c = 'a' then n := !n + 2 else n := !n + 1 in
    if skip then incr_args skip_args else incr_args num_args;
    succ i
  and add_char i c = succ i in
  iter_on_format_args fmt add_conv add_char;
  !skip_args + !num_args;;

let list_iter_i f l =
  let rec loop i = function
  | [] -> ()
  | x :: xs -> f i x; loop (succ i) xs in
  loop 0 l;;

(* ``Abstracting'' version of kprintf: returns a (curried) function that
   will print when totally applied.
   Note: in the following, we are careful not to be badly caught
   by the compiler optimizations on the representation of arrays. *)
let kapr kpr fmt =
  match nargs_of_format_type fmt with
  | 0 -> kpr fmt [||]
  | 1 -> Obj.magic (fun x ->
      let a = Array.make 1 (Obj.repr 0) in
      a.(0) <- x;
      kpr fmt a)
  | 2 -> Obj.magic (fun x y ->
      let a = Array.make 2 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y;
      kpr fmt a)
  | 3 -> Obj.magic (fun x y z ->
      let a = Array.make 3 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y; a.(2) <- z;
      kpr fmt a)
  | 4 -> Obj.magic (fun x y z t ->
      let a = Array.make 4 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y; a.(2) <- z;
      a.(3) <- t;
      kpr fmt a)
  | 5 -> Obj.magic (fun x y z t u ->
      let a = Array.make 5 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y; a.(2) <- z;
      a.(3) <- t; a.(4) <- u;
      kpr fmt a)
  | 6 -> Obj.magic (fun x y z t u v ->
      let a = Array.make 6 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y; a.(2) <- z;
      a.(3) <- t; a.(4) <- u; a.(5) <- v;
      kpr fmt a)
  | nargs ->
    let rec loop i args =
      if i >= nargs then
        let a = Array.make nargs (Obj.repr 0) in
        list_iter_i (fun i arg -> a.(nargs - i - 1) <- arg) args;
        kpr fmt a
      else Obj.magic (fun x -> loop (succ i) (x :: args)) in
    loop 0 [];;

type param_spec = Spec_none | Spec_index of index;;

(* To scan an optional positional parameter specification,
   i.e. an integer followed by a $.
   We do not support *$ specifications, since this would lead to type checking
   problems: the type would be dependant of the {\em value} of an integer
   argument to printf. *)
let scan_positional_spec fmt got_pos n i =
  match String.unsafe_get fmt i with
  | '0'..'9' as d ->
    let rec get_int_litteral accu j =
      match String.unsafe_get fmt j with
      | '0'..'9' as d ->
        get_int_litteral (10 * accu + (int_of_char d - 48)) (succ j)
      | '$' ->
        if accu = 0
          then failwith "printf: bad positional specification (0)." else
        got_pos (Spec_index (index_of_litteral_position accu)) (succ j)
      (* Not a positional specification. *)
      | _ -> got_pos Spec_none i in
    get_int_litteral (int_of_char d - 48) (succ i)
  (* No positional specification. *)
  | _ -> got_pos Spec_none i;;

(* Get the position of the next argument to printf, according to the given
   positional specification. *)
let next_index spec n =
  match spec with
  | Spec_none -> succ_index n
  | Spec_index p -> n;;

(* Get the position of the actual argument to printf, according to its
   optional positional specification. *)
let get_index spec n =
  match spec with
  | Spec_none -> n
  | Spec_index p -> p;;

(* Decode a %format and act on it.
   [fmt] is the printf format string, and [pos] points to a [%] character.
   After consuming the appropriate number of arguments and formatting
   them, one of the five continuations is called:
   [cont_s] for outputting a string (args: string, next pos)
   [cont_a] for performing a %a action (args: fn, arg, next pos)
   [cont_t] for performing a %t action (args: fn, next pos)
   [cont_f] for performing a flush action
   [cont_m] for performing a %( action (args: sfmt, next pos)
   "next pos" is the position in [fmt] of the first character following
   the %format in [fmt]. *)

(* Note: here, rather than test explicitly against [String.length fmt]
   to detect the end of the format, we use [String.unsafe_get] and
   rely on the fact that we'll get a "nul" character if we access
   one past the end of the string.  These "nul" characters are then
   caught by the [_ -> bad_conversion] clauses below.
   Don't do this at home, kids. *)
let scan_format fmt args n pos cont_s cont_a cont_t cont_f cont_m =

  let get_arg spec n = Obj.magic args.(int_of_index (get_index spec n)) in

  let rec scan_positional n widths i =
    let got_pos spec i = scan_flags spec n widths i in
    scan_positional_spec fmt got_pos n i

  and scan_flags spec n widths i =
    match String.unsafe_get fmt i with
    | '*' ->
      let got_pos wspec i =
        let (width : int) = get_arg wspec n in
        scan_flags spec (next_index wspec n) (width :: widths) i in
      scan_positional_spec fmt got_pos n (succ i)
    | '0'..'9'
    | '.' | '#' | '-' | ' ' | '+' -> scan_flags spec n widths (succ i)
    | _ -> scan_conv spec n widths i

  and scan_conv spec n widths i =
    match String.unsafe_get fmt i with
    | '%' ->
      cont_s n "%" (succ i)
    | 's' | 'S' as conv ->
      let (x : string) = get_arg spec n in
      let x = if conv = 's' then x else "\"" ^ String.escaped x ^ "\"" in
      let s =
        (* optimize for common case %s *)
        if i = succ pos then x else
        format_string (extract_format fmt pos i widths) x in
      cont_s (next_index spec n) s (succ i)
    | 'c' | 'C' as conv ->
      let (x : char) = get_arg spec n in
      let s =
        if conv = 'c' then String.make 1 x else "'" ^ Char.escaped x ^ "'" in
      cont_s (next_index spec n) s (succ i)
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' as conv ->
      let (x : int) = get_arg spec n in
      let s = format_int_with_conv conv (extract_format fmt pos i widths) x in
      cont_s (next_index spec n) s (succ i)
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' as conv ->
      let (x : float) = get_arg spec n in
      let s =
        if conv = 'F' then string_of_float x else
        format_float (extract_format fmt pos i widths) x in
      cont_s (next_index spec n) s (succ i)
    | 'B' | 'b' ->
      let (x : bool) = get_arg spec n in
      cont_s (next_index spec n) (string_of_bool x) (succ i)
    | 'a' ->
      let printer = get_arg spec n in
      (* If the printer spec is Spec_none, go on as usual.
         If the printer spec is Spec_index p,
         printer's argument spec is Spec_index (succ_index p). *)
      let n = succ_index (get_index spec n) in
      let arg = get_arg Spec_none n in
      cont_a (next_index spec n) printer arg (succ i)
    | 't' ->
      let printer = get_arg spec n in
      cont_t (next_index spec n) printer (succ i)
    | 'l' | 'n' | 'L' as conv ->
      begin match String.unsafe_get fmt (succ i) with
      | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
        let s =
          match conv with
          | 'l' ->
            let (x : int32) = get_arg spec n in
            format_int32 (extract_format fmt pos (succ i) widths) x
          | 'n' ->
            let (x : nativeint) = get_arg spec n in
            format_nativeint (extract_format fmt pos (succ i) widths) x
          | _ ->
            let (x : int64) = get_arg spec n in
            format_int64 (extract_format fmt pos (succ i) widths) x in
        cont_s (next_index spec n) s (i + 2)
      | _ ->
        let (x : int) = get_arg spec n in
        cont_s
          (next_index spec n)
          (format_int_with_conv 'n' (extract_format fmt pos i widths) x)
          (succ i)
      end
    | '!' -> cont_f n (succ i)
    | '{' | '(' as conv (* ')' '}' *) ->
      let (xf : ('a, 'b, 'c, 'd) format4) = get_arg spec n in
      let i = succ i in
      let j = sub_format_for_printf conv fmt i + 1 in
      if conv = '{' (* '}' *) then
        (* Just print the format argument as a specification. *)
        cont_s
          (next_index spec n)
          (summarize_format_type (format_to_string xf))
          j else
        (* Use the format argument instead of the format specification. *)
        cont_m (next_index spec n) xf j
    | (* '(' *) ')' ->
      cont_s n "" (succ i)
    | conv ->
      bad_conversion fmt i conv in

  scan_positional n [] (succ pos);;

let mkprintf str get_out outc outs flush k fmt =

  let fmt = format_to_string fmt in
  (* out is global to this invocation of pr, and must be shared by all its
     recursive calls (fif) any. *)
  let out = get_out fmt in

  let rec pr k n fmt v =

    let len = String.length fmt in

    let rec doprn n i =
       if i >= len then Obj.magic (k out) else
       match String.unsafe_get fmt i with
       | '%' -> scan_format fmt v n i cont_s cont_a cont_t cont_f cont_m
       |  c  -> outc out c; doprn n (succ i)
     and cont_s n s i =
       outs out s; doprn n i
     and cont_a n printer arg i =
       if str then
         outs out ((Obj.magic printer : unit -> _ -> string) () arg)
       else
         printer out arg;
       doprn n i
     and cont_t n printer i =
       if str then
         outs out ((Obj.magic printer : unit -> string) ())
       else
         printer out;
       doprn n i
     and cont_f n i =
       flush out; doprn n i
     and cont_m n xf i =
       let m = add_int_index (nargs_of_format_type (format_to_string xf)) n in
       pr (Obj.magic (fun _ -> doprn m i)) n (format_to_string xf) v in

     doprn n 0 in

  let kpr = pr k (index_of_int 0) in

  kapr kpr fmt;;

let kfprintf k oc =
  mkprintf false (fun _ -> oc) output_char output_string flush k
let fprintf oc = kfprintf ignore oc
let printf fmt = fprintf stdout fmt
let eprintf fmt = fprintf stderr fmt

let kbprintf k b =
  mkprintf false (fun _ -> b) Buffer.add_char Buffer.add_string ignore k
let bprintf b = kbprintf ignore b

let get_buff fmt =
  let len = 2 * String.length fmt in
  Buffer.create len;;

let get_contents b =
  let s = Buffer.contents b in
  Buffer.clear b;
  s;;

let get_cont k b = k (get_contents b);;

let ksprintf k =
  mkprintf true get_buff Buffer.add_char Buffer.add_string ignore (get_cont k);;

let kprintf = ksprintf;;

let sprintf fmt = ksprintf (fun s -> s) fmt;;
