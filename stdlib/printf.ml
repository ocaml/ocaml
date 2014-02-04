(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*  Xavier Leroy and Pierre Weis, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

external format_float: string -> float -> string
  = "caml_format_float"
external format_int: string -> int -> string
  = "caml_format_int"
external format_int32: string -> int32 -> string
  = "caml_int32_format"
external format_nativeint: string -> nativeint -> string
  = "caml_nativeint_format"
external format_int64: string -> int64 -> string
  = "caml_int64_format"

module Sformat = struct

  type index;;

  external unsafe_index_of_int : int -> index = "%identity"
  ;;
  let index_of_int i =
    if i >= 0 then unsafe_index_of_int i
    else failwith ("Sformat.index_of_int: negative argument " ^ string_of_int i)
  ;;
  external int_of_index : index -> int = "%identity"
  ;;

  let add_int_index i idx = index_of_int (i + int_of_index idx);;
  let succ_index = add_int_index 1;;
  (* Literal position are one-based (hence pred p instead of p). *)
  let index_of_literal_position p = index_of_int (pred p);;

  external length : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int
    = "%string_length"
  ;;
  external get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
    = "%string_safe_get"
  ;;
  external unsafe_get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
    = "%string_unsafe_get"
  ;;
  external unsafe_to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
    = "%identity"
  ;;
  let sub fmt idx len =
    String.sub (unsafe_to_string fmt) (int_of_index idx) len
  ;;
  let to_string fmt = sub fmt (unsafe_index_of_int 0) (length fmt)
  ;;

end
;;

let bad_conversion sfmt i c =
  invalid_arg
    ("Printf: bad conversion %" ^ String.make 1 c ^ ", at char number " ^
     string_of_int i ^ " in format string \'" ^ sfmt ^ "\'")
;;

let bad_conversion_format fmt i c =
  bad_conversion (Sformat.to_string fmt) i c
;;

let incomplete_format fmt =
  invalid_arg
    ("Printf: premature end of format string \'" ^
     Sformat.to_string fmt ^ "\'")
;;

(* Parses a string conversion to return the specified length and the
   padding direction. *)
let parse_string_conversion sfmt =
  let rec parse neg i =
    if i >= String.length sfmt then (0, neg) else
    match String.unsafe_get sfmt i with
    | '1'..'9' ->
      (int_of_string
         (String.sub sfmt i (String.length sfmt - i - 1)),
       neg)
    | '-' ->
      parse true (succ i)
    | _ ->
      parse neg (succ i) in
  try parse false 1 with
  | Failure _ -> bad_conversion sfmt 0 's'
;;

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
;;

(* Format a string given a %s format, e.g. %40s or %-20s.
   To do ?: ignore other flags (#, +, etc). *)
let format_string sfmt s =
  let (p, neg) = parse_string_conversion sfmt in
  pad_string ' ' p neg s 0 (String.length s)
;;

(* Extract a format string out of [fmt] between [start] and [stop] inclusive.
   ['*'] in the format are replaced by integers taken from the [widths] list.
   [extract_format] returns a string which is the string representation of
   the resulting format string. *)
let extract_format fmt start stop widths =
  let skip_positional_spec start =
    match Sformat.unsafe_get fmt start with
    | '0'..'9' ->
      let rec skip_int_literal i =
        match Sformat.unsafe_get fmt i with
        | '0'..'9' -> skip_int_literal (succ i)
        | '$' -> succ i
        | _ -> start in
      skip_int_literal (succ start)
    | _ -> start in
  let start = skip_positional_spec (succ start) in
  let b = Buffer.create (stop - start + 10) in
  Buffer.add_char b '%';
  let rec fill_format i widths =
    if i <= stop then
      match (Sformat.unsafe_get fmt i, widths) with
      | ('*', h :: t) ->
        Buffer.add_string b (string_of_int h);
        let i = skip_positional_spec (succ i) in
        fill_format i t
      | ('*', []) ->
        assert false (* Should not happen since this is ill-typed. *)
      | (c, _) ->
        Buffer.add_char b c;
        fill_format (succ i) widths in
  fill_format start (List.rev widths);
  Buffer.contents b
;;

let extract_format_int conv fmt start stop widths =
  let sfmt = extract_format fmt start stop widths in
  match conv with
  | 'n' | 'N' ->
    sfmt.[String.length sfmt - 1] <- 'u';
    sfmt
  | _ -> sfmt
;;

let extract_format_float conv fmt start stop widths =
  let sfmt = extract_format fmt start stop widths in
  match conv with
  | 'F' ->
    sfmt.[String.length sfmt - 1] <- 'g';
    sfmt
  | _ -> sfmt
;;

(* Returns the position of the next character following the meta format
   string, starting from position [i], inside a given format [fmt].
   According to the character [conv], the meta format string is
   enclosed by the delimiters %{ and %} (when [conv = '{']) or %( and
   %) (when [conv = '(']). Hence, [sub_format] returns the index of
   the character following the [')'] or ['}'] that ends the meta format,
   according to the character [conv]. *)
let sub_format incomplete_format bad_conversion_format conv fmt i =
  let len = Sformat.length fmt in
  let rec sub_fmt c i =
    let close = if c = '(' then ')' else (* '{' *) '}' in
    let rec sub j =
       if j >= len then incomplete_format fmt else
       match Sformat.get fmt j with
       | '%' -> sub_sub (succ j)
       | _ -> sub (succ j)
    and sub_sub j =
       if j >= len then incomplete_format fmt else
       match Sformat.get fmt j with
       | '(' | '{' as c ->
         let j = sub_fmt c (succ j) in
         sub (succ j)
       | '}' | ')' as c ->
         if c = close then succ j else bad_conversion_format fmt i c
       | _ -> sub (succ j) in
    sub i in
  sub_fmt conv i
;;

let sub_format_for_printf conv =
  sub_format incomplete_format bad_conversion_format conv
;;

let iter_on_format_args fmt add_conv add_char =

  let lim = Sformat.length fmt - 1 in

  let rec scan_flags skip i =
    if i > lim then incomplete_format fmt else
    match Sformat.unsafe_get fmt i with
    | '*' -> scan_flags skip (add_conv skip i 'i')
 (* | '$' -> scan_flags skip (succ i) *** PR#4321 *)
    | '#' | '-' | ' ' | '+' -> scan_flags skip (succ i)
    | '_' -> scan_flags true (succ i)
    | '0'..'9'
    | '.' -> scan_flags skip (succ i)
    | _ -> scan_conv skip i
  and scan_conv skip i =
    if i > lim then incomplete_format fmt else
    match Sformat.unsafe_get fmt i with
    | '%' | '@' | '!' | ',' -> succ i
    | 's' | 'S' | '[' -> add_conv skip i 's'
    | 'c' | 'C' -> add_conv skip i 'c'
    | 'd' | 'i' |'o' | 'u' | 'x' | 'X' | 'N' -> add_conv skip i 'i'
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> add_conv skip i 'f'
    | 'B' | 'b' -> add_conv skip i 'B'
    | 'a' | 'r' | 't' as conv -> add_conv skip i conv
    | 'l' | 'n' | 'L' as conv ->
      let j = succ i in
      if j > lim then add_conv skip i 'i' else begin
        match Sformat.get fmt j with
        | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' ->
          add_char (add_conv skip i conv) 'i'
        | _ -> add_conv skip i 'i' end
    | '{' as conv ->
      (* Just get a regular argument, skipping the specification. *)
      let i = add_conv skip i conv in
      (* To go on, find the index of the next char after the meta format. *)
      let j = sub_format_for_printf conv fmt i in
      (* Add the meta specification to the summary anyway. *)
      let rec loop i =
        if i < j - 2 then loop (add_char i (Sformat.get fmt i)) in
      loop i;
      (* Go on, starting at the closing brace to properly close the meta
         specification in the summary. *)
      scan_conv skip (j - 1)
    | '(' as conv ->
      (* Use the static format argument specification instead of
         the runtime format argument value: they must have the same type
         anyway. *)
      scan_fmt (add_conv skip i conv)
    | '}' | ')' as conv -> add_conv skip i conv
    | conv -> bad_conversion_format fmt i conv

  and scan_fmt i =
    if i < lim then
     if Sformat.get fmt i = '%'
     then scan_fmt (scan_flags false (succ i))
     else scan_fmt (succ i)
    else i in

  ignore (scan_fmt 0)
;;

(* Returns a string that summarizes the typing information that a given
   format string contains.
   For instance, [summarize_format_type "A number %d\n"] is "%i".
   It also checks the well-formedness of the format string. *)
let summarize_format_type fmt =
  let len = Sformat.length fmt in
  let b = Buffer.create len in
  let add_char i c = Buffer.add_char b c; succ i in
  let add_conv skip i c =
    if skip then Buffer.add_string b "%_" else Buffer.add_char b '%';
    add_char i c in
  iter_on_format_args fmt add_conv add_char;
  Buffer.contents b
;;

module Ac = struct
  type ac = {
    mutable ac_rglr : int;
    mutable ac_skip : int;
    mutable ac_rdrs : int;
  }
end
;;

open Ac;;

(* Computes the number of arguments of a format (including the flag
   arguments if any). *)
let ac_of_format fmt =
  let ac = { ac_rglr = 0; ac_skip = 0; ac_rdrs = 0; } in
  let incr_ac skip c =
    let inc = if c = 'a' then 2 else 1 in
    if c = 'r' then ac.ac_rdrs <- ac.ac_rdrs + 1;
    if skip
    then ac.ac_skip <- ac.ac_skip + inc
    else ac.ac_rglr <- ac.ac_rglr + inc in
  let add_conv skip i c =
    (* Just finishing a meta format: no additional argument to record. *)
    if c <> ')' && c <> '}' then incr_ac skip c;
    succ i
  and add_char i _ = succ i in

  iter_on_format_args fmt add_conv add_char;
  ac
;;

let count_printing_arguments_of_format fmt =
  let ac = ac_of_format fmt in
  (* For printing, only the regular arguments have to be counted. *)
  ac.ac_rglr
;;

let list_iter_i f l =
  let rec loop i = function
  | [] -> ()
  | [x] -> f i x (* Tail calling [f] *)
  | x :: xs -> f i x; loop (succ i) xs in
  loop 0 l
;;

(* 'Abstracting' version of kprintf: returns a (curried) function that
   will print when totally applied.
   Note: in the following, we are careful not to be badly caught
   by the compiler optimizations for the representation of arrays. *)
let kapr kpr fmt =
  match count_printing_arguments_of_format fmt with
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
    loop 0 []
;;

type positional_specification =
   | Spec_none | Spec_index of Sformat.index
;;

(* To scan an optional positional parameter specification,
   i.e. an integer followed by a [$].

   Calling [got_spec] with appropriate arguments, we 'return' a positional
   specification and an index to go on scanning the [fmt] format at hand.

   Note that this is optimized for the regular case, i.e. no positional
   parameter, since in this case we juste 'return' the constant
   [Spec_none]; in case we have a positional parameter, we 'return' a
   [Spec_index] [positional_specification] which is a bit more costly.

   Note also that we do not support [*$] specifications, since this would
   lead to type checking problems: a [*$] positional specification means
   'take the next argument to [printf] (which must be an integer value)',
   name this integer value $n$; [*$] now designates parameter $n$.

   Unfortunately, the type of a parameter specified via a [*$] positional
   specification should be the type of the corresponding argument to
   [printf], hence this should be the type of the $n$-th argument to [printf]
   with $n$ being the {\em value} of the integer argument defining [*]; we
   clearly cannot statically guess the value of this parameter in the general
   case. Put it another way: this means type dependency, which is completely
   out of scope of the OCaml type algebra. *)

let scan_positional_spec fmt got_spec i =
  match Sformat.unsafe_get fmt i with
  | '0'..'9' as d ->
    let rec get_int_literal accu j =
      match Sformat.unsafe_get fmt j with
      | '0'..'9' as d ->
        get_int_literal (10 * accu + (int_of_char d - 48)) (succ j)
      | '$' ->
        if accu = 0 then
          failwith "printf: bad positional specification (0)." else
        got_spec (Spec_index (Sformat.index_of_literal_position accu)) (succ j)
      (* Not a positional specification: tell so the caller, and go back to
         scanning the format from the original [i] position we were called at
         first. *)
      | _ -> got_spec Spec_none i in
    get_int_literal (int_of_char d - 48) (succ i)
  (* No positional specification: tell so the caller, and go back to scanning
     the format from the original [i] position. *)
  | _ -> got_spec Spec_none i
;;

(* Get the index of the next argument to printf, according to the given
   positional specification. *)
let next_index spec n =
  match spec with
  | Spec_none -> Sformat.succ_index n
  | Spec_index _ -> n
;;

(* Get the index of the actual argument to printf, according to its
   optional positional specification. *)
let get_index spec n =
  match spec with
  | Spec_none -> n
  | Spec_index p -> p
;;

(* Format a float argument as a valid OCaml lexeme. *)
let format_float_lexeme =

  (* To be revised: this procedure should be a unique loop that performs the
     validity check and the string lexeme modification at the same time.
     Otherwise, it is too difficult to handle the strange padding facilities
     given by printf. Let alone handling the correct widths indication,
     knowing that we have sometime to add a '.' at the end of the result!
  *)

  let make_valid_float_lexeme s =
    (* Check if s is already a valid lexeme:
       in this case do nothing,
       otherwise turn s into a valid OCaml lexeme. *)
    let l = String.length s in
    let rec valid_float_loop i =
      if i >= l then s ^ "." else
      match s.[i] with
      (* Sure, this is already a valid float lexeme. *)
      | '.' | 'e' | 'E' -> s
      | _ -> valid_float_loop (i + 1) in

    valid_float_loop 0 in

  (fun sfmt x ->
   match classify_float x with
   | FP_normal | FP_subnormal | FP_zero ->
       make_valid_float_lexeme (format_float sfmt x)
   | FP_infinite ->
       if x < 0.0 then "neg_infinity" else "infinity"
   | FP_nan ->
       "nan")
;;

(* Decode a format string and act on it.
   [fmt] is the [printf] format string, and [pos] points to a [%] character in
   the format string.
   After consuming the appropriate number of arguments and formatting
   them, one of the following five continuations described below is called:

   - [cont_s] for outputting a string
     (arguments: arg num, string, next pos)
   - [cont_a] for performing a %a action
     (arguments: arg num, fn, arg, next pos)
   - [cont_t] for performing a %t action
     (arguments: arg num, fn, next pos)
   - [cont_f] for performing a flush action
     (arguments: arg num, next pos)
   - [cont_m] for performing a %( action
     (arguments: arg num, sfmt, next pos)

   "arg num" is the index in array [args] of the next argument to [printf].
   "next pos" is the position in [fmt] of the first character following
   the %conversion specification in [fmt]. *)

(* Note: here, rather than test explicitly against [Sformat.length fmt]
   to detect the end of the format, we use [Sformat.unsafe_get] and
   rely on the fact that we'll get a "null" character if we access
   one past the end of the string.  These "null" characters are then
   caught by the [_ -> bad_conversion] clauses below.
   Don't do this at home, kids. *)
let scan_format fmt args n pos cont_s cont_a cont_t cont_f cont_m =

  let get_arg spec n =
    Obj.magic (args.(Sformat.int_of_index (get_index spec n))) in

  let rec scan_positional n widths i =
    let got_spec spec i = scan_flags spec n widths i in
    scan_positional_spec fmt got_spec i

  and scan_flags spec n widths i =
    match Sformat.unsafe_get fmt i with
    | '*' ->
      let got_spec wspec i =
        let (width : int) = get_arg wspec n in
        scan_flags spec (next_index wspec n) (width :: widths) i in
      scan_positional_spec fmt got_spec (succ i)
    | '0'..'9'
    | '.' | '#' | '-' | ' ' | '+' -> scan_flags spec n widths (succ i)
    | _ -> scan_conv spec n widths i

  and scan_conv spec n widths i =
    match Sformat.unsafe_get fmt i with
    | '%' | '@' as c ->
      cont_s n (String.make 1 c) (succ i)
    | '!' -> cont_f n (succ i)
    | ',' -> cont_s n "" (succ i)
    | 's' | 'S' as conv ->
      let (x : string) = get_arg spec n in
      let x = if conv = 's' then x else "\"" ^ String.escaped x ^ "\"" in
      let s =
        (* Optimize for common case %s *)
        if i = succ pos then x else
        format_string (extract_format fmt pos i widths) x in
      cont_s (next_index spec n) s (succ i)
    | '[' as conv ->
      bad_conversion_format fmt i conv
    | 'c' | 'C' as conv ->
      let (x : char) = get_arg spec n in
      let s =
        if conv = 'c' then String.make 1 x else "'" ^ Char.escaped x ^ "'" in
      cont_s (next_index spec n) s (succ i)
    | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' | 'N' as conv ->
      let (x : int) = get_arg spec n in
      let s =
        format_int (extract_format_int conv fmt pos i widths) x in
      cont_s (next_index spec n) s (succ i)
    | 'f' | 'e' | 'E' | 'g' | 'G' ->
      let (x : float) = get_arg spec n in
      let s = format_float (extract_format fmt pos i widths) x in
      cont_s (next_index spec n) s (succ i)
    | 'F' as conv ->
      let (x : float) = get_arg spec n in
      let s =
        format_float_lexeme
          (if widths = []
           then "%.12g"
           else extract_format_float conv fmt pos i widths)
          x in
      cont_s (next_index spec n) s (succ i)
    | 'B' | 'b' ->
      let (x : bool) = get_arg spec n in
      cont_s (next_index spec n) (string_of_bool x) (succ i)
    | 'a' ->
      let printer = get_arg spec n in
      (* If the printer spec is Spec_none, go on as usual.
         If the printer spec is Spec_index p,
         printer's argument spec is Spec_index (succ_index p). *)
      let n = Sformat.succ_index (get_index spec n) in
      let arg = get_arg Spec_none n in
      cont_a (next_index spec n) printer arg (succ i)
    | 'r' as conv ->
      bad_conversion_format fmt i conv
    | 't' ->
      let printer = get_arg spec n in
      cont_t (next_index spec n) printer (succ i)
    | 'l' | 'n' | 'L' as conv ->
      begin match Sformat.unsafe_get fmt (succ i) with
      | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' ->
        let i = succ i in
        let s =
          match conv with
          | 'l' ->
            let (x : int32) = get_arg spec n in
            format_int32 (extract_format fmt pos i widths) x
          | 'n' ->
            let (x : nativeint) = get_arg spec n in
            format_nativeint (extract_format fmt pos i widths) x
          | _ ->
            let (x : int64) = get_arg spec n in
            format_int64 (extract_format fmt pos i widths) x in
        cont_s (next_index spec n) s (succ i)
      | _ ->
        let (x : int) = get_arg spec n in
        let s = format_int (extract_format_int 'n' fmt pos i widths) x in
        cont_s (next_index spec n) s (succ i)
      end
    | '{' | '(' as conv (* ')' '}' *) ->
      let (xf : ('a, 'b, 'c, 'd, 'e, 'f) format6) = get_arg spec n in
      let i = succ i in
      let i = sub_format_for_printf conv fmt i in
      if conv = '{' (* '}' *) then
        (* Just print the format argument as a specification. *)
        cont_s
          (next_index spec n)
          (summarize_format_type xf)
          i else
        (* Use the format argument instead of the format specification. *)
        cont_m (next_index spec n) xf i
    | (* '(' *) ')' ->
      cont_s n "" (succ i)
    | conv ->
      bad_conversion_format fmt i conv in

  scan_positional n [] (succ pos)
;;

let mkprintf to_s get_out outc outs flush k fmt =

  (* [out] is global to this definition of [pr], and must be shared by all its
     recursive calls (if any). *)
  let out = get_out fmt in
  let outc c = outc out c in
  let outs s = outs out s in

  let rec pr k n fmt v =

    let len = Sformat.length fmt in

    let rec doprn n i =
       if i >= len then Obj.magic (k out) else
       match Sformat.unsafe_get fmt i with
       | '%' -> scan_format fmt v n i cont_s cont_a cont_t cont_f cont_m
       |  c  -> outc c; doprn n (succ i)

    and cont_s n s i =
      outs s; doprn n i
    and cont_a n printer arg i =
      if to_s then
        outs ((Obj.magic printer : unit -> _ -> string) () arg)
      else
        printer out arg;
      doprn n i
    and cont_t n printer i =
      if to_s then
        outs ((Obj.magic printer : unit -> string) ())
      else
        printer out;
      doprn n i
    and cont_f n i =
      flush out; doprn n i
    and cont_m n xf i =
      let m =
        Sformat.add_int_index
          (count_printing_arguments_of_format xf) n in
      pr (Obj.magic (fun _ -> doprn m i)) n xf v in

    doprn n 0 in

  let kpr = pr k (Sformat.index_of_int 0) in

  kapr kpr fmt
;;

(**************************************************************

  Defining [fprintf] and various flavors of [fprintf].

 **************************************************************)

let kfprintf k oc =
  mkprintf false (fun _ -> oc) output_char output_string flush k
;;
let ikfprintf k oc = kapr (fun _ _ -> Obj.magic (k oc));;

let fprintf oc = kfprintf ignore oc;;
let ifprintf oc = ikfprintf ignore oc;;
let printf fmt = fprintf stdout fmt;;
let eprintf fmt = fprintf stderr fmt;;

let kbprintf k b =
  mkprintf false (fun _ -> b) Buffer.add_char Buffer.add_string ignore k
;;
let bprintf b = kbprintf ignore b;;

let get_buff fmt =
  let len = 2 * Sformat.length fmt in
  Buffer.create len
;;

let get_contents b =
  let s = Buffer.contents b in
  Buffer.clear b;
  s
;;

let get_cont k b = k (get_contents b);;

let ksprintf k =
  mkprintf true get_buff Buffer.add_char Buffer.add_string ignore (get_cont k)
;;

let sprintf fmt = ksprintf (fun s -> s) fmt;;

(**************************************************************

  Deprecated stuff.

 **************************************************************)

let kprintf = ksprintf;;

(* For OCaml system internal use only: needed to implement modules [Format]
  and [Scanf]. *)

module CamlinternalPr = struct

  module Sformat = Sformat;;

  module Tformat = struct

    type ac =
      Ac.ac = {
      mutable ac_rglr : int;
      mutable ac_skip : int;
      mutable ac_rdrs : int;
    }
    ;;

    let ac_of_format = ac_of_format;;

    let count_printing_arguments_of_format =
      count_printing_arguments_of_format;;

    let sub_format = sub_format;;

    let summarize_format_type = summarize_format_type;;

    let scan_format = scan_format;;

    let kapr = kapr;;

  end
  ;;

end
;;
