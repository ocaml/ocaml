(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Miscellaneous useful types and functions *)

val fatal_error: string -> 'a
val fatal_errorf: ('a, Format.formatter, unit, 'b) format4 -> 'a
exception Fatal_error

val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a;;

val map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
        (* [map_end f l t] is [map f l @ t], just more efficient. *)
val map_left_right: ('a -> 'b) -> 'a list -> 'b list
        (* Like [List.map], with guaranteed left-to-right evaluation order *)
val for_all2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        (* Same as [List.for_all] but for a binary predicate.
           In addition, this [for_all2] never fails: given two lists
           with different lengths, it returns false. *)
val replicate_list: 'a -> int -> 'a list
        (* [replicate_list elem n] is the list with [n] elements
           all identical to [elem]. *)
val list_remove: 'a -> 'a list -> 'a list
        (* [list_remove x l] returns a copy of [l] with the first
           element equal to [x] removed. *)
val split_last: 'a list -> 'a list * 'a
        (* Return the last element and the other elements of the given list. *)
val samelist: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
        (* Like [List.for_all2] but returns [false] if the two
           lists have different length. *)
val sameoption: ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
val map2_head: ('a -> 'b -> 'c) -> 'a list -> 'b list -> ('c list * 'b list)
        (* [let (r1,r2) = map2_head f l1 l2]
           If [l1] is of length n and [l2 = h2 @ t2] with h2 of length n
           and t2 of length k, r1 is [List.map2 f l1 h1] and r2 is t2 *)
val some_if_all_elements_are_some: 'a option list -> 'a list option
val split_at: int -> 'a list -> 'a list * 'a list
        (* [split_at n l] returns the couple [before, after]
           where [before] are the [n] first elements of [l] and [after]
           are the remaining ones.
           If [l] has less than [n] elements, raises Invalid_argument *)
val uniq_sort : ('a -> 'a -> int) -> 'a list -> 'a list
        (* Sorts and remove duplicated elements according to the
           comparison function *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
        (* [filter_map f l] is the list [List.map f l] with only the
           elements matching [Some _] *)
val compare_lists : ('a -> 'a -> int) -> 'a list -> 'a list -> int
        (* compare_lists is the lexicographic order supported by the
           provided order *)
val may: ('a -> unit) -> 'a option -> unit
val may_map: ('a -> 'b) -> 'a option -> 'b option
val may_fold: ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
val may_default: ('a -> 'b) -> 'a option -> 'b -> 'b

val find_in_path: string list -> string -> string
        (* Search a file in a list of directories. *)
val find_in_path_rel: string list -> string -> string
        (* Search a relative file in a list of directories. *)
val find_in_path_uncap: string list -> string -> string
        (* Same, but search also for uncapitalized name, i.e.
           if name is Foo.ml, allow /path/Foo.ml and /path/foo.ml
           to match. *)
val remove_file: string -> unit
        (* Delete the given file if it exists. Never raise an error. *)
val expand_directory: string -> string -> string
        (* [expand_directory alt file] eventually expands a [+] at the
           beginning of file into [alt] (an alternate root directory) *)

val create_hashtable: int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
        (* Create a hashtable of the given size and fills it with the
           given bindings. *)

val copy_file: in_channel -> out_channel -> unit
        (* [copy_file ic oc] reads the contents of file [ic] and copies
           them to [oc]. It stops when encountering EOF on [ic]. *)
val copy_file_chunk: in_channel -> out_channel -> int -> unit
        (* [copy_file_chunk ic oc n] reads [n] bytes from [ic] and copies
           them to [oc]. It raises [End_of_file] when encountering
           EOF on [ic]. *)
val string_of_file: in_channel -> string
        (* [string_of_file ic] reads the contents of file [ic] and copies
           them to a string. It stops when encountering EOF on [ic]. *)
val log2: int -> int
        (* [log2 n] returns [s] such that [n = 1 lsl s]
           if [n] is a power of 2*)
val align: int -> int -> int
        (* [align n a] rounds [n] upwards to a multiple of [a]
           (a power of 2). *)
val no_overflow_add: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 + n2] does not overflow. *)
val no_overflow_sub: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)
val no_overflow_mul: int -> int -> bool
        (* [no_overflow_mul n1 n2] returns [true] if the computation of
           [n1 * n2] does not overflow. *)
val no_overflow_lsl: int -> int -> bool
        (* [no_overflow_lsl n k] returns [true] if the computation of
           [n lsl k] does not overflow. *)

val chop_extension_if_any: string -> string
        (* Like Filename.chop_extension but returns the initial file
           name if it has no extension *)

val chop_extensions: string -> string
        (* Return the given file name without its extensions. The extensions
           is the longest suffix starting with a period and not including
           a directory separator, [.xyz.uvw] for instance.

           Return the given name if it does not contain an extension. *)

val search_substring: string -> string -> int -> int
        (* [search_substring pat str start] returns the position of the first
           occurrence of string [pat] in string [str].  Search starts
           at offset [start] in [str].  Raise [Not_found] if [pat]
           does not occur. *)

val replace_substring: before:string -> after:string -> string -> string
        (* [search_substring ~before ~after str] replaces all
           occurences of [before] with [after] in [str] and returns
           the resulting string. *)

val rev_split_words: string -> string list
        (* [rev_split_words s] splits [s] in blank-separated words, and return
           the list of words in reverse order. *)

val get_ref: 'a list ref -> 'a list
        (* [get_ref lr] returns the content of the list reference [lr] and reset
           its content to the empty list. *)

val fst3: 'a * 'b * 'c -> 'a
val snd3: 'a * 'b * 'c -> 'b
val thd3: 'a * 'b * 'c -> 'c

val fst4: 'a * 'b * 'c * 'd -> 'a
val snd4: 'a * 'b * 'c * 'd -> 'b
val thd4: 'a * 'b * 'c * 'd -> 'c
val for4: 'a * 'b * 'c * 'd -> 'd

module LongString :
  sig
    type t = bytes array
    val create : int -> t
    val length : t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val blit : t -> int -> t -> int -> int -> unit
    val output : out_channel -> t -> int -> int -> unit
    val unsafe_blit_to_bytes : t -> int -> bytes -> int -> int -> unit
    val input_bytes : in_channel -> int -> t
  end

val edit_distance : string -> string -> int -> int option
(** [edit_distance a b cutoff] computes the edit distance between
    strings [a] and [b]. To help efficiency, it uses a cutoff: if the
    distance [d] is smaller than [cutoff], it returns [Some d], else
    [None].

    The distance algorithm currently used is Damerau-Levenshtein: it
    computes the number of insertion, deletion, substitution of
    letters, or swapping of adjacent letters to go from one word to the
    other. The particular algorithm may change in the future.
*)

val spellcheck : string list -> string -> string list
(** [spellcheck env name] takes a list of names [env] that exist in
    the current environment and an erroneous [name], and returns a
    list of suggestions taken from [env], that are close enough to
    [name] that it may be a typo for one of them. *)

val did_you_mean : Format.formatter -> (unit -> string list) -> unit
(** [did_you_mean ppf get_choices] hints that the user may have meant
    one of the option returned by calling [get_choices]. It does nothing
    if the returned list is empty.

    The [unit -> ...] thunking is meant to delay any potentially-slow
    computation (typically computing edit-distance with many things
    from the current environment) to when the hint message is to be
    printed. You should print an understandable error message before
    calling [did_you_mean], so that users get a clear notification of
    the failure even if producing the hint is slow.
*)

val split : string -> char -> string list
(** [String.split string char] splits the string [string] at every char
    [char], and returns the list of sub-strings between the chars.
    [String.concat (String.make 1 c) (String.split s c)] is the identity.
    @since 4.01
 *)

val cut_at : string -> char -> string * string
(** [String.cut_at s c] returns a pair containing the sub-string before
   the first occurrence of [c] in [s], and the sub-string after the
   first occurrence of [c] in [s].
   [let (before, after) = String.cut_at s c in
    before ^ String.make 1 c ^ after] is the identity if [s] contains [c].

   Raise [Not_found] if the character does not appear in the string
   @since 4.01
*)


module StringSet: Set.S with type elt = string
module StringMap: Map.S with type key = string
(* TODO: replace all custom instantiations of StringSet/StringMap in various
   compiler modules with this one. *)

(* Color handling *)
module Color : sig
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
  ;;

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

  val ansi_of_style_l : style list -> string
  (* ANSI escape sequence for the given style *)

  type styles = {
    error: style list;
    warning: style list;
    loc: style list;
  }

  val default_styles: styles
  val get_styles: unit -> styles
  val set_styles: styles -> unit

  val setup : Clflags.color_setting -> unit
  (* [setup opt] will enable or disable color handling on standard formatters
     according to the value of color setting [opt].
     Only the first call to this function has an effect. *)

  val set_color_tag_handling : Format.formatter -> unit
  (* adds functions to support color tags to the given formatter. *)
end
