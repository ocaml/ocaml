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

(** Miscellaneous useful types and functions

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val fatal_error: string -> 'a
val fatal_errorf: ('a, Format.formatter, unit, 'b) format4 -> 'a
exception Fatal_error

val try_finally :
  ?always:(unit -> unit) ->
  ?exceptionally:(unit -> unit) ->
  (unit -> 'a) -> 'a
(** [try_finally work ~always ~exceptionally] is designed to run code
    in [work] that may fail with an exception, and has two kind of
    cleanup routines: [always], that must be run after any execution
    of the function (typically, freeing system resources), and
    [exceptionally], that should be run only if [work] or [always]
    failed with an exception (typically, undoing user-visible state
    changes that would only make sense if the function completes
    correctly). For example:

    {[
      let objfile = outputprefix ^ ".cmo" in
      let oc = open_out_bin objfile in
      Misc.try_finally
        (fun () ->
           bytecode
           ++ Timings.(accumulate_time (Generate sourcefile))
               (Emitcode.to_file oc modulename objfile);
           Warnings.check_fatal ())
        ~always:(fun () -> close_out oc)
        ~exceptionally:(fun _exn -> remove_file objfile);
    ]}

    If [exceptionally] fail with an exception, it is propagated as
    usual.

    If [always] or [exceptionally] use exceptions internally for
    control-flow but do not raise, then [try_finally] is careful to
    preserve any exception backtrace coming from [work] or [always]
    for easier debugging.
*)


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
val may: ('a -> unit) -> 'a option -> unit
val may_map: ('a -> 'b) -> 'a option -> 'b option

type ref_and_value = R : 'a ref * 'a -> ref_and_value

val protect_refs : ref_and_value list -> (unit -> 'a) -> 'a
(** [protect_refs l f] temporarily sets [r] to [v] for each [R (r, v)] in [l]
    while executing [f]. The previous contents of the references is restored
    even if [f] raises an exception. *)

module Stdlib : sig
  module List : sig
    type 'a t = 'a list

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** The lexicographic order supported by the provided order.
        There is no constraint on the relative lengths of the lists. *)

    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** Returns [true] iff the given lists have the same length and content
        with respect to the given equality function. *)

    val find_map : ('a -> 'b option) -> 'a t -> 'b option
    (** [find_map f l] returns the first evaluation of [f] that returns [Some],
       or returns None if there is no such element. *)

    val some_if_all_elements_are_some : 'a option t -> 'a t option
    (** If all elements of the given list are [Some _] then [Some xs]
        is returned with the [xs] being the contents of those [Some]s, with
        order preserved.  Otherwise return [None]. *)

    val map2_prefix : ('a -> 'b -> 'c) -> 'a t -> 'b t -> ('c t * 'b t)
    (** [let r1, r2 = map2_prefix f l1 l2]
        If [l1] is of length n and [l2 = h2 @ t2] with h2 of length n,
        r1 is [List.map2 f l1 h1] and r2 is t2. *)

    val split_at : int -> 'a t -> 'a t * 'a t
    (** [split_at n l] returns the pair [before, after] where [before] is
        the [n] first elements of [l] and [after] the remaining ones.
        If [l] has less than [n] elements, raises Invalid_argument. *)
  end

  module Option : sig
    type 'a t = 'a option

    val is_none : 'a t -> bool
    val is_some : 'a t -> bool

    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val iter : ('a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val value_default : ('a -> 'b) -> default:'b -> 'a t -> 'b
  end

  module Array : sig
    val exists2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
    (* Same as [Array.exists], but for a two-argument predicate. Raise
       Invalid_argument if the two arrays are determined to have
       different lengths. *)
  end

  module String : sig
    include module type of String
    module Set : Set.S with type elt = string
    module Map : Map.S with type key = string
    module Tbl : Hashtbl.S with type key = string

    val for_all : (char -> bool) -> t -> bool
  end

  external compare : 'a -> 'a -> int = "%compare"
end

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

val split_path_contents: ?sep:char -> string -> string list
(* [split_path_contents ?sep s] interprets [s] as the value of a "PATH"-like
   variable and returns the corresponding list of directories. [s] is split
   using the platform-specific delimiter, or [~sep] if it is passed.

   Returns the empty list if [s] is empty. *)

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
val output_to_file_via_temporary:
      ?mode:open_flag list -> string -> (string -> out_channel -> 'a) -> 'a
        (* Produce output in temporary file, then rename it
           (as atomically as possible) to the desired output file name.
           [output_to_file_via_temporary filename fn] opens a temporary file
           which is passed to [fn] (name + output channel).  When [fn] returns,
           the channel is closed and the temporary file is renamed to
           [filename]. *)

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
        (* [no_overflow_sub n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)
val no_overflow_mul: int -> int -> bool
        (* [no_overflow_mul n1 n2] returns [true] if the computation of
           [n1 * n2] does not overflow. *)
val no_overflow_lsl: int -> int -> bool
        (* [no_overflow_lsl n k] returns [true] if the computation of
           [n lsl k] does not overflow. *)

module Int_literal_converter : sig
  val int : string -> int
  val int32 : string -> int32
  val int64 : string -> int64
  val nativeint : string -> nativeint
end

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
        (* [replace_substring ~before ~after str] replaces all
           occurrences of [before] with [after] in [str] and returns
           the resulting string. *)

val rev_split_words: string -> string list
        (* [rev_split_words s] splits [s] in blank-separated words, and returns
           the list of words in reverse order. *)

val get_ref: 'a list ref -> 'a list
        (* [get_ref lr] returns the content of the list reference [lr] and reset
           its content to the empty list. *)

val set_or_ignore : ('a -> 'b option) -> 'b option ref -> 'a -> unit
        (* [set_or_ignore f opt x] sets [opt] to [f x] if it returns [Some _],
           or leaves it unmodified if it returns [None]. *)

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
    val blit_string : string -> int -> t -> int -> int -> unit
    val output : out_channel -> t -> int -> int -> unit
    val input_bytes_into : t -> in_channel -> int -> unit
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

val cut_at : string -> char -> string * string
(** [String.cut_at s c] returns a pair containing the sub-string before
   the first occurrence of [c] in [s], and the sub-string after the
   first occurrence of [c] in [s].
   [let (before, after) = String.cut_at s c in
    before ^ String.make 1 c ^ after] is the identity if [s] contains [c].

   Raise [Not_found] if the character does not appear in the string
   @since 4.01
*)

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

  type setting = Auto | Always | Never

  val setup : setting option -> unit
  (* [setup opt] will enable or disable color handling on standard formatters
     according to the value of color setting [opt].
     Only the first call to this function has an effect. *)

  val set_color_tag_handling : Format.formatter -> unit
  (* adds functions to support color tags to the given formatter. *)
end

(* See the -error-style option *)
module Error_style : sig
  type setting =
    | Contextual
    | Short
end

val normalise_eol : string -> string
(** [normalise_eol s] returns a fresh copy of [s] with any '\r' characters
   removed. Intended for pre-processing text which will subsequently be printed
   on a channel which performs EOL transformations (i.e. Windows) *)

val delete_eol_spaces : string -> string
(** [delete_eol_spaces s] returns a fresh copy of [s] with any end of
   line spaces removed. Intended to normalize the output of the
   toplevel for tests. *)

val pp_two_columns :
  ?sep:string -> ?max_lines:int ->
  Format.formatter -> (string * string) list -> unit
(** [pp_two_columns ?sep ?max_lines ppf l] prints the lines in [l] as two
   columns separated by [sep] ("|" by default). [max_lines] can be used to
   indicate a maximum number of lines to print -- an ellipsis gets inserted at
   the middle if the input has too many lines.

   Example:

    {v pp_two_columns ~max_lines:3 Format.std_formatter [
      "abc", "hello";
      "def", "zzz";
      "a"  , "bllbl";
      "bb" , "dddddd";
    ] v}

    prints

    {v
    abc | hello
    ...
    bb  | dddddd
    v}
*)

(** {1 Hook machinery}

    Hooks machinery:
   [add_hook name f] will register a function that will be called on the
    argument of a later call to [apply_hooks]. Hooks are applied in the
    lexicographical order of their names.
*)

type hook_info = {
  sourcefile : string;
}

exception HookExnWrapper of
    {
      error: exn;
      hook_name: string;
      hook_info: hook_info;
    }
    (** An exception raised by a hook will be wrapped into a
        [HookExnWrapper] constructor by the hook machinery.  *)


val raise_direct_hook_exn: exn -> 'a
  (** A hook can use [raise_unwrapped_hook_exn] to raise an exception that will
      not be wrapped into a {!HookExnWrapper}. *)

module type HookSig = sig
  type t
  val add_hook : string -> (hook_info -> t -> t) -> unit
  val apply_hooks : hook_info -> t -> t
end

module MakeHooks : functor (M : sig type t end) -> HookSig with type t = M.t


(** configuration variables *)
val show_config_and_exit : unit -> unit
val show_config_variable_and_exit : string -> unit

val get_build_path_prefix_map: unit -> Build_path_prefix_map.map option
(** Returns the map encoded in the [BUILD_PATH_PREFIX_MAP] environment
    variable. *)

val debug_prefix_map_flags: unit -> string list
(** Returns the list of [--debug-prefix-map] flags to be passed to the
    assembler, built from the [BUILD_PATH_PREFIX_MAP] environment variable. *)

val print_if :
  Format.formatter -> bool ref -> (Format.formatter -> 'a -> unit) -> 'a -> 'a
(** [print_if ppf flag fmt x] prints [x] with [fmt] on [ppf] if [b] is true. *)
