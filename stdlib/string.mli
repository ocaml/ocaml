(* Module [String]: string operations *)

external length : string -> int = "%string_length"
        (* Return the length (number of characters) of the given string. *)

external get : string -> int -> char = "%string_safe_get"
        (* [String.get s n] returns character number [n] in string [s].
           The first character is character number 0.
           The last character is character number [String.length s - 1].
           Raise [Invalid_argument] if [n] is ouside the range
           0 to [(String.length s - 1)].
           You can also write [s.[n]] instead of [String.get s n]. *)
external set : string -> int -> char -> unit = "%string_safe_set"
        (* [String.set s n c] modifies string [s] in place,
           replacing the character number [n] by [c].
           Raise [Invalid_argument] if [n] is ouside the range
           0 to [(String.length s - 1)].
           You can also write [s.[n] <- c] instead of [String.set s n c]. *)

external create : int -> string = "create_string"
        (* [String.create n] returns a fresh string of length [n].
           The string initially contains arbitrary characters. *)
val make : int -> char -> string
        (* [String.make n c] returns a fresh string of length [n],
           filled with the character [c]. *)
val copy : string -> string
        (* Return a copy of the given string. *)
val sub : string -> int -> int -> string
        (* [String.sub s start len] returns a fresh string of length [len],
           containing the characters number [start] to [start + len - 1]
           of string [s].
           Raise [Invalid_argument] if [start] and [len] do not
           designate a valid substring of [s]; that is, if [start < 0],
           or [len < 0], or [start + len > String.length s]. *)
val fill : string -> int -> int -> char -> unit
        (* [fill_string s start len c] modifies string [s] in place,
           replacing the characters number [start] to [start + len - 1]
           by [c].
           Raise [Invalid_argument] if [start] and [len] do not
           designate a valid substring of [s]. *)
val blit : string -> int -> string -> int -> int -> unit
        (* [blit_string s1 o1 s2 o2 len] copies [len] characters
           from string [s1], starting at character number [o1], to string [s2],
           starting at character number [o2]. It works correctly even if
           [s1] and [s2] are the same string,
           and the source and destination chunks overlap.
           Raise [Invalid_argument] if [o1] and [len] do not
           designate a valid substring of [s1], or if [o2] and [len] do not
           designate a valid substring of [s2]. *)

val concat : string -> string list -> string
        (* Catenate a list of strings. *)

val escaped: string -> string
        (* Return a copy of the argument, with special characters represented
           by escape sequences, following the lexical conventions of
           Caml Light. *)

(*--*)

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int -> string -> int -> int -> unit
                     = "blit_string" "noalloc"
external unsafe_fill : string -> int -> int -> char -> unit
                     = "fill_string" "noalloc"


