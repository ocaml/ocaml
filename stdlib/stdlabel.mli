(* $Id$ *)

(* Module [Stdlabel]: labelized versions of frequently used modules *)

(* Modules are not reimplemented, functions are just labelized in the
   interface. You can mix source files using labels and source files
   without them, with no code size penalty. Labelized versions behave
   as the original ones in all respects, including efficiency. *)

(* From [Pervasives]: the initially opened module *)

val open_out_gen : mode:open_flag list -> perm:int ->
      	       	   file:string -> out_channel
        (* [open_out_gen mode rights filename] opens the file named
           [filename] for writing, as above. The extra argument [mode]
           specify the opening mode. The extra argument [rights] specifies
           the file permissions, in case the file must be created.
           [open_out] and [open_out_bin] are special cases of this function. *)
val output_char : to:out_channel -> char -> unit
        (* Write the character on the given output channel. *)
val output_string : to:out_channel -> string -> unit
        (* Write the string on the given output channel. *)
val output : out_channel -> buffer:string -> pos:int -> len:int -> unit
        (* [output chan buff ofs len] writes [len] characters from string 
           [buff], starting at offset [ofs], to the output channel [chan].
           Raise [Invalid_argument "output"] if [ofs] and [len] do not
           designate a valid substring of [buff]. *)
val output_byte : to:out_channel -> int -> unit
        (* Write one 8-bit integer (as the single character with that code)
           on the given output channel. The given integer is taken modulo
           256. *)
val output_binary_int : to:out_channel -> int -> unit
        (* Write one integer in binary format on the given output channel.
           The only reliable way to read it back is through the
           [input_binary_int] function. The format is compatible across
           all machines for a given version of Objective Caml. *)
val output_value : to:out_channel -> 'a -> unit
        (* Write the representation of a structured value of any type
           to a channel. Circularities and sharing inside the value
           are detected and preserved. The object can be read back,
           by the function [input_value]. See the description of module
           [Marshal] for more information. [output_value] is equivalent
           to [Marshal.to_channel] with an empty list of flags. *)
val seek_out : out_channel -> pos:int -> unit
        (* [seek_out chan pos] sets the current writing position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds (such as terminals, pipes and sockets),
           the behavior is unspecified. *)

val open_in_gen : mode:open_flag list -> perm:int -> file:string -> in_channel
        (* [open_in_gen mode rights filename] opens the file named
           [filename] for reading, as above. The extra arguments
           [mode] and [rights] specify the opening mode and file permissions.
           [open_in] and [open_in_bin] are special cases of this function. *)
val input : in_channel -> buffer:string -> pos:int -> len:int -> int
        (* [input chan buff ofs len] attempts to read [len] characters
           from channel [chan], storing them in string [buff], starting at
           character number [ofs]. It returns the actual number of characters
           read, between 0 and [len] (inclusive).
           A return value of 0 means that the end of file was reached.
           A return value between 0 and [len] exclusive means that
           no more characters were available at that time; [input] must be
           called again to read the remaining characters, if desired.
           Exception [Invalid_argument "input"] is raised if [ofs] and [len]
           do not designate a valid substring of [buff]. *)          
val really_input : in_channel -> buffer:string -> pos:int -> len:int -> unit
        (* [really_input chan buff ofs len] reads [len] characters
           from channel [chan], storing them in string [buff], starting at
           character number [ofs]. Raise [End_of_file] if
           the end of file is reached before [len] characters have been read.
           Raise [Invalid_argument "really_input"] if
           [ofs] and [len] do not designate a valid substring of [buff]. *)
val seek_in : in_channel -> pos:int -> unit
        (* [seek_in chan pos] sets the current reading position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds, the behavior is unspecified. *)

module List : sig

(* From [List]: list operations *)

(* Some functions are flagged as not tail-recursive.  A tail-recursive
   function uses constant stack space, while a non-tail-recursive function
   uses stack space proportional to the length of its list argument, which
   can be a problem with very long lists.  When the function takes several
   list arguments, an approximate formula giving stack usage (in unknown
   units) is shown in parentheses.

   The above considerations can usually be ignored if your lists are not
   longer than about 10000 elements.
*)

val length : 'a list -> int
        (* Return the length (number of elements) of the given list. *)
val hd : 'a list -> 'a
        (* Return the first element of the given list. Raise
           [Failure "hd"] if the list is empty. *)
val tl : 'a list -> 'a list
        (* Return the given list without its first element. Raise
           [Failure "tl"] if the list is empty. *)
val nth : 'a list -> pos:int -> 'a
        (* Return the n-th element of the given list.
           The first element (head of the list) is at position 0.
           Raise [Failure "nth"] if the list is too short. *)
val rev : 'a list -> 'a list
        (* List reversal. *)
val append : 'a list -> 'a list -> 'a list
        (* Catenate two lists.  Same function as the infix operator [@].
           Not tail-recursive.  The [@] operator is not tail-recursive
           either. *)
val rev_append : 'a list -> 'a list -> 'a list
        (* [List.rev_append l1 l2] reverses [l1] and catenates it to [l2].
           This is equivalent to [List.rev l1 @ l2], but [rev_append] is
           tail-recursive and more efficient. *)
val concat  : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
        (* Catenate (flatten) a list of lists.  Not tail-recursive
           (length of the argument + length of the longest sub-list). *)

(** Iterators *)

val iter : fun:('a -> unit) -> 'a list -> unit
        (* [List.iter f [a1; ...; an]] applies function [f] in turn to
           [a1; ...; an]. It is equivalent to
           [begin f a1; f a2; ...; f an; () end]. *)
val map : fun:('a -> 'b) -> 'a list -> 'b list
        (* [List.map f [a1; ...; an]] applies function [f] to [a1, ..., an],
           and builds the list [[f a1; ...; f an]]
           with the results returned by [f].  Not tail-recursive. *)
val rev_map : fun:('a -> 'b) -> 'a list -> 'b list
        (* [List.rev_map f l] gives the same result as
           [List.rev (List.map f l)], but is tail-recursive and
           more efficient. *)
val fold_left : fun:(acc:'a -> 'b -> 'a) -> acc:'a -> 'b list -> 'a
        (* [List.fold_left f a [b1; ...; bn]] is
           [f (... (f (f a b1) b2) ...) bn]. *)
val fold_right : fun:('a -> acc:'b -> 'b) -> 'a list -> acc:'b -> 'b
        (* [List.fold_right f [a1; ...; an] b] is
           [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. *)

(** Iterators on two lists *)

val iter2 : fun:('a -> 'b -> unit) -> 'a list -> 'b list -> unit
        (* [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
           [f a1 b1; ...; f an bn].
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val map2 : fun:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        (* [List.map2 f [a1; ...; an] [b1; ...; bn]] is
           [[f a1 b1; ...; f an bn]].
           Raise [Invalid_argument] if the two lists have
           different lengths.  Not tail-recursive. *)
val rev_map2 : fun:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        (* [List.rev_map2 f l] gives the same result as
           [List.rev (List.map2 f l)], but is tail-recursive and
           more efficient. *)
val fold_left2 :
	fun:(acc:'a -> 'b -> 'c -> 'a) -> acc:'a -> 'b list -> 'c list -> 'a
        (* [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
           [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val fold_right2 :
	fun:('a -> 'b -> acc:'c -> 'c) -> 'a list -> 'b list -> acc:'c -> 'c
        (* [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
           [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
           Raise [Invalid_argument] if the two lists have
           different lengths.  Not tail-recursive. *)

(** List scanning *)

val for_all : pred:('a -> bool) -> 'a list -> bool
        (* [for_all p [a1; ...; an]] checks if all elements of the list
           satisfy the predicate [p]. That is, it returns
           [(p a1) && (p a2) && ... && (p an)]. *)
val exists : pred:('a -> bool) -> 'a list -> bool
        (* [exists p [a1; ...; an]] checks if at least one element of
           the list satisfies the predicate [p]. That is, it returns
           [(p a1) || (p a2) || ... || (p an)]. *)
val for_all2 : pred:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : pred:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        (* Same as [for_all] and [exists], but for a two-argument predicate.
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val mem : elt:'a -> 'a list -> bool
        (* [mem a l] is true if and only if [a] is equal
           to an element of [l]. *)
val memq : elt:'a -> 'a list -> bool
        (* Same as [mem], but uses physical equality instead of structural
           equality to compare list elements. *)

(** List searching *)

val find : pred:('a -> bool) -> 'a list -> 'a
        (* [find p l] returns the first element of the list [l]
           that satisfies the predicate [p].
           Raise [Not_found] if there is no value that satisfies [p] in the
           list [l]. *)

val filter : pred:('a -> bool) -> 'a list -> 'a list
val find_all : pred:('a -> bool) -> 'a list -> 'a list
        (* [filter p l] returns all the elements of the list [l]
           that satisfies the predicate [p].  The order of the elements
           in the input list is preserved.  [find_all] is another name
           for [filter]. *)

val partition : pred:('a -> bool) -> 'a list -> 'a list * 'a list
        (* [partition p l] returns a pair of lists [(l1, l2)], where
           [l1] is the list of all the elements of [l] that
           satisfy the predicate [p], and [l2] is the list of all the
           elements of [l] that do not satisfy [p].
           The order of the elements in the input list is preserved. *)

(** Association lists *)

val assoc : key:'a -> ('a * 'b) list -> 'b
        (* [assoc a l] returns the value associated with key [a] in the list of
           pairs [l]. That is,
             [assoc a [ ...; (a,b); ...] = b]
           if [(a,b)] is the leftmost binding of [a] in list [l].
           Raise [Not_found] if there is no value associated with [a] in the
           list [l]. *)
val assq : key:'a -> ('a * 'b) list -> 'b
        (* Same as [assoc], but uses physical equality instead of structural
           equality to compare keys. *)

val mem_assoc : key:'a -> ('a * 'b) list -> bool
        (* Same as [assoc], but simply return true if a binding exists,
           and false if no bindings exist for the given key. *)
val mem_assq : key:'a -> ('a * 'b) list -> bool
        (* Same as [mem_assoc], but uses physical equality instead of
           structural equality to compare keys. *)

val remove_assoc : key:'a -> ('a * 'b) list -> ('a * 'b) list
        (* [remove_assoc a l] returns the list of
           pairs [l] without the first pair with key [a], if any.
           Not tail-recursive. *)

val remove_assq : key:'a -> ('a * 'b) list -> ('a * 'b) list
        (* Same as [remove_assq], but uses physical equality instead
           of structural equality to compare keys.  Not tail-recursive. *)

(** Lists of pairs *)

val split : ('a * 'b) list -> 'a list * 'b list
        (* Transform a list of pairs into a pair of lists:
           [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
           Not tail-recursive.
        *)
val combine : 'a list -> 'b list -> ('a * 'b) list
        (* Transform a pair of lists into a list of pairs:
           [combine ([a1; ...; an], [b1; ...; bn])] is
              [[(a1,b1); ...; (an,bn)]].
           Raise [Invalid_argument] if the two lists
           have different lengths.  Not tail-recursive. *)

end

module Array : sig

external length : 'a array -> int = "%array_length"
        (* Return the length (number of elements) of the given array. *)
external get: 'a array -> int -> 'a = "%array_safe_get"
        (* [Array.get a n] returns the element number [n] of array [a].
           The first element has number 0.
           The last element has number [Array.length a - 1].
           Raise [Invalid_argument "Array.get"]  if [n] is outside the range
           0 to [(Array.length a - 1)].
           You can also write [a.(n)] instead of [Array.get a n]. *)
external set: 'a array -> int -> 'a -> unit = "%array_safe_set"
        (* [Array.set a n x] modifies array [a] in place, replacing
           element number [n] with [x].
           Raise [Invalid_argument "Array.set"] if [n] is outside the range
           0 to [Array.length a - 1].
           You can also write [a.(n) <- x] instead of [Array.set a n x]. *)
external make: len:int -> 'a -> 'a array = "make_vect"
external create: len:int -> 'a -> 'a array = "make_vect"
        (* [Array.make n x] returns a fresh array of length [n],
           initialized with [x].
           All the elements of this new array are initially
           physically equal to [x] (in the sense of the [==] predicate).
           Consequently, if [x] is mutable, it is shared among all elements
           of the array, and modifying [x] through one of the array entries
           will modify all other entries at the same time.
           Raise [Invalid_argument] if [n <= 0] or [n > Sys.max_array_length].
           If the value of [x] is a floating-point number, then the maximum
           size is only [Sys.max_array_length / 2].
           [Array.create] is a deprecated alias for [Array.make]. *)
val init: len:int -> fun:(int -> 'a) -> 'a array
        (* [Array.init n f] returns a fresh array of length [n],
           with element number [i] initialized to the result of [f i].
           In other terms, [Array.init n f] tabulates the results of [f]
           applied to the integers [0] to [n-1]. *)
val make_matrix: dimx:int -> dimy:int -> 'a -> 'a array array
val create_matrix: dimx:int -> dimy:int -> 'a -> 'a array array
        (* [Array.make_matrix dimx dimy e] returns a two-dimensional array
           (an array of arrays) with first dimension [dimx] and
           second dimension [dimy]. All the elements of this new matrix
           are initially physically equal to [e].
           The element ([x,y]) of a matrix [m] is accessed
           with the notation [m.(x).(y)].
           Raise [Invalid_argument] if [dimx] or [dimy] is less than 1 or
           greater than [Sys.max_array_length].
           If the value of [e] is a floating-point number, then the maximum
           size is only [Sys.max_array_length / 2].
           [Array.create_matrix] is a deprecated alias for [Array.make_matrix].
           *)
val append: 'a array -> 'a array -> 'a array
        (* [Array.append v1 v2] returns a fresh array containing the
           concatenation of the arrays [v1] and [v2]. *)
val concat: 'a array list -> 'a array
        (* Same as [Array.append], but catenates a list of arrays. *)
val sub: 'a array -> pos:int -> len:int -> 'a array
        (* [Array.sub a start len] returns a fresh array of length [len],
           containing the elements number [start] to [start + len - 1]
           of array [a].
           Raise [Invalid_argument "Array.sub"] if [start] and [len] do not
           designate a valid subarray of [a]; that is, if
           [start < 0], or [len < 0], or [start + len > Array.length a]. *)
val copy: 'a array -> 'a array
        (* [Array.copy a] returns a copy of [a], that is, a fresh array
           containing the same elements as [a]. *)
val fill: 'a array -> pos:int -> len:int -> 'a -> unit
        (* [Array.fill a ofs len x] modifies the array [a] in place,
           storing [x] in elements number [ofs] to [ofs + len - 1].
           Raise [Invalid_argument "Array.fill"] if [ofs] and [len] do not
           designate a valid subarray of [a]. *)
val blit: 'a array -> pos:int -> to:'a array -> to_pos:int -> len:int -> unit
        (* [Array.blit v1 o1 v2 o2 len] copies [len] elements
           from array [v1], starting at element number [o1], to array [v2],
           starting at element number [o2]. It works correctly even if
           [v1] and [v2] are the same array, and the source and
           destination chunks overlap.
           Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not
           designate a valid subarray of [v1], or if [o2] and [len] do not
           designate a valid subarray of [v2]. *)
val to_list: 'a array -> 'a list
        (* [Array.to_list a] returns the list of all the elements of [a]. *)
val of_list: 'a list -> 'a array
        (* [Array.of_list l] returns a fresh array containing the elements
           of [l]. *)
val iter: fun:('a -> unit) -> 'a array -> unit
        (* [Array.iter f a] applies function [f] in turn to all
           the elements of [a].  It is equivalent to
           [f a.(0); f a.(1); ...; f a.(Array.length a - 1); ()]. *)
val map: fun:('a -> 'b) -> 'a array -> 'b array
        (* [Array.map f a] applies function [f] to all the elements of [a],
           and builds an array with the results returned by [f]:
           [[| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |]]. *)
val iteri: fun:(i:int -> 'a -> unit) -> 'a array -> unit
val mapi: fun:(i:int -> 'a -> 'b) -> 'a array -> 'b array
        (* Same as [Array.iter] and [Array.map] respectively, but the
           function is applied to the index of the element as first argument,
           and the element itself as second argument. *)
val fold_left: fun:(acc:'a -> 'b -> 'a) -> acc:'a -> 'b array -> 'a
        (* [Array.fold_left f x a] computes
           [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
           where [n] is the length of the array [a]. *)
val fold_right: fun:('b -> acc:'a -> 'a) -> 'b array -> acc:'a -> 'a
        (* [Array.fold_right f a x] computes
           [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))],
           where [n] is the length of the array [a]. *)
(*--*)

external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"

end

module String : sig

external length : string -> int = "%string_length"
        (* Return the length (number of characters) of the given string. *)

external get : string -> int -> char = "%string_safe_get"
        (* [String.get s n] returns character number [n] in string [s].
           The first character is character number 0.
           The last character is character number [String.length s - 1].
           Raise [Invalid_argument] if [n] is outside the range
           0 to [(String.length s - 1)].
           You can also write [s.[n]] instead of [String.get s n]. *)
external set : string -> int -> char -> unit = "%string_safe_set"
        (* [String.set s n c] modifies string [s] in place,
           replacing the character number [n] by [c].
           Raise [Invalid_argument] if [n] is outside the range
           0 to [(String.length s - 1)].
           You can also write [s.[n] <- c] instead of [String.set s n c]. *)

external create : len:int -> string = "create_string"
        (* [String.create n] returns a fresh string of length [n].
           The string initially contains arbitrary characters.
           Raise [Invalid_argument] if [n <= 0] or [n > Sys.max_string_length].
           *)
val make : len:int -> char -> string
        (* [String.make n c] returns a fresh string of length [n],
           filled with the character [c].
           Raise [Invalid_argument] if [n <= 0] or [n > Sys.max_string_length].
           *)
val copy : string -> string
        (* Return a copy of the given string. *)
val sub : string -> pos:int -> len:int -> string
        (* [String.sub s start len] returns a fresh string of length [len],
           containing the characters number [start] to [start + len - 1]
           of string [s].
           Raise [Invalid_argument] if [start] and [len] do not
           designate a valid substring of [s]; that is, if [start < 0],
           or [len < 0], or [start + len > String.length s]. *)
val fill : string -> pos:int -> len:int -> char -> unit
        (* [String.fill s start len c] modifies string [s] in place,
           replacing the characters number [start] to [start + len - 1]
           by [c].
           Raise [Invalid_argument] if [start] and [len] do not
           designate a valid substring of [s]. *)
val blit : string -> pos:int -> to:string -> to_pos:int -> len:int -> unit
        (* [String.blit src srcoff dst dstoff len] copies [len] characters
           from string [src], starting at character number [srcoff], to
           string [dst], starting at character number [dstoff]. It works
           correctly even if [src] and [dst] are the same string,
           and the source and destination chunks overlap.
           Raise [Invalid_argument] if [srcoff] and [len] do not
           designate a valid substring of [src], or if [dstoff] and [len]
           do not designate a valid substring of [dst]. *)

val concat : string -> string list -> string
        (* [String.concat sep sl] catenates the list of strings [sl],
           inserting the separator string [sep] between each. *)

val escaped: string -> string
        (* Return a copy of the argument, with special characters represented
           by escape sequences, following the lexical conventions of
           Objective Caml. *)

val index: string -> elt:char -> int
        (* [String.index s c] returns the position of the leftmost
           occurrence of character [c] in string [s].
           Raise [Not_found] if [c] does not occur in [s]. *)
val rindex: string -> elt:char -> int
        (* [String.rindex s c] returns the position of the rightmost
           occurrence of character [c] in string [s].
           Raise [Not_found] if [c] does not occur in [s]. *)
val index_from: string -> pos:int -> elt:char -> int
val rindex_from: string -> pos:int -> elt:char -> int
        (* Same as [String.index] and [String.rindex], but start
           searching at the character position given as second argument.
           [String.index s c] is equivalent to [String.index_from s 0 c],
           and [String.rindex s c] to
           [String.rindex_from s (String.length s - 1) c]. *)

val contains : string -> elt:char -> bool
        (* [String.contains s c] tests if character [c]
           appears in the string [s]. *)
val contains_from : string -> pos:int -> elt:char -> bool
        (* [String.contains_from s start c] tests if character [c]
           appears in the substring of [s] starting from [start] to the end
           of [s].
           Raise [Invalid_argument] if [start] is not a valid index of [s]. *)
val rcontains_from : string -> pos:int -> elt:char -> bool
        (* [String.rcontains_from s stop c] tests if character [c]
           appears in the substring of [s] starting from the beginning
           of [s] to index [stop].
           Raise [Invalid_argument] if [stop] is not a valid index of [s]. *)

val uppercase: string -> string
        (* Return a copy of the argument, with all lowercase letters
           translated to uppercase, including accented letters of the ISO
           Latin-1 (8859-1) character set. *)
val lowercase: string -> string
        (* Return a copy of the argument, with all uppercase letters
           translated to lowercase, including accented letters of the ISO
           Latin-1 (8859-1) character set. *)
val capitalize: string -> string
        (* Return a copy of the argument, with the first letter
           set to uppercase. *)
val uncapitalize: string -> string
        (* Return a copy of the argument, with the first letter
           set to lowercase. *)

(*--*)

external unsafe_get : string -> pos:int -> char = "%string_unsafe_get"
external unsafe_set : string -> pos:int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
	string -> pos:int -> to:string -> to_pos:int -> len:int -> unit
        = "blit_string" "noalloc"
external unsafe_fill : string -> pos:int -> len:int -> char -> unit
        = "fill_string" "noalloc"

end
