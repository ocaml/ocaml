(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Pervasives]: the initially opened module *)

(* This module provides the built-in types (numbers, booleans,
   strings, exceptions, references, lists, arrays, input-output channels, ...)
   and the basic operations over these types.

   This module is automatically opened at the beginning of each compilation.
   All components of this module can therefore be referred by their short
   name, without prefixing them by [Pervasives]. *)

(*** Predefined types *)

(*- type int *)
        (* The type of integer numbers. *)
(*- type char *)
        (* The type of characters. *)
(*- type string *)
        (* The type of character strings. *)
(*- type float *)
        (* The type of floating-point numbers. *)
(*- type bool *)
        (* The type of booleans (truth values). *)
(*- type unit = () *)
        (* The type of the unit value. *)
(*- type exn *)
        (* The type of exception values. *)
(*- type 'a array *)
        (* The type of arrays whose elements have type ['a]. *)
(*- type 'a list = [] | :: of 'a * 'a list *)
        (* The type of lists whose elements have type ['a]. *)
type 'a option = None | Some of 'a
        (* The type of optional values. *)
(*- type ('a, 'b, 'c) format *)
        (* The type of format strings. ['a] is the type of the parameters
           of the format, ['c] is the result type for the [printf]-style
           function, and ['b] is the type of the first argument given to
           [%a] and [%t] printing functions (see module [Printf]). *)

(*** Exceptions *)

external raise : exn -> 'a = "%raise"
        (* Raise the given exception value *)
(*- exception Match_failure of (string * int * int) *)
        (* Exception raised when none of the cases of a pattern-matching
           apply. The arguments are the location of the pattern-matching
           in the source code (file name, position of first character,
           position of last character). *)
exception Assert_failure of (string * int * int)
        (* Exception raised when an assertion fails.  The arguments are
           the location of the pattern-matching in the source code
           (file name, position of first character, position of last
           character). *)
(*- exception Invalid_argument of string *)
        (* Exception raised by library functions to signal that the given
           arguments do not make sense. *)
(*- exception Failure of string *)
        (* Exception raised by library functions to signal that they are
           undefined on the given arguments. *)
(*- exception Not_found *)
        (* Exception raised by search functions when the desired object
           could not be found. *)
(*- exception Out_of_memory *)
        (* Exception raised by the garbage collector
           when there is insufficient memory to complete the computation. *)
(*- exception Stack_overflow *)
        (* Exception raised by the bytecode interpreter when the evaluation
           stack reaches its maximal size. This often indicates infinite
           or excessively deep recursion in the user's program. *)
(*- exception Sys_error of string *)
        (* Exception raised by the input/output functions to report
           an operating system error. *)
(*- exception End_of_file *)
        (* Exception raised by input functions to signal that the
           end of file has been reached. *)
(*- exception Division_by_zero *)
        (* Exception raised by division and remainder operations
           when their second argument is null. *)
exception Exit
        (* This exception is not raised by any library function.  It is
           provided for use in your programs. *)

val invalid_arg: string -> 'a
        (* Raise exception [Invalid_argument] with the given string. *)
val failwith: string -> 'a
        (* Raise exception [Failure] with the given string. *)

(*** Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
        (* [e1 = e2] tests for structural equality of [e1] and [e2].
           Mutable structures (e.g. references and arrays) are equal
           if and only if their current contents are structurally equal,
           even if the two mutable objects are not the same physical object.
           Equality between functional values raises [Invalid_argument].
           Equality between cyclic data structures may not terminate. *)
external (<>) : 'a -> 'a -> bool = "%notequal"
        (* Negation of [(=)]. *)
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
        (* Structural ordering functions. These functions coincide with
           the usual orderings over integer, string and floating-point
           numbers, and extend them to a total ordering over all types.
           The ordering is compatible with [(=)]. As in the case
           of [(=)], mutable structures are compared by contents.
           Comparison between functional values raises [Invalid_argument].
           Comparison between cyclic structures may not terminate. *)
external compare: 'a -> 'a -> int = "compare" "noalloc"
        (* [compare x y] returns [0] if [x=y], a negative integer if
           [x<y], and a positive integer if [x>y]. The same restrictions
           as for [=] apply. [compare] can be used as the comparison function
           required by the [Set] and [Map] modules. *)
val min: 'a -> 'a -> 'a
        (* Return the smaller of the two arguments. *)
val max: 'a -> 'a -> 'a
        (* Return the greater of the two arguments. *)
external (==) : 'a -> 'a -> bool = "%eq"
        (* [e1 == e2] tests for physical equality of [e1] and [e2].
           On integers and characters, it is the same as structural
           equality. On mutable structures, [e1 == e2] is true if and only if
           physical modification of [e1] also affects [e2].
           On non-mutable structures, the behavior of [(==)] is
           implementation-dependent, except that [e1 == e2] implies
           [e1 = e2]. *)
external (!=) : 'a -> 'a -> bool = "%noteq"
        (* Negation of [(==)]. *)

(*** Boolean operations *)

external not : bool -> bool = "%boolnot"
        (* The boolean negation. *)
external (&) : bool -> bool -> bool = "%sequand"
external (&&) : bool -> bool -> bool = "%sequand"
        (* The boolean ``and''. Evaluation is sequential, left-to-right:
           in [e1 & e2], [e1] is evaluated first, and if it returns [false],
           [e2] is not evaluated at all. *)
external (or) : bool -> bool -> bool = "%sequor"
external (||) : bool -> bool -> bool = "%sequor"
        (* The boolean ``or''. Evaluation is sequential, left-to-right:
           in [e1 or e2], [e1] is evaluated first, and if it returns [true],
           [e2] is not evaluated at all. *)

(*** Integer arithmetic *)

(* Integers are 31 bits wide (or 63 bits on 64-bit processors).
   All operations are taken modulo $2^{31}$ (or $2^{63}$).
   They do not fail on overflow. *)

external (~-) : int -> int = "%negint"
        (* Unary negation. You can also write [-e] instead of [~-e]. *)
external succ : int -> int = "%succint"
        (* [succ x] is [x+1]. *)
external pred : int -> int = "%predint"
        (* [pred x] is [x-1]. *)
external (+) : int -> int -> int = "%addint"
        (* Integer addition. *)
external (-) : int -> int -> int = "%subint"
        (* Integer subtraction. *)
external ( * ) : int -> int -> int = "%mulint"
        (* Integer multiplication. *)
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"
        (* Integer division and remainder.
           Raise [Division_by_zero] if the second argument is 0.
           If one of the arguments is negative, the result is
           platform-dependent. *)
val abs : int -> int
        (* Return the absolute value of the argument. *)
val max_int: int
val min_int: int
        (* The greatest and smallest representable integers. *)


(** Bitwise operations *)

external (land) : int -> int -> int = "%andint"
        (* Bitwise logical and. *)
external (lor) : int -> int -> int = "%orint"
        (* Bitwise logical or. *)
external (lxor) : int -> int -> int = "%xorint"
        (* Bitwise logical exclusive or. *)
val lnot: int -> int
        (* Bitwise logical negation. *)
external (lsl) : int -> int -> int = "%lslint"
        (* [n lsl m] shifts [n] to the left by [m] bits. *)
external (lsr) : int -> int -> int = "%lsrint"
        (* [n lsr m] shifts [n] to the right by [m] bits.
           This is a logical shift: zeroes are inserted regardless of
           the sign of [n].*)
external (asr) : int -> int -> int = "%asrint"
        (* [n asr m] shifts [n] to the right by [m] bits.
           This is an arithmetic shift: the sign bit of [n] is replicated. *)

(*** Floating-point arithmetic *)

(* On most platforms, Caml's floating-point numbers follow the
   IEEE 754 standard, using double precision (64 bits) numbers.
   Floating-point operations do not fail on overflow or underflow,
   but return denormal numbers. *)

external (~-.) : float -> float = "%negfloat"
        (* Unary negation. You can also write [-.e] instead of [~-.e]. *)
external (+.) : float -> float -> float = "%addfloat"
        (* Floating-point addition *)
external (-.) : float -> float -> float = "%subfloat"
        (* Floating-point subtraction *)
external ( *. ) : float -> float -> float = "%mulfloat"
        (* Floating-point multiplication *)
external (/.) : float -> float -> float = "%divfloat"
        (* Floating-point division. *)
external ( ** ) : float -> float -> float = "power_float" "pow" "float"
        (* Exponentiation *)
external exp : float -> float = "exp_float" "exp" "float"

external acos : float -> float = "acos_float" "acos" "float"
external asin : float -> float = "asin_float" "asin" "float"
external atan : float -> float = "atan_float" "atan" "float"
external atan2 : float -> float -> float = "atan2_float" "atan2" "float"
external cos : float -> float = "cos_float" "cos" "float"
external cosh : float -> float = "cosh_float" "cosh" "float"

external log : float -> float = "log_float" "log" "float"
external log10 : float -> float = "log10_float" "log10" "float"

external sin : float -> float = "sin_float" "sin" "float"
external sinh : float -> float = "sinh_float" "sinh" "float"
external sqrt : float -> float = "sqrt_float" "sqrt" "float"
external tan : float -> float = "tan_float" "tan" "float"
external tanh : float -> float = "tanh_float" "tanh" "float"
        (* Usual transcendental functions on floating-point numbers. *)
external ceil : float -> float = "ceil_float" "ceil" "float"
external floor : float -> float = "floor_float" "floor" "float"
          (* Round the given float to an integer value.
             [floor f] returns the greatest integer value less than or
             equal to [f].
             [ceil f] returns the least integer value greater than or
             equal to [f]. *)
external abs_float : float -> float = "%absfloat"
        (* Return the absolute value of the argument. *)
external mod_float : float -> float -> float = "fmod_float" "fmod" "float"
          (* [fmod a b] returns the remainder of [a] with respect to
             [b]. *)
external frexp : float -> float * int = "frexp_float"
          (* [frexp f] returns the pair of the significant
             and the exponent of [f] (when [f] is zero, the
             significant [x] and the exponent [n] of [f] are equal to
             zero; when [f] is non-zero, they are defined by
             [f = x *. 2 ** n]). *)
external ldexp : float -> int -> float = "ldexp_float"
           (* [ldexp x n] returns [x *. 2 ** n]. *)
external modf : float -> float * float = "modf_float" "modf"
           (* [modf f] returns the pair of the fractional and integral
              part of [f]. *)
external float : int -> float = "%floatofint"
        (* Convert an integer to floating-point. *)
external truncate : float -> int = "%intoffloat"
        (* Truncate the given floating-point number to an integer.
           The result is unspecified if it falls outside the
           range of representable integers. *)

(*** String operations *)

(* More string operations are provided in module [String]. *)

val (^) : string -> string -> string
        (* String concatenation. *)

(*** String conversion functions *)

val string_of_bool : bool -> string
        (* Return the string representation of a boolean. *)
val string_of_int : int -> string
        (* Return the string representation of an integer, in decimal. *)
external int_of_string : string -> int = "int_of_string"
        (* Convert the given string to an integer.
           The string is read in decimal (by default) or in hexadecimal,
           octal or binary if the string begins with [0x], [0o] or [0b]
           respectively.
           Raise [Failure "int_of_string"] if the given string is not
           a valid representation of an integer. *)
val string_of_float : float -> string
        (* Return the string representation of a floating-point number. *)
external float_of_string : string -> float = "float_of_string"
        (* Convert the given string to a float.
           The result is unspecified if the given string is not
           a valid representation of a float. *)

(*** Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
        (* Return the first component of a pair. *)
external snd : 'a * 'b -> 'b = "%field1"
        (* Return the second component of a pair. *)

(*** List operations *)

(* More list operations are provided in module [List]. *)

val (@) : 'a list -> 'a list -> 'a list
        (* List concatenation. *)

(*** Input/output *)

type in_channel
type out_channel
        (* The types of input channels and output channels. *)

val stdin : in_channel
val stdout : out_channel
val stderr : out_channel
        (* The standard input, standard output, and standard error output
           for the process. *)

(** Output functions on standard output *)

val print_char : char -> unit
        (* Print a character on standard output. *)
val print_string : string -> unit
        (* Print a string on standard output. *)
val print_int : int -> unit
        (* Print an integer, in decimal, on standard output. *)
val print_float : float -> unit
        (* Print a floating-point number, in decimal, on standard output. *)
val print_endline : string -> unit
        (* Print a string, followed by a newline character, on
           standard output. *)
val print_newline : unit -> unit
        (* Print a newline character on standard output, and flush
           standard output. This can be used to simulate line
           buffering of standard output. *)

(** Output functions on standard error *)

val prerr_char : char -> unit
        (* Print a character on standard error. *)
val prerr_string : string -> unit
        (* Print a string on standard error. *)
val prerr_int : int -> unit
        (* Print an integer, in decimal, on standard error. *)
val prerr_float : float -> unit
        (* Print a floating-point number, in decimal, on standard error. *)
val prerr_endline : string -> unit
        (* Print a string, followed by a newline character on standard error
           and flush standard error. *)
val prerr_newline : unit -> unit
        (* Print a newline character on standard error, and flush
           standard error. *)

(** Input functions on standard input *)

val read_line : unit -> string
        (* Flush standard output, then read characters from standard input
           until a newline character is encountered. Return the string of
           all characters read, without the newline character at the end. *)
val read_int : unit -> int
        (* Flush standard output, then read one line from standard input
           and convert it to an integer. Raise [Failure "int_of_string"]
           if the line read is not a valid representation of an integer. *)
val read_float : unit -> float
        (* Flush standard output, then read one line from standard input
           and convert it to a floating-point number.
           The result is unspecified if the line read is not a valid
           representation of a floating-point number. *)

(** General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock
        (* Opening modes for [open_out_gen] and [open_in_gen].
-          [Open_rdonly]: open for reading.
-          [Open_wronly]: open for writing.
-          [Open_append]: open for appending.
-          [Open_creat]: create the file if it does not exist.
-          [Open_trunc]: empty the file if it already exists.
-          [Open_excl]: fail if the file already exists.
-          [Open_binary]: open in binary mode (no conversion).
-          [Open_text]: open in text mode (may perform conversions).
-          [Open_nonblock]: open in non-blocking mode. *)
           
val open_out : string -> out_channel
        (* Open the named file for writing, and return a new output channel
           on that file, positionned at the beginning of the file. The
           file is truncated to zero length if it already exists. It
           is created if it does not already exists.
           Raise [Sys_error] if the file could not be opened. *)
val open_out_bin : string -> out_channel
        (* Same as [open_out], but the file is opened in binary mode,
           so that no translation takes place during writes. On operating
           systems that do not distinguish between text mode and binary
           mode, this function behaves like [open_out]. *)
val open_out_gen : open_flag list -> int -> string -> out_channel
        (* [open_out_gen mode rights filename] opens the file named
           [filename] for writing, as above. The extra argument [mode]
           specify the opening mode. The extra argument [rights] specifies
           the file permissions, in case the file must be created.
           [open_out] and [open_out_bin] are special cases of this function. *)
val flush : out_channel -> unit
        (* Flush the buffer associated with the given output channel, 
           performing all pending writes on that channel.
           Interactive programs must be careful about flushing standard
           output and standard error at the right time. *)
val output_char : out_channel -> char -> unit
        (* Write the character on the given output channel. *)
val output_string : out_channel -> string -> unit
        (* Write the string on the given output channel. *)
val output : out_channel -> string -> int -> int -> unit
        (* [output chan buff ofs len] writes [len] characters from string 
           [buff], starting at offset [ofs], to the output channel [chan].
           Raise [Invalid_argument "output"] if [ofs] and [len] do not
           designate a valid substring of [buff]. *)
val output_byte : out_channel -> int -> unit
        (* Write one 8-bit integer (as the single character with that code)
           on the given output channel. The given integer is taken modulo
           256. *)
val output_binary_int : out_channel -> int -> unit
        (* Write one integer in binary format on the given output channel.
           The only reliable way to read it back is through the
           [input_binary_int] function. The format is compatible across
           all machines for a given version of Objective Caml. *)
val output_value : out_channel -> 'a -> unit
        (* Write the representation of a structured value of any type
           to a channel. Circularities and sharing inside the value
           are detected and preserved. The object can be read back,
           by the function [input_value]. The format is compatible across
           all machines for a given version of Objective Caml. *)
val seek_out : out_channel -> int -> unit
        (* [seek_out chan pos] sets the current writing position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds (such as terminals, pipes and sockets),
           the behavior is unspecified. *)
val pos_out : out_channel -> int
        (* Return the current writing position for the given channel. *)
val out_channel_length : out_channel -> int
        (* Return the total length (number of characters) of the
           given channel.  This works only for regular files. On files of
           other kinds, the result is meaningless. *)
val close_out : out_channel -> unit
        (* Close the given channel, flushing all buffered write operations.
           The behavior is unspecified if any of the functions above is
           called on a closed channel. *)

(** General input functions *)

val open_in : string -> in_channel
        (* Open the named file for reading, and return a new input channel
           on that file, positionned at the beginning of the file.
           Raise [Sys_error] if the file could not be opened. *)
val open_in_bin : string -> in_channel
        (* Same as [open_in], but the file is opened in binary mode,
           so that no translation takes place during reads. On operating
           systems that do not distinguish between text mode and binary
           mode, this function behaves like [open_in]. *)
val open_in_gen : open_flag list -> int -> string -> in_channel
        (* [open_in_gen mode rights filename] opens the file named
           [filename] for reading, as above. The extra arguments
           [mode] and [rights] specify the opening mode and file permissions.
           [open_in] and [open_in_bin] are special cases of this function. *)
val input_char : in_channel -> char
        (* Read one character from the given input channel.
           Raise [End_of_file] if there are no more characters to read. *)
val input_line : in_channel -> string
        (* Read characters from the given input channel, until a
           newline character is encountered. Return the string of
           all characters read, without the newline character at the end.
           Raise [End_of_file] if the end of the file is reached
           at the beginning of line. *)
val input : in_channel -> string -> int -> int -> int
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
val really_input : in_channel -> string -> int -> int -> unit
        (* [really_input chan buff ofs len] reads [len] characters
           from channel [chan], storing them in string [buff], starting at
           character number [ofs]. Raise [End_of_file] if
           the end of file is reached before [len] characters have been read.
           Raise [Invalid_argument "really_input"] if
           [ofs] and [len] do not designate a valid substring of [buff]. *)
val input_byte : in_channel -> int
        (* Same as [input_char], but return the 8-bit integer representing
           the character.
           Raise [End_of_file] if an end of file was reached. *)
val input_binary_int : in_channel -> int
        (* Read an integer encoded in binary format from the given input
           channel. See [output_binary_int].
           Raise [End_of_file] if an end of file was reached while reading the
           integer. *)
val input_value : in_channel -> 'a
        (* Read the representation of a structured value, as produced
           by [output_value], and return the corresponding value.
           This is not type-safe. The type of the returned object is
           not ['a] properly speaking: the returned object has one
           unique type, which cannot be determined at compile-time.
           The programmer should explicitly give the expected type of the
           returned value, using the following syntax:
                     [(input_value chan : type)].
           The behavior is unspecified if the object in the file does not
           belong to the given type. *)
val seek_in : in_channel -> int -> unit
        (* [seek_in chan pos] sets the current reading position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds, the behavior is unspecified. *)
val pos_in : in_channel -> int
        (* Return the current reading position for the given channel. *)
val in_channel_length : in_channel -> int
        (* Return the total length (number of characters) of the
           given channel. This works only for regular files. On files of
           other kinds, the result is meaningless. *)
val close_in : in_channel -> unit
        (* Close the given channel. Anything can happen if any of the
           functions above is called on a closed channel. *)

(*** References *)

type 'a ref = { mutable contents: 'a }
        (* The type of references (mutable indirection cells) containing
           a value of type ['a]. *)
external ref : 'a -> 'a ref = "%makemutable"
        (* Return a fresh reference containing the given value. *)
external (!) : 'a ref -> 'a = "%field0"
        (* [!r] returns the current contents of reference [r].
           Could be defined as [fun r -> r.contents]. *)
external (:=) : 'a ref -> 'a -> unit = "%setfield0"
        (* [r := a] stores the value of [a] in reference [r].
           Could be defined as [fun r v -> r.contents <- v]. *)
external incr : int ref -> unit = "%incr"
        (* Increment the integer contained in the given reference.
           Could be defined as [fun r -> r := succ !r]. *)
external decr : int ref -> unit = "%decr"
        (* Decrement the integer contained in the given reference.
           Could be defined as [fun r -> r := pred !r]. *)

(*** Program termination *)

val exit : int -> 'a
        (* Flush all pending writes on [stdout] and [stderr],
           and terminate the process, returning the given status code
           to the operating system (usually 0 to indicate no errors,
           and a small positive integer to indicate failure.) 
           An implicit [exit 0] is performed each time a program
           terminates normally (but not if it terminates because of
           an uncaught exception). *)

val at_exit: (unit -> unit) -> unit
        (* Register the given function to be called at program
           termination time. The functions registered with [at_exit]
           will be called in some unspecified order when the program
           executes [exit]. They will not be called if the program
           terminates because of an uncaught exception. *)

(*--*)

(*** For system use only, not for the casual user *)

val unsafe_really_input : in_channel -> string -> int -> int -> unit

val do_at_exit: unit -> unit
