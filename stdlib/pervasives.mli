(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** The initially opened module.

   This module provides the built-in types (numbers, booleans,
   strings, exceptions, references, lists, arrays, input-output channels, ...)
   and the basic operations over these types.

   This module is automatically opened at the beginning of each compilation.
   All components of this module can therefore be referred by their short
   name, without prefixing them by [Pervasives].
*)

(** {2 Predefined types} 
These are predefined types :
{[ type int ]}   The type of integer numbers.
{[ type char]}   The type of characters.
*)
(*
{- [type char ]

   The type of characters. }
{-  [type string]

   The type of character strings.}
{-  [type float]

   The type of floating-point numbers. }
{-  [type bool]

   The type of booleans (truth values).}
{-  [type unit = ()]

   The type of the unit value.}
{-  [type exn]

   The type of exception values.}
{-  [type 'a array] 

   The type of arrays whose elements have type ['a]. }
{-  [type 'a list = [] | :: of 'a * 'a list]

   The type of lists whose elements have type ['a].}
{-  [type 'a option = None | Some of 'a]

   The type of optional values. }
{-  [type ('a, 'b, 'c) format]

   The type of format strings. ['a] is the type of the parameters
   of the format, ['c] is the result type for the [printf]-style
   function, and ['b] is the type of the first argument given to
   [%a] and [%t] printing functions (see module {!Printf}). }}
*)

(** {2 Exceptions} *)

(** Raise the given exception value *)
external raise : exn -> 'a = "%raise"
        
(** These are predefined exceptions :
{ul
{- [exception Match_failure of (string * int * int)]

Exception raised when none of the cases of a pattern-matching
   apply. The arguments are the location of the pattern-matching
   in the source code (file name, position of first character,
   position of last character).
}
{- [exception Assert_failure of (string * int * int)]

Exception raised when an assertion fails.  The arguments are
   the location of the pattern-matching in the source code
   (file name, position of first character, position of last
   character).
}
{- [exception Invalid_argument of string]

Exception raised by library functions to signal that the given
   arguments do not make sense.
}
{- [exception Failure of string]

Exception raised by library functions to signal that they are
   undefined on the given arguments. 
}
{- [exception Not_found]

Exception raised by search functions when the desired object
   could not be found.
}
{- [exception Out_of_memory]

Exception raised by the garbage collector
   when there is insufficient memory to complete the computation.
}
{- [exception Stack_overflow]

Exception raised by the bytecode interpreter when the evaluation
   stack reaches its maximal size. This often indicates infinite
   or excessively deep recursion in the user's program.
}
{- [exception Sys_error of string]

Exception raised by the input/output functions to report
   an operating system error.
}
{- [exception End_of_file]

Exception raised by input functions to signal that the
   end of file has been reached.
}
{- [exception Division_by_zero]

Exception raised by division and remainder operations
   when their second argument is null.
}
{- [exception Sys_blocked_io]

A special case of [Sys_error] raised when no I/O is possible
   on a non-blocking I/O channel.
}}
*)

(** The [Exit] exception is not raised by any library function.  It is
    provided for use in your programs.*)
exception Exit

(** Raise exception [Invalid_argument] with the given string. *)
val invalid_arg: string -> 'a

(** Raise exception [Failure] with the given string. *)
val failwith: string -> 'a


(** {2 Comparisons} *)


(** [e1 = e2] tests for structural equality of [e1] and [e2].
   Mutable structures (e.g. references and arrays) are equal
   if and only if their current contents are structurally equal,
   even if the two mutable objects are not the same physical object.
   Equality between functional values raises [Invalid_argument].
   Equality between cyclic data structures may not terminate. *)
external (=) : 'a -> 'a -> bool = "%equal"

(** Negation of [Pervasives.=]. *)
external (<>) : 'a -> 'a -> bool = "%notequal"


(** See {!Pervasives.>=}. *)
external (<) : 'a -> 'a -> bool = "%lessthan"
(** See {!Pervasives.>=}. *)
external (>) : 'a -> 'a -> bool = "%greaterthan"
(** See {!Pervasives.>=}. *)
external (<=) : 'a -> 'a -> bool = "%lessequal"

(** Structural ordering functions. These functions coincide with
   the usual orderings over integers, characters, strings
   and floating-point numbers, and extend them to a
   total ordering over all types.
   The ordering is compatible with [(=)]. As in the case
   of [(=)], mutable structures are compared by contents.
   Comparison between functional values raises [Invalid_argument].
   Comparison between cyclic structures may not terminate. *)
external (>=) : 'a -> 'a -> bool = "%greaterequal"

(** [compare x y] returns [0] if [x=y], a negative integer if
   [x<y], and a positive integer if [x>y]. The same restrictions
   as for [=] apply. [compare] can be used as the comparison function
   required by the {!Set} and {!Map} modules. *)
external compare: 'a -> 'a -> int = "compare"

(** Return the smaller of the two arguments. *)
val min: 'a -> 'a -> 'a

(** Return the greater of the two arguments. *)
val max: 'a -> 'a -> 'a

(** [e1 == e2] tests for physical equality of [e1] and [e2].
   On integers and characters, it is the same as structural
   equality. On mutable structures, [e1 == e2] is true if and only if
   physical modification of [e1] also affects [e2].
   On non-mutable structures, the behavior of [(==)] is
   implementation-dependent, except that [e1 == e2] implies
   [e1 = e2]. *)
external (==) : 'a -> 'a -> bool = "%eq"

(** Negation of {!Pervasives.==}. *)
external (!=) : 'a -> 'a -> bool = "%noteq"


(** {2 Boolean operations} *)


(** The boolean negation. *)
external not : bool -> bool = "%boolnot"

(** See {!Pervasives.&}. *)
external (&&) : bool -> bool -> bool = "%sequand"

(** The boolean ``and''. Evaluation is sequential, left-to-right:
   in [e1 && e2], [e1] is evaluated first, and if it returns [false],
   [e2] is not evaluated at all. *)
external (&) : bool -> bool -> bool = "%sequand"


(** See {!Pervasives.or}.*)
external (||) : bool -> bool -> bool = "%sequor"

(** The boolean ``or''. Evaluation is sequential, left-to-right:
   in [e1 || e2], [e1] is evaluated first, and if it returns [true],
   [e2] is not evaluated at all. *)
external (or) : bool -> bool -> bool = "%sequor"


(** {2 Integer arithmetic} *)

(** Integers are 31 bits wide (or 63 bits on 64-bit processors).
   All operations are taken modulo 2{^31} (or 2{^63}).
   They do not fail on overflow. *)

(** Unary negation. You can also write [-e] instead of [~-e]. *)
external (~-) : int -> int = "%negint"

(** [succ x] is [x+1]. *)
external succ : int -> int = "%succint"

(** [pred x] is [x-1]. *)
external pred : int -> int = "%predint"

(** Integer addition. *)
external (+) : int -> int -> int = "%addint"

(** Integer subtraction. *)
external (-) : int -> int -> int = "%subint"

(** Integer multiplication. *)
external ( * ) : int -> int -> int = "%mulint"

(** Integer division.
   Raise [Division_by_zero] if the second argument is 0. *)
external (/) : int -> int -> int = "%divint"

(** Integer remainder.  If [x >= 0] and [y > 0], the result
   of [x mod y] satisfies the following properties:
   [0 <= x mod y < y] and
   [x = (x / y) * y + x mod y].
   If [y = 0], [x mod y] raises [Division_by_zero].
   If [x < 0] or [y < 0], the result of [x mod y] is
   not specified and depends on the platform. *)
external (mod) : int -> int -> int = "%modint"

(** Return the absolute value of the argument. *)
val abs : int -> int

(** The greatest representable integer. *)
val max_int: int
(** The smallest representable integer. *)
val min_int: int



(** {3 Bitwise operations} *)


(** Bitwise logical and. *)
external (land) : int -> int -> int = "%andint"

(** Bitwise logical or. *)
external (lor) : int -> int -> int = "%orint"

(** Bitwise logical exclusive or. *)
external (lxor) : int -> int -> int = "%xorint"

(** Bitwise logical negation. *)
val lnot: int -> int

(** [n lsl m] shifts [n] to the left by [m] bits.
   The result is unspecified if [m < 0] or [m >= bitsize],
   where [bitsize] is [32] on a 32-bit platform and
   [64] on a 64-bit platform. *)
external (lsl) : int -> int -> int = "%lslint"

(** [n lsr m] shifts [n] to the right by [m] bits.
   This is a logical shift: zeroes are inserted regardless of
   the sign of [n].
   The result is unspecified if [m < 0] or [m >= bitsize]. *)
external (lsr) : int -> int -> int = "%lsrint"

(** [n asr m] shifts [n] to the right by [m] bits.
   This is an arithmetic shift: the sign bit of [n] is replicated.
   The result is unspecified if [m < 0] or [m >= bitsize]. *)
external (asr) : int -> int -> int = "%asrint"
    

(** {2 Floating-point arithmetic}

   Caml's floating-point numbers follow the
   IEEE 754 standard, using double precision (64 bits) numbers.
   Floating-point operations never raise an exception on overflow,
   underflow, division by zero, etc.  Instead, special IEEE numbers
   are returned as appropriate, such as [infinity] for [1.0 /. 0.0],
   [neg_infinity] for [-1.0 /. 0.0], and [nan] (``not a number'')
   for [0.0 /. 0.0].  These special numbers then propagate through
   floating-point computations as expected: for instance,
   [1.0 /. infinity] is [0.0], and any operation with [nan] as 
   argument returns [nan] as result. 
*)

(** Unary negation. You can also write [-.e] instead of [~-.e]. *)
external (~-.) : float -> float = "%negfloat"

(** Floating-point addition *)
external (+.) : float -> float -> float = "%addfloat"

(** Floating-point subtraction *)
external (-.) : float -> float -> float = "%subfloat"

(** Floating-point multiplication *)
external ( *. ) : float -> float -> float = "%mulfloat"

(** Floating-point division. *)
external (/.) : float -> float -> float = "%divfloat"

(** Exponentiation *)
external ( ** ) : float -> float -> float = "power_float" "pow" "float"

(** Square root *)
external sqrt : float -> float = "sqrt_float" "sqrt" "float"

(** Exponential. *)
external exp : float -> float = "exp_float" "exp" "float"
(** Natural logarithm. *)
external log : float -> float = "log_float" "log" "float"
(** Base 10 logarithm. *)
external log10 : float -> float = "log10_float" "log10" "float"

external cos : float -> float = "cos_float" "cos" "float"
(** See {!Pervasives.atan2}. *)
external sin : float -> float = "sin_float" "sin" "float"
(** See {!Pervasives.atan2}. *)
external tan : float -> float = "tan_float" "tan" "float"
(** See {!Pervasives.atan2}. *)
external acos : float -> float = "acos_float" "acos" "float"
(** See {!Pervasives.atan2}. *)
external asin : float -> float = "asin_float" "asin" "float"
(** See {!Pervasives.atan2}. *)
external atan : float -> float = "atan_float" "atan" "float"
(** The usual trigonometric functions. *)
external atan2 : float -> float -> float = "atan2_float" "atan2" "float"

(** See {!Pervasives.tanh}. *)
external cosh : float -> float = "cosh_float" "cosh" "float"
(** See {!Pervasives.tanh}. *)
external sinh : float -> float = "sinh_float" "sinh" "float"
(** The usual hyperbolic trigonometric functions. *)
external tanh : float -> float = "tanh_float" "tanh" "float"

(** See {!Pervasives.floor}. *)
external ceil : float -> float = "ceil_float" "ceil" "float"
(** Round the given float to an integer value.
   [floor f] returns the greatest integer value less than or
   equal to [f].
   [ceil f] returns the least integer value greater than or
   equal to [f]. *)
external floor : float -> float = "floor_float" "floor" "float"

(** Return the absolute value of the argument. *)
external abs_float : float -> float = "%absfloat"

(** [mod_float a b] returns the remainder of [a] with respect to
   [b].  The returned value is [a -. n *. b], where [n]
   is the quotient [a /. b] rounded towards zero to an integer. *)
external mod_float : float -> float -> float = "fmod_float" "fmod" "float"

(** [frexp f] returns the pair of the significant
   and the exponent of [f].  When [f] is zero, the
   significant [x] and the exponent [n] of [f] are equal to
   zero.  When [f] is non-zero, they are defined by
   [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)
external frexp : float -> float * int = "frexp_float"

(** [ldexp x n] returns [x *. 2 ** n]. *)
external ldexp : float -> int -> float = "ldexp_float"

(** [modf f] returns the pair of the fractional and integral
   part of [f]. *)
external modf : float -> float * float = "modf_float"

(** Same as {!Pervasives.float_of_int}. *)
external float : int -> float = "%floatofint"
(** Convert an integer to floating-point. *)
external float_of_int : int -> float = "%floatofint"

(** Same as {!Pervasives.int_of_float}. *)
external truncate : float -> int = "%intoffloat"
(** Truncate the given floating-point number to an integer.
   The result is unspecified if it falls outside the
   range of representable integers. *)
external int_of_float : float -> int = "%intoffloat"

(** Positive infinity. *)
val infinity: float
(** Negative infinity. *)
val neg_infinity: float

(** A special floating-point value denoting the result of an
   undefined operation such as [0.0 /. 0.0].  Stands for
   ``not a number''. *)
val nan: float

(** The five classes of floating-point numbers, as determined by
   the {!Pervasives.classify_float} function. *)
type fpclass =
    FP_normal           (** Normal number, none of the below *)
  | FP_subnormal        (** Number very close to 0.0, has reduced precision *)
  | FP_zero             (** Number is 0.0 or -0.0 *)
  | FP_infinite         (** Number is positive or negative infinity *)
  | FP_nan              (** Not a number: result of an undefined operation *)

(** Return the class of the given floating-point number:
   normal, subnormal, zero, infinite, or not a number. *)
external classify_float: float -> fpclass = "classify_float"


(** {2 String operations}

   More string operations are provided in module {!String}.
*)

(** String concatenation. *)
val (^) : string -> string -> string


(** {2 Character operations}

   More character operations are provided in module {!Char}.
*)

(** Return the ASCII code of the argument. *)
external int_of_char : char -> int = "%identity"

(** Return the character with the given ASCII code.
   Raise [Invalid_argument "char_of_int"] if the argument is
   outside the range 0--255. *)
val char_of_int : int -> char


(** {2 Unit operations} *)


(** Discard the value of its argument and return [()].
   For instance, [ignore(f x)] discards the result of
   the side-effecting function [f].  It is equivalent to
   [f x; ()], except that the latter may generate a
   compiler warning; writing [ignore(f x)] instead
   avoids the warning. *)
external ignore : 'a -> unit = "%ignore"


(** {2 String conversion functions} *)

(** Return the string representation of a boolean. *)
val string_of_bool : bool -> string

(** Convert the given string to a boolean.
   Raise [Invalid_argument "bool_of_string"] if the string is not
   ["true"] or ["false"]. *)
val bool_of_string : string -> bool

(** Return the string representation of an integer, in decimal. *)
val string_of_int : int -> string

(** Convert the given string to an integer.
   The string is read in decimal (by default) or in hexadecimal,
   octal or binary if the string begins with [0x], [0o] or [0b]
   respectively.
   Raise [Failure "int_of_string"] if the given string is not
   a valid representation of an integer. *)
external int_of_string : string -> int = "int_of_string"

(** Return the string representation of a floating-point number. *)
val string_of_float : float -> string

(** Convert the given string to a float.
   The result is unspecified if the given string is not
   a valid representation of a float. *)
external float_of_string : string -> float = "float_of_string"



(** {2 Pair operations} *)


(** Return the first component of a pair. *)
external fst : 'a * 'b -> 'a = "%field0"
(** Return the second component of a pair. *)
external snd : 'a * 'b -> 'b = "%field1"


(** {2 List operations}

   More list operations are provided in module {!List}. 
*)

(** List concatenation. *)
val (@) : 'a list -> 'a list -> 'a list


(** {2 Input/output} *)

(** The type of input channel. *)
type in_channel
(** The type of output channel. *)
type out_channel

(** The standard input for the process. *)
val stdin : in_channel
(** The standard output for the process. *)
val stdout : out_channel
(** The standard error ouput for the process. *)
val stderr : out_channel


(** {3 Output functions on standard output} *)

(** Print a character on standard output. *)
val print_char : char -> unit

(** Print a string on standard output. *)
val print_string : string -> unit

(** Print an integer, in decimal, on standard output. *)
val print_int : int -> unit

(** Print a floating-point number, in decimal, on standard output. *)
val print_float : float -> unit

(** Print a string, followed by a newline character, on
   standard output. *)
val print_endline : string -> unit

(** Print a newline character on standard output, and flush
   standard output. This can be used to simulate line
   buffering of standard output. *)
val print_newline : unit -> unit


(** {3 Output functions on standard error} *)

(** Print a character on standard error. *)
val prerr_char : char -> unit

(** Print a string on standard error. *)
val prerr_string : string -> unit

(** Print an integer, in decimal, on standard error. *)
val prerr_int : int -> unit

(** Print a floating-point number, in decimal, on standard error. *)
val prerr_float : float -> unit

(** Print a string, followed by a newline character on standard error
   and flush standard error. *)
val prerr_endline : string -> unit

(** Print a newline character on standard error, and flush
   standard error. *)
val prerr_newline : unit -> unit


(** {3 Input functions on standard input} *)

(** Flush standard output, then read characters from standard input
   until a newline character is encountered. Return the string of
   all characters read, without the newline character at the end. *)
val read_line : unit -> string

(** Flush standard output, then read one line from standard input
   and convert it to an integer. Raise [Failure "int_of_string"]
   if the line read is not a valid representation of an integer. *)
val read_int : unit -> int

(** Flush standard output, then read one line from standard input
   and convert it to a floating-point number.
   The result is unspecified if the line read is not a valid
   representation of a floating-point number. *)
val read_float : unit -> float

(** {3 General output functions} *)


(** Opening modes for {!Pervasives.open_out_gen} and {!Pervasives.open_in_gen}. *)
type open_flag =
    Open_rdonly      (** open for reading. *)
  | Open_wronly      (** open for writing. *)
  | Open_append      (** open for appending. always write at end of file (needs [Open_wronly]. *)
  | Open_creat       (** create the file if it does not exist. *)
  | Open_trunc       (** empty the file if it already exists. *)
  | Open_excl        (** fail if the file already exists. *)
  | Open_binary      (** open in binary mode (no conversion). *)
  | Open_text        (** open in text mode (may perform conversions). *)
  | Open_nonblock    (** open in non-blocking mode. *)
           
(** Open the named file for writing, and return a new output channel
   on that file, positionned at the beginning of the file. The
   file is truncated to zero length if it already exists. It
   is created if it does not already exists.
   Raise [Sys_error] if the file could not be opened. *)
val open_out : string -> out_channel

(** Same as {!Pervasives.open_out}, but the file is opened in binary mode,
   so that no translation takes place during writes. On operating
   systems that do not distinguish between text mode and binary
   mode, this function behaves like {!Pervasives.open_out}. *)
val open_out_bin : string -> out_channel

(** Open the named file for writing, as above. The extra argument [mode]
   specify the opening mode. The extra argument [perm] specifies
   the file permissions, in case the file must be created.
   {!Pervasives.open_out} and {!Pervasives.open_out_bin} are special
   cases of this function. *)
val open_out_gen : open_flag list -> int -> string -> out_channel

(** Flush the buffer associated with the given output channel, 
   performing all pending writes on that channel.
   Interactive programs must be careful about flushing standard
   output and standard error at the right time. *)
val flush : out_channel -> unit

(** Flush all opened output channels. *)
val flush_all : unit -> unit

(** Write the character on the given output channel. *)
val output_char : out_channel -> char -> unit

(** Write the string on the given output channel. *)
val output_string : out_channel -> string -> unit

(** Write [len] characters from string [buf], starting at offset
   [pos], to the given output channel.
   Raise [Invalid_argument "output"] if [pos] and [len] do not
   designate a valid substring of [buf]. *)
val output : out_channel -> string -> int -> int -> unit

(** Write one 8-bit integer (as the single character with that code)
   on the given output channel. The given integer is taken modulo
   256. *)
val output_byte : out_channel -> int -> unit

(** Write one integer in binary format on the given output channel.
   The only reliable way to read it back is through the
   {!Pervasives.input_binary_int} function. The format is compatible across
   all machines for a given version of Objective Caml. *)
val output_binary_int : out_channel -> int -> unit

(** Write the representation of a structured value of any type
   to a channel. Circularities and sharing inside the value
   are detected and preserved. The object can be read back,
   by the function {!Pervasives.input_value}. See the description of module
   {!Marshal} for more information. {!Pervasives.output_value} is equivalent
   to {!Marshal.to_channel} with an empty list of flags. *)
val output_value : out_channel -> 'a -> unit

(** [seek_out chan pos] sets the current writing position to [pos]
   for channel [chan]. This works only for regular files. On
   files of other kinds (such as terminals, pipes and sockets),
   the behavior is unspecified. *)
val seek_out : out_channel -> int -> unit

(** Return the current writing position for the given channel. *)
val pos_out : out_channel -> int

(** Return the total length (number of characters) of the
   given channel.  This works only for regular files. On files of
   other kinds, the result is meaningless. *)
val out_channel_length : out_channel -> int

(** Close the given channel, flushing all buffered write operations.
   A [Sys_error] exception is raised if any of the functions above
   is called on a closed channel. *)
val close_out : out_channel -> unit

(** [set_binary_mode_out oc true] sets the channel [oc] to binary
   mode: no translations take place during output.
   [set_binary_mode_out oc false] sets the channel [oc] to text
   mode: depending on the operating system, some translations
   may take place during output.  For instance, under Windows,
   end-of-lines will be translated from [\n] to [\r\n].
   This function has no effect under operating systems that
   do not distinguish between text mode and binary mode. *)
val set_binary_mode_out : out_channel -> bool -> unit


(** {3 General input functions} *)

(** Open the named file for reading, and return a new input channel
   on that file, positionned at the beginning of the file.
   Raise [Sys_error] if the file could not be opened. *)
val open_in : string -> in_channel

(** Same as {!Pervasives.open_in}, but the file is opened in binary mode,
   so that no translation takes place during reads. On operating
   systems that do not distinguish between text mode and binary
   mode, this function behaves like {!Pervasives.open_in}. *)
val open_in_bin : string -> in_channel

(** Open the named file for reading, as above. The extra arguments
   [mode] and [perm] specify the opening mode and file permissions.
   {!Pervasives.open_in} and {!Pervasives.open_in_bin} are special
   cases of this function. *)
val open_in_gen : open_flag list -> int -> string -> in_channel

(** Read one character from the given input channel.
   Raise [End_of_file] if there are no more characters to read. *)
val input_char : in_channel -> char

(** Read characters from the given input channel, until a
   newline character is encountered. Return the string of
   all characters read, without the newline character at the end.
   Raise [End_of_file] if the end of the file is reached
   at the beginning of line. *)
val input_line : in_channel -> string

(** Read up to [len] characters from the given channel,
   storing them in string [buf], starting at character number [pos].
   It returns the actual number of characters read, between 0 and
   [len] (inclusive).
   A return value of 0 means that the end of file was reached.
   A return value between 0 and [len] exclusive means that
   not all requested [len] characters were read, either because
   no more characters were available at that time, or because
   the implementation found it convenient to do a partial read;
   [input] must be called again to read the remaining characters,
   if desired.  (See also {!Pervasives.really_input} for reading
   exactly [len] characters.)
   Exception [Invalid_argument "input"] is raised if [pos] and [len]
   do not designate a valid substring of [buf]. *)          
val input : in_channel -> string -> int -> int -> int

(** Read [len] characters from the given channel, storing them in
   string [buf], starting at character number [pos].
   Raise [End_of_file] if the end of file is reached before [len]
   characters have been read.
   Raise [Invalid_argument "really_input"] if
   [pos] and [len] do not designate a valid substring of [buf]. *)
val really_input : in_channel -> string -> int -> int -> unit

(** Same as {!Pervasives.input_char}, but return the 8-bit integer representing
   the character.
   Raise [End_of_file] if an end of file was reached. *)
val input_byte : in_channel -> int

(** Read an integer encoded in binary format from the given input
   channel. See {!Pervasives.output_binary_int}.
   Raise [End_of_file] if an end of file was reached while reading the
   integer. *)
val input_binary_int : in_channel -> int

(** Read the representation of a structured value, as produced
   by {!Pervasives.output_value}, and return the corresponding value.
   This function is identical to {!Marshal.from_channel};
   see the description of module {!Marshal} for more information,
   in particular concerning the lack of type safety. *)
val input_value : in_channel -> 'a

(** [seek_in chan pos] sets the current reading position to [pos]
   for channel [chan]. This works only for regular files. On
   files of other kinds, the behavior is unspecified. *)
val seek_in : in_channel -> int -> unit

(** Return the current reading position for the given channel. *)
val pos_in : in_channel -> int

(** Return the total length (number of characters) of the
   given channel. This works only for regular files. On files of
   other kinds, the result is meaningless. *)
val in_channel_length : in_channel -> int

(** Close the given channel.  A [Sys_error] exception is raised
   if any of the functions above is called on a closed channel. *)
val close_in : in_channel -> unit

(** [set_binary_mode_in ic true] sets the channel [ic] to binary
   mode: no translations take place during input.
   [set_binary_mode_out ic false] sets the channel [ic] to text
   mode: depending on the operating system, some translations
   may take place during input.  For instance, under Windows,
   end-of-lines will be translated from [\r\n] to [\n].
   This function has no effect under operating systems that
   do not distinguish between text mode and binary mode. *)
val set_binary_mode_in : in_channel -> bool -> unit


(** {2 References} *)


(** The type of references (mutable indirection cells) containing
   a value of type ['a]. *)
type 'a ref = { mutable contents: 'a }

(** Return a fresh reference containing the given value. *)
external ref : 'a -> 'a ref = "%makemutable"

(** [!r] returns the current contents of reference [r].
   Equivalent to [fun r -> r.contents]. *)
external (!) : 'a ref -> 'a = "%field0"

(** [r := a] stores the value of [a] in reference [r].
   Equivalent to [fun r v -> r.contents <- v]. *)
external (:=) : 'a ref -> 'a -> unit = "%setfield0"

(** Increment the integer contained in the given reference.
   Equivalent to [fun r -> r := succ !r]. *)
external incr : int ref -> unit = "%incr"

(** Decrement the integer contained in the given reference.
   Equivalent to [fun r -> r := pred !r]. *)
external decr : int ref -> unit = "%decr"


(** {2 Program termination} *)


(** Flush all pending writes on {!Pervasives.stdout} and
   {!Pervasives.stderr},
   and terminate the process, returning the given status code
   to the operating system (usually 0 to indicate no errors,
   and a small positive integer to indicate failure.) 
   An implicit [exit 0] is performed each time a program
   terminates normally (but not if it terminates because of
   an uncaught exception). *)
val exit : int -> 'a

(** Register the given function to be called at program
   termination time. The functions registered with [at_exit]
   will be called when the program executes {!Pervasives.exit}. 
   They will not be called if the program
   terminates because of an uncaught exception.
   The functions are called in ``last in, first out'' order:
   the function most recently added with [at_exit] is called first. *)
val at_exit: (unit -> unit) -> unit


(*--*)

(** {2 For system use only, not for the casual user} *)

val unsafe_really_input : in_channel -> string -> int -> int -> unit

val do_at_exit: unit -> unit
