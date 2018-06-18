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

open Cmm

(** [bind name arg fn] is equivalent to [let name = arg in fn name],
    or simply [fn arg] if [arg] is simple enough *)
val bind :
  string -> expression -> (expression -> expression) -> expression

(** Same as [bind], but also treats loads from a variable as simple *)
val bind_load :
  string -> expression -> (expression -> expression) -> expression

(** Same as [bind], but does not treat variables as simple *)
val bind_nonvar :
  string -> expression -> (expression -> expression) -> expression

(** Headers *)

(** A null header with GC bits set to black *)
val caml_black : nativeint

(** A constant equal to the tag for float arrays *)
val floatarray_tag : Debuginfo.t -> expression

(** [block_header tag size] creates a header with tag [tag] for a
    block of size [size] *)
val block_header : int -> int -> nativeint

(** Same as block_header, but with GC bits set to black *)
val black_block_header : int -> int -> nativeint

(** Closure headers of the given size *)
val white_closure_header : int -> nativeint
val black_closure_header : int -> nativeint

(** Infix header at the given offset *)
val infix_header : int -> nativeint

(** Header for a boxed float value *)
val float_header : nativeint

(** Header for an unboxed float array of the given size *)
val floatarray_header : int -> nativeint

(** Header for a string (or bytes) of the given length *)
val string_header : int -> nativeint

(** Boxed integers headers *)
val boxedint32_header : nativeint
val boxedint64_header : nativeint
val boxedintnat_header : nativeint

(** Wrappers *)
val alloc_float_header : Debuginfo.t -> expression
val alloc_floatarray_header : int -> Debuginfo.t -> expression
val alloc_closure_header : int -> Debuginfo.t -> expression
val alloc_infix_header : int -> Debuginfo.t -> expression
val alloc_boxedint32_header : Debuginfo.t -> expression
val alloc_boxedint64_header : Debuginfo.t -> expression
val alloc_boxedintnat_header : Debuginfo.t -> expression

(** Integers *)

(** Minimal/maximal OCaml integer values whose backend representation fits
    in a regular OCaml integer *)
val max_repr_int : int
val min_repr_int : int

(** Make an integer constant from the given integer (tags the integer) *)
val int_const : Debuginfo.t -> int -> expression
val cint_const : int -> data_item
val targetint_const : int -> Targetint.t

(** Make a Cmm constant holding the given nativeint value.
    Uses [Cconst_int] instead of [Cconst_nativeint] when possible
    to preserve peephole optimisations. *)
val natint_const_untagged : Debuginfo.t -> Nativeint.t -> expression

(** Add an integer to the given expression *)
val add_const : expression -> int -> Debuginfo.t -> expression

(** Increment/decrement of integers *)
val incr_int : expression -> Debuginfo.t -> expression
val decr_int : expression -> Debuginfo.t -> expression

(** Simplify the given expression knowing its last bit will be
    irrelevant *)
val ignore_low_bit_int : expression -> expression

(** Arithmetical operations on integers *)
val add_int : expression -> expression -> Debuginfo.t -> expression
val sub_int : expression -> expression -> Debuginfo.t -> expression
val lsl_int : expression -> expression -> Debuginfo.t -> expression
val mul_int : expression -> expression -> Debuginfo.t -> expression
val lsr_int : expression -> expression -> Debuginfo.t -> expression
val asr_int : expression -> expression -> Debuginfo.t -> expression
val div_int :
  expression -> expression -> Lambda.is_safe -> Debuginfo.t -> expression
val mod_int :
  expression -> expression -> Lambda.is_safe -> Debuginfo.t -> expression

(** Integer tagging
    [tag_int] and [force_tag_int] are functionnaly equivalent, but
    produce syntactically different expressions ([tag_int] produces
    an addition, while [force_tag_int] produces a logical or).
    The difference marks the fact that the shift operation in [tag_int]
    is assumed not to overflow, and so [untag_int (tag_int i)] can be
    simplified to [i]. With [force_tag_int], the initial shift might
    overflow, so the above simplification would be wrong. *)
val tag_int : expression -> Debuginfo.t -> expression
val force_tag_int : expression -> Debuginfo.t -> expression

(** Integer untagging *)
val untag_int : expression -> Debuginfo.t -> expression

(** Specific division operations for boxed integers *)
val safe_div_bi :
  Lambda.is_safe ->
  expression ->
  expression ->
  Primitive.boxed_integer ->
  Debuginfo.t ->
  expression
val safe_mod_bi :
  Lambda.is_safe ->
  expression ->
  expression ->
  Primitive.boxed_integer ->
  Debuginfo.t ->
  expression

(** If-Then-Else expression
    [mk_if_then_else dbg cond ifso_dbg ifso ifnot_dbg ifnot] associates
    [dbg] to the global if-then-else expression, [ifso_dbg] to the
    then branch [ifso], and [ifnot_dbg] to the else branch [ifnot] *)
val mk_if_then_else :
  Debuginfo.t ->
  expression ->
  Debuginfo.t -> expression ->
  Debuginfo.t -> expression ->
  expression

(** Boolean negation *)
val mk_not : Debuginfo.t -> expression -> expression

(** Exception raising *)
val raise_regular : Debuginfo.t -> expression -> expression
val raise_symbol : Debuginfo.t -> string -> expression

(** Convert a tagged integer into a raw integer with boolean meaning *)
val test_bool : Debuginfo.t -> expression -> expression

(** Float boxing and unboxing *)
val box_float : Debuginfo.t -> expression -> expression
val unbox_float : Debuginfo.t -> expression -> expression

(** Map the given function over a Ccatch expression's handlers and body *)
val map_ccatch :
  (expression -> expression) -> rec_flag ->
  (int * (Backend_var.With_provenance.t * machtype) list * expression
   * Debuginfo.t) list ->
  expression -> expression

(** Complex number creation and access *)
val box_complex : Debuginfo.t -> expression -> expression -> expression
val complex_re : expression -> Debuginfo.t -> expression
val complex_im : expression -> Debuginfo.t -> expression

(** Make the given expression return a unit value *)
val return_unit : Debuginfo.t -> expression -> expression

(** Remove a trailing unit return if any *)
val remove_unit : expression -> expression

(** Blocks *)

(** [field_address ptr n dbg] returns an expression for the address of the
    [n]th field of the block pointed to by [ptr] *)
val field_address : expression -> int -> Debuginfo.t -> expression

(** [mk_get_field mut ptr n dbg] returns an expression for the access to the
    [n]th field of the block pointed to by [ptr] *)
val mk_get_field :
  Asttypes.mutable_flag -> expression -> int -> Debuginfo.t -> expression

(** [set_field ptr n newval init dbg] returns an expression for setting the
    [n]th field of the block pointed to by [ptr] to [newval] *)
val set_field :
  expression -> int -> expression -> Lambda.initialization_or_assignment ->
  Debuginfo.t -> expression

(** Load a block's header *)
val get_header : expression -> Debuginfo.t -> expression

(** Same as [get_header], but also set all profiling bits of the header
    are to 0 (if profiling is enabled) *)
val get_header_without_profinfo : expression -> Debuginfo.t -> expression

(** Load a block's tag *)
val get_tag : expression -> Debuginfo.t -> expression

(** Load a block's size *)
val get_size : expression -> Debuginfo.t -> expression

(** Arrays *)

(* TODO: remove from mli? *)
val log2_size_addr : int
val log2_size_float : int

val wordsize_shift : int
val numfloat_shift : int

(** Check whether the given array is an array of regular OCaml values
    (as opposed to unboxed floats), from its header or pointer *)
val is_addr_array_hdr : expression -> Debuginfo.t -> expression
val is_addr_array_ptr : expression -> Debuginfo.t -> expression

(** Get the length of an array from its header *)
val addr_array_length : expression -> Debuginfo.t -> expression
val float_array_length : expression -> Debuginfo.t -> expression

(* TODO: remove after objects have been moved *)
val lsl_const : expression -> int -> Debuginfo.t -> expression

(* TODO: consider removing from mli ? *)
(** From the implementation of [array_indexing ?typ log2size ptr ofs dbg] :
   Produces a pointer to the element of the array [ptr] on the position [ofs]
   with the given element [log2size] log2 element size. [ofs] is given as a
   tagged int expression.
   The optional ?typ argument is the C-- type of the result.
   By default, it is Addr, meaning we are constructing a derived pointer
   into the heap.  If we know the pointer is outside the heap
   (this is the case for bigarray indexing), we give type Int instead. *)
val array_indexing :
  ?typ:machtype_component -> int -> expression -> expression -> Debuginfo.t ->
  expression

(** Array loads and stores
    [unboxed_float_array_ref] and [float_array_ref] differ in the
    boxing of the result; [float_array_set] takes an unboxed float *)
val addr_array_ref : expression -> expression -> Debuginfo.t -> expression
val int_array_ref : expression -> expression -> Debuginfo.t -> expression
val unboxed_float_array_ref :
  expression -> expression -> Debuginfo.t -> expression
val float_array_ref : expression -> expression -> Debuginfo.t -> expression
val addr_array_set :
  expression -> expression -> expression -> Debuginfo.t -> expression
val addr_array_initialize :
  expression -> expression -> expression -> Debuginfo.t -> expression
val int_array_set :
  expression -> expression -> expression -> Debuginfo.t -> expression
val float_array_set :
  expression -> expression -> expression -> Debuginfo.t -> expression


(** Strings *)

val string_length : expression -> Debuginfo.t -> expression
val bigstring_length : expression -> Debuginfo.t -> expression

(** Objects *)

(** Lookup a method by its hash, using [caml_get_public_method]
    Arguments :
    - obj : the object from which to lookup
    - tag : the hash of the method name, as a tagged integer *)
val lookup_tag : expression -> expression -> Debuginfo.t -> expression

(** Lookup a method by its offset in the method table
    Arguments :
    - obj : the object from which to lookup
    - lab : the position of the required method in the object's
    method array, as a tagged integer *)
val lookup_label : expression -> expression -> Debuginfo.t -> expression

(** Lookup and call a method using the method cache
    Arguments :
    - obj : the object from which to lookup
    - tag : the hash of the method name, as a tagged integer
    - cache : the method cache array
    - pos : the position of the cache entry in the cache array
    - args : the additional arguments to the method call *)
val call_cached_method :
  expression -> expression -> expression -> expression -> expression list ->
  Debuginfo.t -> expression

(** Allocations *)

(** Allocate a block of regular values with the given tag *)
val make_alloc : Debuginfo.t -> int -> expression list -> expression

(** Allocate a block of unboxed floats with the given tag *)
val make_float_alloc : Debuginfo.t -> int -> expression list -> expression

(** Bounds checking *)

(** Generate a [Ccheckbound] term
    [Ccheckbounds] takes two arguments : first the bound to check against,
    then the index.
    It results in a bounds error if the index is greater than or equal to
    the bound. *)
val make_checkbound : Debuginfo.t -> expression list -> expression

(** [check_bound safety access_size dbg length a2 k] prefixes expression [k]
    with a check that reading [access_size] bits starting at position [a2]
    in a string/bytes value of length [length] is within bounds, unless
    [safety] is [Unsafe]. *)
val check_bound :
  Lambda.is_safe -> Clambda_primitives.memory_access_size -> Debuginfo.t ->
  expression -> expression -> expression ->
  expression

(** Generic application functions *)

(** Get the symbol for the generic application with [n] arguments, and
    ensure its presence in the set of defined symbols *)
val apply_function : int -> string

(** If [n] is positive, get the symbol for the generic curryfication with
    [n] arguments, and ensure its presence in the set of defined symbols.
    Otherwise, do the same for the generic tuplification with [-n] arguments. *)
val curry_function : int -> string

(** Bigarrays *)

(** [bigarray_get unsafe kind layout b args dbg]
    - unsafe : if true, do not insert bound checks
    - kind : see [Lambda.bigarray_kind]
    - layout : see [Lambda.bigarray_layout]
    - b : the bigarray to load from
    - args : a list of tagged integer expressions, corresponding to the
    indices in the respective dimensions
    - dbg : debugging information *)
val bigarray_get :
  bool -> Lambda.bigarray_kind -> Lambda.bigarray_layout ->
  expression -> expression list -> Debuginfo.t ->
  expression

(** [bigarray_set unsafe kind layout b args newval dbg]
    Same as [bigarray_get], with [newval] the value being assigned *)
val bigarray_set :
  bool -> Lambda.bigarray_kind -> Lambda.bigarray_layout ->
  expression -> expression list -> expression -> Debuginfo.t ->
  expression

(** Boxed numbers *)

(** Global symbols for the ops field of boxed integers *)
val caml_nativeint_ops : string
val caml_int32_ops : string
val caml_int64_ops : string

(** Box a given integer into a block, without sharing of constants *)
val box_int_gen :
  Debuginfo.t -> Primitive.boxed_integer -> expression -> expression

(** Unbox a given boxed integer *)
val unbox_int :
  Primitive.boxed_integer -> expression -> Debuginfo.t -> expression

(** Used to prepare 32-bit integers on 64-bit platforms for a lsr operation *)
val make_unsigned_int :
  Primitive.boxed_integer -> expression -> Debuginfo.t -> expression

val unaligned_load_16 : expression -> expression -> Debuginfo.t -> expression
val unaligned_set_16 :
  expression -> expression -> expression -> Debuginfo.t -> expression
val unaligned_load_32 : expression -> expression -> Debuginfo.t -> expression
val unaligned_set_32 :
  expression -> expression -> expression -> Debuginfo.t -> expression
val unaligned_load_64 : expression -> expression -> Debuginfo.t -> expression
val unaligned_set_64 :
  expression -> expression -> expression -> Debuginfo.t -> expression

(** Raw memory accesses *)

(** [unaligned_set size ptr idx newval dbg] *)
val unaligned_set :
  Clambda_primitives.memory_access_size ->
  expression -> expression -> expression -> Debuginfo.t -> expression

(** [unaligned_load size ptr idx dbg] *)
val unaligned_load :
  Clambda_primitives.memory_access_size ->
  expression -> expression -> Debuginfo.t -> expression

(** [box_sized size dbg exp] *)
val box_sized :
  Clambda_primitives.memory_access_size ->
  Debuginfo.t -> expression -> expression

(** Primitives *)

val simplif_primitive :
  Clambda_primitives.primitive -> Clambda_primitives.primitive
