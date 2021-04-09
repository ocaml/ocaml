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
(* Per function environment for emit - common code for all targets. *)
type label = Linear.label

(* Record calls to caml_call_gc, emitted out of line. *)
type gc_call =
  { gc_lbl: label;                      (* Entry label *)
    gc_return_lbl: label;               (* Where to branch after GC *)
    gc_frame_lbl: label;                (* Label of frame descriptor *)
  }

(* Record calls to caml_ml_array_bound_error.
   In -g mode, or when using Spacetime profiling, we maintain one call to
   caml_ml_array_bound_error per bound check site.  Without -g, we can share
   a single call. *)

type bound_error_call =
  { bd_lbl: label;                      (* Entry label *)
    bd_frame: label;                    (* Label of frame descriptor *)
  }

(* Pending floating-point literals *)
type float_literal =
  {
    fl : int64;
    lbl : label;
  }

(* Pending large integer literals *)
type int_literal =
  {
    n : nativeint;
    n_lbl : label;
  }

(* Environment for emitting a function *)
type 'a per_function_env = {
  f : Linear.fundecl;
  mutable stack_offset : int;
  mutable call_gc_sites : gc_call list;
  mutable bound_error_sites : bound_error_call list;
  mutable bound_error_call : label option;
  mutable float_literals : float_literal list;
  mutable int_literals : int_literal list;
  p : 'a;
}
