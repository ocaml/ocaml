(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

module Make_options (F :
    sig
      val _a : unit -> unit
      val _c : unit -> unit
      val _cclib : string -> unit
      val _ccopt : string -> unit
      val _custom : unit -> unit
      val _g : unit -> unit
      val _i : unit -> unit
      val _I : string -> unit
      val _impl : string -> unit
      val _intf : string -> unit
      val _intf_suffix : string -> unit
      val _linkall : unit -> unit
      val _make_runtime : unit -> unit
      val _noassert : unit -> unit
      val _o : string -> unit
      val _output_obj : unit -> unit
      val _pp : string -> unit
      val _thread : unit -> unit
      val _unsafe : unit -> unit
      val _use_prims : string -> unit
      val _use_runtime : string -> unit
      val _v : unit -> unit
      val _verbose : unit -> unit
      val _nopervasives : unit -> unit
      val _drawlambda : unit -> unit
      val _dlambda : unit -> unit
      val _dinstr : unit -> unit
      val anonymous : string -> unit
    end) :
  sig
    val list : (string * Arg.spec * string) list
  end
