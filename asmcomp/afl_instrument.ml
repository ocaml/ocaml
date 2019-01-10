(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                                                                        *)
(*   Copyright 2016 Stephen Dolan.                                        *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Insert instrumentation for afl-fuzz *)

open Lambda
open Cmm

module V = Backend_var
module VP = Backend_var.With_provenance

let afl_area_ptr =
  Cconst_symbol (Backend_sym.Names.caml_afl_area_ptr, Debuginfo.none)

let afl_prev_loc =
  Cconst_symbol (Backend_sym.Names.caml_afl_prev_loc, Debuginfo.none)

let afl_map_size = 1 lsl 16

let cconst_int i = Cconst_int (i, Debuginfo.none)

let rec with_afl_logging b =
  if !Clflags.afl_inst_ratio < 100 &&
    Random.int 100 >= !Clflags.afl_inst_ratio then instrument b else
  let instrumentation =
    (* The instrumentation that afl-fuzz requires is:

         cur_location = <COMPILE_TIME_RANDOM>;
         shared_mem[cur_location ^ prev_location]++;
         prev_location = cur_location >> 1;

       See http://lcamtuf.coredump.cx/afl/technical_details.txt or
       docs/technical_details.txt in afl-fuzz source for for a full
       description of what's going on. *)
    let cur_location = Random.int afl_map_size in
    let cur_pos = V.create_local "pos" in
    let afl_area = V.create_local "shared_mem" in
    let op oper args = Cop (oper, args, Debuginfo.none) in
    Clet(VP.create afl_area,
      op (Cload (Word_int, Asttypes.Mutable)) [afl_area_ptr],
      Clet(VP.create cur_pos, op Cxor [op (Cload (Word_int, Asttypes.Mutable))
        [afl_prev_loc]; cconst_int cur_location],
      Csequence(
        op (Cstore(Byte_unsigned, Assignment))
          [op Cadda [Cvar afl_area; Cvar cur_pos];
            op Cadda [op (Cload (Byte_unsigned, Asttypes.Mutable))
                        [op Cadda [Cvar afl_area; Cvar cur_pos]];
                      cconst_int 1]],
        op (Cstore(Word_int, Assignment))
          [afl_prev_loc; cconst_int (cur_location lsr 1)]))) in
  Csequence(instrumentation, instrument b)

and instrument = function
  (* these cases add logging, as they may be targets of conditional branches *)
  | Cifthenelse (cond, ifso_dbg, t, ifnot_dbg, f, dbg) ->
     Cifthenelse (instrument cond,
       ifso_dbg, with_afl_logging t,
       ifnot_dbg, with_afl_logging f,
       dbg)
  | Cloop (e, dbg) ->
     Cloop (with_afl_logging e, dbg)
  | Ctrywith (e, ex, handler, handler_dbg) ->
     Ctrywith (instrument e, ex, with_afl_logging handler, handler_dbg)
  | Cswitch (e, cases, handlers, dbg) ->
     Cswitch (instrument e, cases,
       Array.map (fun (expr, dbg) -> with_afl_logging expr, dbg) handlers,
       dbg)

  (* these cases add no logging, but instrument subexpressions *)
  | Clet (v, e, body) -> Clet (v, instrument e, instrument body)
  | Cphantom_let (v, defining_expr, body) ->
    Cphantom_let (v, defining_expr, instrument body)
  | Cassign (v, e) -> Cassign (v, instrument e)
  | Ctuple es -> Ctuple (List.map instrument es)
  | Cop (op, es, dbg) -> Cop (op, List.map instrument es, dbg)
  | Csequence (e1, e2) -> Csequence (instrument e1, instrument e2)
  | Ccatch (isrec, cases, body) ->
     Ccatch (isrec,
             List.map (fun (nfail, ids, e, dbg) ->
                 nfail, ids, instrument e, dbg)
               cases,
             instrument body)
  | Cexit (ex, args) -> Cexit (ex, List.map instrument args)

  (* these are base cases and have no logging *)
  | Cconst_int _ | Cconst_natint _ | Cconst_float _
  | Cconst_symbol _ | Cconst_pointer _ | Cconst_natpointer _
  | Cblockheader _ | Cvar _ as c -> c

let instrument_function c =
  with_afl_logging c

let instrument_initialiser c =
  (* Each instrumented module calls caml_setup_afl at
     initialisation, which is a no-op on the second and subsequent
     calls *)
  with_afl_logging
    (Csequence
       (Cop (Cextcall (Backend_sym.Names.caml_setup_afl, typ_int, false, None),
             [cconst_int 0],
             Debuginfo.none),
        c))
