(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Specific operations for the Intel 386 processor *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iscaled of int * int                (* reg * scale + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

type specific_operation =
    Ilea of addressing_mode             (* Lea gives scaled adds *)
  | Istore_int of int * addressing_mode (* Store an integer constant *)
  | Istore_symbol of string * addressing_mode (* Store a symbol *)
  | Ioffset_loc of int * addressing_mode (* Add a constant to a location *)
  | Ifloatarith of float_oper * addressing_mode option *
                   addressing_mode option * addressing_mode option 
                                        (* Reg/mem float operations *)
and float_oper =
    Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv

(* Sizes, endianness *)

let big_endian = false

let size_addr = 4
let size_int = 4
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 n -> Iindexed2(n + delta)
  | Iscaled(scale, n) -> Iscaled(scale, n + delta)
  | Iindexed2scaled(scale, n) -> Iindexed2scaled(scale, n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1
  | Iindexed2 n -> 2
  | Iscaled(scale, n) -> 1
  | Iindexed2scaled(scale, n) -> 2

(* Printing operations and addressing modes *)

open Format

let print_addressing printreg addr arg =
  match addr with
    Ibased(s, 0) ->
      print_string "\""; print_string s; print_string "\""
  | Ibased(s, n) ->
      print_string "\""; print_string s; print_string "\" + "; print_int n
  | Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed2 n ->
      printreg arg.(0); print_string " + "; printreg arg.(1);
      if n <> 0 then begin print_string " + "; print_int n end
  | Iscaled(scale, n) ->
      printreg arg.(0); print_string " * "; print_int scale;
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed2scaled(scale, n) ->
      printreg arg.(0); print_string " + "; printreg arg.(1);
      print_string " * "; print_int scale;
      if n <> 0 then begin print_string " + "; print_int n end

let print_sub_addressing printreg addr arg pos =
  print_addressing printreg addr (Array.sub arg pos (Array.length arg - pos))

let print_specific_operation printreg op arg =
  match op with
    Ilea addr -> print_addressing printreg addr arg
  | Istore_int(n, addr) ->
      print_string "["; print_addressing printreg addr arg;
      print_string "] := "; print_int n
  | Istore_symbol(lbl, addr) ->
      print_string "["; print_addressing printreg addr arg;
      print_string "] := \""; print_string lbl; print_string "\""
  | Ioffset_loc(n, addr) ->
      print_string "["; print_addressing printreg addr arg;
      print_string "] +:= "; print_int n
  | Ifloatarith(op, arg1, arg2, res) ->
      let pos_arg2 =
        match arg1 with None -> 1 | Some addr -> num_args_addressing addr in
      let pos_res =
        pos_arg2 +
        (match arg2 with None -> 1 | Some addr -> num_args_addressing addr) in
      begin match res with
          None -> ()
        | Some addr ->
            print_string "["; print_sub_addressing printreg addr arg pos_res;
            print_string "] := "
      end;
      let print_arg pos = function
          None ->
            printreg arg.(pos)
        | Some addr ->
            print_string "[";
            print_sub_addressing printreg addr arg pos in
      print_arg 0 arg1;
      begin match op with
        Ifloatadd -> print_string " +f "
      | Ifloatsub -> print_string " -f "
      | Ifloatmul -> print_string " *f "
      | Ifloatdiv -> print_string " /f "
      end;
      print_arg pos_arg2 arg2
