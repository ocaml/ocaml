(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* This is a test of dynamics for various types. *)

(* All the extra lets make it easy see the types with ocamlc -i *)

let do_test s b =
  if b then begin print_string ("    ok:" ^ s); print_newline () end
  else      begin print_string ("fail  :" ^ s); print_newline () end;;


(* primitive types ==================================== *)

(* see the declarations ident_* in typing/predefs.ml *)


(* int        *) let d_int = dynamic 1;;
                 let x_int = coerce (d_int : int);;
                 do_test "int" (x_int = 1);;
                 
(* char       *) let d_char = dynamic '1';;
                 let x_char = coerce (d_char : char);;
                 do_test "char" (x_char = '1');;

(* string     *) let d_string = dynamic "1";;
                 let x_string = coerce (d_string : string);;
                 do_test "string" (x_string = "1");;

(* float      *) let d_float = dynamic 1.0;;
                 let x_float = coerce (d_float : float);;
                 do_test "float" (x_float = 1.0);;

(* bool       *) let d_bool = dynamic true;;
                 let x_bool = coerce (d_bool : bool);;
                 do_test "bool" (x_bool = true);;

(* unit       *) let d_unit = dynamic ();;
                 let x_unit = coerce (d_unit : unit);;
                 do_test "unit" (x_unit = ());;
                 (* to do: make sure the optimizer isn't eliminating
                    the evaluation of x_unit = () *)

(* exn        *) exception Ex;;
                 let d_exn = dynamic Ex;;
                 let x_exn = coerce (d_exn : exn);;
                 do_test "exn" (x_exn = Ex);;

(* array      *) let d_array = dynamic [| 1 |];;
                 let x_array = coerce (d_array : int array);;
                 do_test "array" (x_array  = [| 1 |]);;
                 (* to do: test whether dynamic copies pointers or
                    values *)

(* list       *) let d_list = dynamic [1];;
                 let x_list = coerce (d_list : int list);;
                 do_test "list" (x_list = [1]);;
                 (* to do: test polymorphism *)

(* format     *) (* to do *)

(* option     *) let d_option1 = dynamic (Some 1);;
                 let x_option1 = coerce (d_option1 : int option);;
                 do_test "option1 (Some)" (x_option1 = Some 1);;

                 let d_option2 = dynamic None;;
                 let x_option2 = coerce (d_option2 : int option);;
                 do_test "option2 (None)" (x_option2 = None);;

(* nativeint  *) (* to do *)
(* int32      *) (* to do *)
(* int64      *) (* to do *)

(* dyn        *) let d_dyn = dynamic (dynamic 1);;
                 let x_dyn = coerce (d_dyn : dyn);;
                 do_test "dyn" (x_dyn = dynamic 1);;
                 (* to do: test many more kinds of nesting *)

(* channel    *) (* to do *)
(* process    *) (* to do *)
(* location   *) (* to do *)


(* other types ==================================== *)

(* tuple      *) let d_tuple = dynamic (1,2);;
                 let x_tuple = coerce (d_tuple : int * int);;
                 do_test "tuple" (x_tuple = (1,2));;
                 (* to do: test other kinds *)

(* arrow      *) let d_arrow = dynamic (fun x -> x + 1);;
                 let x_arrow = coerce (d_arrow : int -> int);;
                 do_test "arrow" (x_arrow 1 = 2);;
                 (* to do: test polymorphism, closures *)

(* record     *) type t_record = {f : int;};;
                 let d_record = dynamic ({f = 1;});;
                 let x_record = coerce (d_record : t_record);;
                 do_test "record" (x_record = {f = 1;});;

(* variant    *) type t_variant = C of int;;
                 let d_variant = dynamic (C 1);;
                 let x_variant = coerce (d_variant : t_variant);;
                 do_test "" (x_variant = C 1);;

(* object     *) (* to do *)
(* mutable    *) (* to do *)
(* manifest module types *) (* to do *)
       











