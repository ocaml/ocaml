(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* WARNING: if you change something in this file, you must look at
   opterrors.ml and ocamldoc/odoc_analyse.ml
   to see if you need to make the same changes there.
*)

open Format

(* Report an error *)

let report_error ppf exn =
  let report ppf x =
    match Location.error_of_exn x with
    | Some err -> Location.report_error ppf err
    | None -> fprintf ppf "@]"; raise x
  in
  fprintf ppf "@[%a@]@." report exn
