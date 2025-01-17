(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val register : pass_name:string -> unit

val with_log
   : log:Compiler_diagnostic.Debug.id Log.t
  -> field: string list Compiler_diagnostic.Debug.field
  -> pass_name:string
  -> f:(unit -> 'b option)
  -> input:'a
  -> print_input:(Format.formatter -> 'a -> unit)
  -> print_output:(Format.formatter -> 'b -> unit)
  -> 'b option
