(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Cambium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Definition of a class which outputs a dot file showing
    top modules dependencies. *)

val dot_include_all : bool ref

val dot_types : bool ref

val dot_reduce : bool ref

val dot_colors : string list ref

module Generator :
  sig
    class dot :
      object
        val mutable colors : string list
        val mutable loc_colors : (Odoc_info.Name.t * string) list
        val mutable modules : Odoc_info.Module.t_module list
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_for_module :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method generate_for_type :
          Format.formatter ->
          Odoc_info.Type.t_type * Odoc_info.Name.t list -> unit
        method generate_modules : Odoc_info.Module.t_module list -> unit
        method generate_types : Odoc_info.Type.t_type list -> unit
        method get_one_color : string option
        method header : string
        method node_color : Odoc_info.Name.t -> string option
        method print_module_atts :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method print_one_dep :
          Format.formatter -> Odoc_info.Name.t -> Odoc_info.Name.t -> unit
        method print_type_atts :
          Format.formatter -> Odoc_info.Type.t_type -> unit
      end
  end

module type Dot_generator =
  sig
    class dot :
      object
        val mutable colors : string list
        val mutable loc_colors : (Odoc_info.Name.t * string) list
        val mutable modules : Odoc_info.Module.t_module list
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_for_module :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method generate_for_type :
          Format.formatter ->
          Odoc_info.Type.t_type * Odoc_info.Name.t list -> unit
        method generate_modules : Odoc_info.Module.t_module list -> unit
        method generate_types : Odoc_info.Type.t_type list -> unit
        method get_one_color : string option
        method header : string
        method node_color : Odoc_info.Name.t -> string option
        method print_module_atts :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method print_one_dep :
          Format.formatter -> Odoc_info.Name.t -> Odoc_info.Name.t -> unit
        method print_type_atts :
         Format.formatter -> Odoc_info.Type.t_type -> unit
      end
  end
