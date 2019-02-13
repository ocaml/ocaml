(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Management of include directories.

    This module offers a high level interface to locating files in the
    load path, which is constructed from [-I] command line flags and a few
    other parameters.

    It makes the assumption that the contents of include directories
    doesn't change during the execution of the compiler.
*)

val add_dir : string -> unit
(** Add a directory to the load path *)

val remove_dir : string -> unit
(** Remove a directory from the load path *)

val reset : unit -> unit
(** Remove all directories *)

val init : string list -> unit
(** [init l] is the same as [reset (); List.iter add_dir (List.rev l)] *)

val get_paths : unit -> string list
(** Return the list of directories passed to [add_dir] so far, in
    reverse order. *)

val find : string -> string
(** Locate a file in the load path. Raise [Not_found] if the file
    cannot be found. This function is optimized for the case where the
    filename is a basename, i.e. doesn't contain a directory
    separator. *)

val find_uncap : string -> string
(** Same as [find], but search also for uncapitalized name, i.e.  if
    name is Foo.ml, allow /path/Foo.ml and /path/foo.ml to match. *)

module Dir : sig
  type t
  (** Represent one directory in the load path. *)

  val create : string -> t

  val path : t -> string

  val files : t -> string list
  (** All the files in that directory. This doesn't include files in
      sub-directories of this directory. *)
end

val add : Dir.t -> unit

val get : unit -> Dir.t list
(** Same as [get_paths ()], except that it returns a [Dir.t list]. *)
