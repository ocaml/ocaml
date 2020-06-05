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
*)

type t
(** The type of load paths, mostly specified by sequences of [-I] flags. *)

type path = Dir of string | File of string
(** The type of load path entries *)

val path_to_string : path -> string

val empty : t

val of_dirs : string list -> t

val of_paths : path list -> t

val add_dir : t -> string -> t
(** Add a directory to the load path. *)

val add_file : t -> string -> t
(** Add a file to the load path. *)

val dirs : t -> string list
(** Return the sequence of directories in the load path. *)

val paths : t -> path list
(** Return the full sequence of entries in the load path. *)

val mem : path -> t -> bool

val concat : t list -> t

val expand_directory : string -> t -> t

val find_uncap : string -> t -> string

val find_rel : string -> t -> string

module Cache : sig
  (** This module takes care of caching the contents of the load path, to avoid
      costly directory traversals each time a file needs to be looked up.

      It makes the assumption that the contents of include directories
      doesn't change during the execution of the compiler. *)

  val add_dir : string -> unit
  (** Add a directory to the cache *)

  val add_file : string -> unit
  (** Add a file to the cache *)

  val add_path : path -> unit

  val remove_dir : string -> unit
  (** Remove a directory from the cache *)

  val reset : unit -> unit
  (** Remove all directories *)

  val init : t -> unit
  (** Initialize the cache with the given load path.  Earlier entries in the
      load path take precedence over later entries (ie left-to-right precedence
      for command-line flags). *)

  val get_paths : unit -> t
  (** Return a load path such that [init (get_paths ())] is equivalent to the
      current cache. *)

  val find : string -> string
  (** Locate a file in the load path. Raise [Not_found] if the file
      cannot be found. This function is optimized for the case where the
      filename is a basename, i.e. doesn't contain a directory
      separator. *)

  val find_uncap : string -> string
  (** Same as [find], but search also for uncapitalized name, i.e.  if
      name is Foo.ml, allow /path/Foo.ml and /path/foo.ml to match. *)

  module Path : sig
    type t
    (** Represent a cached version of an entry in the load path. *)

    val dir : string -> t

    val path : t -> path

    val files : t -> string list
    (** All the files in that directory. This doesn't include files in
        sub-directories of this directory. *)
  end

  val add : Path.t -> unit

  val get : unit -> Path.t list
  (** Same as [get_paths ()], except that it returns a [Dir.t list]. *)
end
