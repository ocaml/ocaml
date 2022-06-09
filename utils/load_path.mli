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
(** Add a directory to the end of the load path (i.e. at lowest priority.) *)

val remove_dir : string -> unit
(** Remove a directory from the load path *)

val reset : unit -> unit
(** Remove all directories *)

module Dir : sig
  type t
  (** Represent one directory in the load path. *)

  val create : string -> t

  val path : t -> string

  val files : t -> string list
  (** All the files in that directory. This doesn't include files in
      sub-directories of this directory. *)

  val find : t -> string -> string option
  (** [find dir fn] returns the full path to [fn] in [dir]. *)

  val find_uncap : t -> string -> string option
  (** As {!find}, but search also for uncapitalized name, i.e. if name is
      Foo.ml, either /path/Foo.ml or /path/foo.ml may be returned. *)
end

type auto_include_callback =
  (Dir.t -> string -> string option) -> string -> string
(** The type of callback functions on for [init ~auto_include] *)

val no_auto_include : auto_include_callback
(** No automatic directory inclusion: misses in the load path raise [Not_found]
    as normal. *)

val init : auto_include:auto_include_callback -> string list -> unit
(** [init l] is the same as [reset (); List.iter add_dir (List.rev l)] *)

val auto_include_otherlibs :
  (string -> unit) -> auto_include_callback
(** [auto_include_otherlibs alert] is a callback function to be passed to
    {!Load_path.init} and automatically adds [-I +lib] to the load path after
    calling [alert lib]. *)

val get_paths : unit -> string list
(** Return the list of directories passed to [add_dir] so far. *)

val find : string -> string
(** Locate a file in the load path. Raise [Not_found] if the file
    cannot be found. This function is optimized for the case where the
    filename is a basename, i.e. doesn't contain a directory
    separator. *)

val find_uncap : string -> string
(** Same as [find], but search also for uncapitalized name, i.e.  if
    name is Foo.ml, allow /path/Foo.ml and /path/foo.ml to match. *)

val[@deprecated] add : Dir.t -> unit
(** Old name for {!append_dir} *)

val append_dir : Dir.t -> unit
(** [append_dir d] adds [d] to the end of the load path (i.e. at lowest
    priority. *)

val prepend_dir : Dir.t -> unit
(** [prepend_dir d] adds [d] to the start of the load path (i.e. at highest
    priority. *)

val get : unit -> Dir.t list
(** Same as [get_paths ()], except that it returns a [Dir.t list]. *)
