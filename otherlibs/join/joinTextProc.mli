(** Convenience functions for forking Unix commands oriented for text
    processing.

 All functions provided by this module
  fork commands given in the style
  of the [Unix.execvp] function.
  That is, a command is a program name plus an array
  of command line arguments and the program name is searched
  in path.

@see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#VALexecvp> [Unix.execvp]. *)

(**
 Additionally, the child process is controled by a combination
 of [kill]/[wait] channels, and the I/O channels are connected to
 producers (type [JoinCom.Prod.t]) for the [Async] interface, and
 to ordinary lists for the [Sync] interface.
*)


module Async : sig
(** Asynchronous producer interface.

    The functions below all return means to control the child process
    and some producers connected to the forked process output channels,
    when instructed to do so. *)

    type producer = string JoinCom.P.t (** Producers of text lines *)

    type t =
      { out : producer ; (** Connected to child process standard output *)
        err : producer ; (** Connected to child process standard error *)
        waitpid : Unix.process_status Join.chan Join.chan ; (** Invoke continuation argument when child process changes status. *)
        kill : int -> unit; (** Send (Unix) signal to child process *)
        gkill : int -> unit  (** Send (Unix) signal to child process group *)
      }
(** Notice that [out] or [err] above can be empty producers when
    child process channels redirection is not commanded. *)

    val command : string -> string array -> t
    (** Analogous of {!JoinProc.command}.
        Standard channels stdin, stdout, and stderr are the ones
        of the parent process.
        Accordingly, the [out] and [err] producers are empty producers. *)

    val open_in : string -> string array -> t
   (** Analogous of {!JoinProc.open_in}.
       Standard output of child process is connected to the [out] producer. *)

    val open_out : string -> string array -> producer -> t
    (**  Analogous of {!JoinProc.open_out}.
         Standard input of child process is read from the third, producer,
         argument. *)

    val open_in_out : string -> string array -> producer -> t
    (**  Analogous of {!JoinProc.open_in_out}.
         Combination of {!open_in} and {!open_out}. *)

    val open_full : string -> string array -> producer -> t
    (** Analogous of {!JoinProc.open_full}.
        Connect all three standard channels of the forked command
        to producers. *)
end

module Sync : sig
(** Synchronous list-based interface

    The functions below all return means to control the child process
    and ({i i. e.} a [wait]/[kill] pair).
    The [wait] synchronous channel returns the child status
    after termination, plus lists of lines.

    Input to the forked command also is a list of lines. *)

(** Depending on the function input may be absent,
    as output or error can also be (then replaced by empty lists [[]]). *)

  type text = string list (** Text is a list of lines *)

  type result = (** Result of forked command *)
      { st : Unix.process_status ; (** Child status *)
        out : text ; (** Standard output of child *)
        err : text ; (** Standard error of child *)
    }

  type t = (** Abstraction of forked command *)
      { wait : unit  -> result; (** Get result (will block) *)
        kill : int -> unit;     (** Kill child *)
        gkill : int -> unit;     (** Kill child group *)
      }
  val command : string -> string array ->  t
    (** Analogous of {!JoinProc.command}.
        Standard channels stdin, stdout, and stderr are the ones
        of the parent process.
        Accordingly, the [out] and [err] lists are empty. *)

    val open_in : string -> string array -> t
   (** Analogous of {!JoinProc.open_in}.
       Standard output of child process is the [out] list. *)

    val open_in_out : string -> string array -> text -> t
   (** Analogous of {!JoinProc.open_in_out}.
       The child process reads it standard input from third argument.
       Standard output of child process is the [out] list. *)

    val open_full : string -> string array -> text -> t
    (** Analogous of {!JoinProc.open_full}.
     Input of child is read from third argument, while standard output
     and error are the [out] and [err] lists. *)
end
