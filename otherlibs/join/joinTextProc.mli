(** Convenience functions for forking Unix commands oriented for text
    processing.

    The functions provided here are wrappers around the homonymous
    functions of module {!JoinProc}. *)


module Async : sig
(** Asychronous producer interface.

    The functions below all return means to control the child process
    and some producers connected to the forked process output channels,
    when instructed to do so. *)

    type producer = string JoinCom.P.t (** Producers of text lines *)

    type t =
      { out : producer ; (** Connected to child process standard output *)
        err : producer ; (** Connected to child process standard error *)
        waitpid : Unix.process_status Join.chan Join.chan ; (** Invoke continuation argument when child process changes status. *)
        kill : int -> unit; (** Send (Unix) signal to child process *)
        pid : int; (** Pid of child process. *)
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

  type text = string list

  type result =
      { st : Unix.process_status ;
        out : text ;
        err : text ; }

  type t =
      { wait : unit  -> result;
        kill : int -> unit; }


  val create : string -> string array -> string list -> t

end
