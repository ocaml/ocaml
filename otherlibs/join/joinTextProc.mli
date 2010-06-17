
module Async : sig

  type producer = string JoinCom.P.t

  type t =
      { out : producer ;
        err : producer ;
        waitpid : Unix.process_status Join.chan Join.chan ;
        kill : int -> unit;
        pid : int; }

  val create : string -> string array -> producer -> t

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
