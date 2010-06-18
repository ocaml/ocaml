
(** Communication by asynchronous producers/consumers pairs *)

(** By convention, consumers consume data,
    while producers produce data *)

(** Producers *)
module P :  sig

    type 'a t = {
      get : 'a option Join.chan Join.chan; (** Ask for an item *)
      kill : unit Join.chan; (** Signal no more items are asked for *)
    }

  val empty : unit -> 'a t
(** Empty producer, [get] will always return [None]. *)

  val of_list : 'a list -> 'a t
(** [of_list xs] returns a producer for the elements of xs *)

  val to_list : ('a t * 'a list Join.chan) Join.chan
(** [to_list (prod,k)] asynchronously consumes all the elements produced
    by [prod]. The complete list, in production order, is sent on [k] *)

  val of_text : in_channel -> string t
(** [of_text chan] returns a producer [prod] for the lines of
    channel [chan]. Notice that [chan] is not closed when end of file
    or I/O error occurs. Use [prod.kill] to close [chan]. *)

  val to_text : (string t * out_channel * unit Join.chan) Join.chan 
(** [to_text (prod,chan,k)]  asynchronously consumes all the elements produced
    by [prod], writing lines to channel [chan] in production order.
    A unit message is sent on [k] whn production is over.

    Notice that [prod] is killed in case of I/O error. *)
end

(** Consumers *)
module C : sig
  type 'a t = {
      put : ('a * bool Join.chan) Join.chan; (** Submit item *)
      close : unit -> unit; (** Signal end of items *)
    }

end

val connect : ('a P.t * 'a C.t * unit Join.chan) Join.chan
(** [connect (prod,cons,k)] connnects producer [prod] and consumer [cons].
    The consumer will consume all production, until [None] is sent.
    Then, the consumer is closed and an unit message is sent to
    continuation [k].
    In case the consumer rejects an item, the producer is killed. *)
