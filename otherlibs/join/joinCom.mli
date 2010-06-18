
(** Communication by asynchronous consumer/producers pairs *)

(** By convention, consumers consume data,
    while producers produce data *)

(** Consumers *)
module C : sig
  type 'a t = {
      put : ('a * bool Join.chan) Join.chan;
      close : unit -> unit;
    }

end


(** Producers *)
module P :  sig

    type 'a t = {
      get : 'a option Join.chan Join.chan;
      kill : unit Join.chan;
    }

  val empty : unit -> 'a t
(** Empty producer, always send [None]. *)

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

    [prod] is killed in case of I/O error. *)
end

