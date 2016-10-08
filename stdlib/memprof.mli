(** [Memprof] is an sampling engine for allocated words. The blocks

    are sampled according to a Poisson process. That is, every block
    has a probability of being sampled which is proportional to its
    size, and large blocks can be sampled several times. The samples
    appear using a configurable sampling rate.

    This engine is typically used by a statistical memory profiler.
 *)

type alloc_kind =
  | Minor
  | Major
  | Major_postponed
  | Serialized
(** Allocation kinds
    - [Minor] : the allocation took place in the minor heap.
    - [Major] : the allocation took place in the major heap.
    - [Major_postponed] : the allocation took place in the major heap,
      but the callback call has been postponed because the allocation
      took place in a context where calling an arbitrary code was not
      possible.
    - Serialized : the allocation happened during a
      deserialization. *)

type sample_info = {
    n_samples: int;
    (** The number of samples in this block. Always > 1, it is sampled
        according to a Poisosn process, and in average equal to the
        size of the block (including the header) multiplied by lambda
        (the sampling rate). *)
    kind: alloc_kind;
    (** The kind of the allocation. *)
    tag: int;
    (** The tag of the allocated block. *)
    size: int;
    (** The size of the allocated block, in words (exclusing the
        header). *)
    callstack: Printexc.raw_backtrace;
    (** The callstack for the allocation. *)
}
(** The meta data passed at each callback.  *)

type 'a callback = sample_info -> (Obj.t, 'a) Ephemeron.K1.t option
(** [callback] is the type of callbacks launch by the sampling engine.
   A callback returns an option over an ephemeron whose key is set
   to the allocated block for further tracking.

   The sampling is temporarily disabled when calling the callback. So
   it need not be reentrant.

   Note that when the callback kind is [Major_postponed], the callback
   needed to be postponed after the actual allocation. Therefore, the
   context of the callback is maybe slightly different than
   expected. This should happen very rarely if no C binding is
   used. *)

type 'a ctrl = {
    sampling_rate : float;
    (** The sampling rate in samples per word (including headers).
        Usually, with cheap callbacks, a rate of 0.001 has no visible
        effect on performances, and 0.01 keeps similar performances. *)
    callstack_size : int;
    (** The lenght of the callstack recorded at every sample. *)
    callback : 'a callback
    (** The callback to be called at every sample. *)
}
(** Control data for the sampling engine.  *)

val start : 'a ctrl -> unit
(** Start the sampling with the given parameters. If another
    sampling were running before, it is stopped. *)

val stop : unit -> unit
(** Stop the sampling. *)
