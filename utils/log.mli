(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The {!Log} module provides an unified interface for logging structured data
    in a log. A log can be printed on a collection of {!Format} device.
*)

type !'id log
type 'id t = 'id log
 (** A log for the structured diagnostic with tag ['id]. *)

type ('id,'a, 'opt) field = ('id,'a, 'opt) Diagnostic.field
(** A field of type ['a] for an ['id log]. *)

(** Lower-level device for log *)
module Device: sig
  type t
  val make: ?on_close:(unit->unit) -> Format.formatter ref -> t
  val out_channel: Out_channel.t -> t
  val err: t
  val std: t
end

(** Backend printers *)
type printer = {
  record: Format.formatter -> Diagnostic.typed_record -> unit;
  item: Format.formatter -> string * Diagnostic.typed_val -> unit;
}

val log_version: _ log -> Diagnostic.version option
val log_scheme: 'id log -> 'id Diagnostic.t

 (** [streaming] log prints on the lower device item by item (using the
     [printer.item] function )*)
val make:
  streaming:bool -> printer:printer -> Misc.Color.setting option ->
  Diagnostic_validation.version -> 'a Diagnostic.t -> Device.t -> 'a log

val tmp: 'a Diagnostic.t -> 'a log
(** [tmp diagnostic] creates a temporary log with no attached [Device.t], that
    only stores logged item. *)

(** {1:log_fns Logging function } *)

(** [set log f x] send the value [x] to the [log] at field [f] if the field is
    active at the log version. Streaming log will print the field directly,
    while other log will only print the field contents when flushed.*)
val set: 'id log -> ('a,'id, _) field -> 'a -> unit
val (.%[]<-): 'id log -> ('a,'id, _) field -> 'a -> unit

(** [cons log f x] either prints directly the item [x] as a singleton list for
    streaming logs, or add the item to the current field otherwise. *)
val cons: 'id log -> ('a list, 'id, _) field -> 'a -> unit

(** [flush log] pushes on the underlying device the stored contents on the
    [Device.t] devices, and ensure that the [Device.t] is flushed too. Detached
    children log are flushed too.*)
val flush: 'id log -> unit

 (** Add a separator between items in streaming mode *)
val separate: 'id log -> unit


(** [close log] closes all device attached to the log. *)
val close: 'id log -> unit

(** [redirect log field device] attach [device] to the field. All subsequent
    printing on this field will be pushed on [device]. *)
val redirect: 'id log -> ('a,'id,_) field -> Device.t -> unit

(** {1:log_sublog Sublog function } *)

(** If [f] is a field for a record type [r], [detach parent f] creates a sublog
    for a record type [r] that shares its contents and redirections with the
    field [f] of the [parent] log. *)
val detach: 'id log -> ('id2 Diagnostic.record, 'id, _) field -> 'id2 log

(** If [f] is a field for a list of record [r], [detach_item log f] creates a
    [r] sublog that shares its contents with a new item in the list stored in
    the field [f] in the parent [log]. *)
val detach_item:
  'id log -> ('id2 Diagnostic.record list, 'id, _) field -> 'id2 log

(** {1:log_non_streaming Non-streaming log contents} *)

val get: 'id log -> ('a,'id, _) field -> 'a option
val dynamic_get: 'id log -> string -> Diagnostic.typed_val option

(** [replay source dest] transfer the contents of the [source] log (if any) to
    the [dest] log.*)
val replay: source:'a log -> dest:'a log -> unit

(** {1 Printing functions }*)

val f :
  (string,'a, _) field -> 'a log -> ('b, Format.formatter, unit) format -> 'b
  (** [fmt field log ppf] records the output of [ppf] as
      a string at field [field] in [log].
  *)

val d :
  (Format_doc.t,'a,_) field -> 'a log -> ('b, Format_doc.formatter, unit) format
  -> 'b
  (** [fmt field log ppf] records the formatted message at field [field] in
      [log]. *)

val itemf :
  (string list,'a, _) field -> 'a log -> ('b, Format.formatter, unit) format ->
  'b

val itemd :
  (Format_doc.t list,'a,_) field -> 'a log
  -> ('b, Format_doc.formatter, unit) format -> 'b

val log_if:
  'id log -> (string, 'id, _) field -> bool ->
  (Format.formatter -> 'a -> unit) -> 'a -> unit
