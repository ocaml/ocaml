(** Decorate a pair of outcometree to highlight difference *)

(** Control how much difference tree are extended *)
val fuel: int ref

type mode = Inclusion | Unification
open Outcometree

(** Decorated tree generator *)
module Gen: sig
  type ('a,'b) t = mode -> 'a * 'a -> int -> 'b Decorated.ext * 'b Decorated.ext
  val typ: (out_type, Decorated.out_type) t
  val sig_item: (out_sig_item, Decorated.out_sig_item) t
  val class_type: (out_class_type, Decorated.out_class_type) t
  val modtype: (Outcometree.out_module_type, Decorated.out_module_type) t
end

type ('a,'b) t = mode -> 'a * 'a -> 'b Decorated.ext * 'b Decorated.ext

val typ: (out_type, Decorated.out_type) t
val sig_item: (out_sig_item, Decorated.out_sig_item) t
val class_type: (out_class_type, Decorated.out_class_type) t
val modtype: (out_module_type, Decorated.out_module_type) t
