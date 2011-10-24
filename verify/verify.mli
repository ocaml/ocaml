open Types
open Typedtree

(* 0: dynamic, 1: static, 2: hybrid *)
val transl_contracts : int -> structure * module_coercion -> structure * module_coercion

val contract_id_in_expr :
           ThmEnv.t -> Typedtree.expression -> Typedtree.expression

val contract_id_in_contract :
          ThmEnv.t -> Typedtree.core_contract -> Typedtree.core_contract

val deep_transl_contract: ThmEnv.t -> Typedtree.expression -> Typedtree.expression
