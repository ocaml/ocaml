open Types
open Typedtree

val dynamic_contract_checking: bool ref -> structure * module_coercion 
                                        -> structure * module_coercion
val static_contract_checking: bool ref -> structure * module_coercion 
                                       -> structure * module_coercion
val hybrid_contract_checking: bool ref -> structure * module_coercion 
                                       -> structure * module_coercion
val contract_id_in_contract: ThmEnv.t -> Typedtree.core_contract 
                                      -> Typedtree.core_contract

val contract_id_in_expr: ThmEnv.t -> Typedtree.expression -> Typedtree.expression
val deep_transl_contract: ThmEnv.t -> Typedtree.expression -> Typedtree.expression

