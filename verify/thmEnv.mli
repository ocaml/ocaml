open Why_ptree
open Asttypes
open Types
open Typedtree

type aval = Inline of expression
  | NotCon of (pattern * expression) list
  | IsCon of Path.t * constructor_description * expression list
  | NoVal 

type t = {
  tasks: decl list;
  name: Path.t;
  depth: int;
  contract_name: Path.t;
  dep_contracts: Typedtree.core_contract Ident.tbl; 
  contract_decls: Typedtree.contract_declaration list;
  opened_contract_decls: (Path.t * Types.contract_declaration) Ident.tbl; 
  axioms: axiom_declaration list;
  goalTasks: decl list;
  abinds: (Ident.t, expression) Tbl.t;
  vals: (expression, aval) Tbl.t;
}

val initEnv : contract_declaration list ->  
              (Path.t * Types.contract_declaration) Ident.tbl -> t
val tasks : t -> decl list
val name  : t -> Path.t
val depth : t -> int
val contract_name : t -> Path.t
val dep_contracts : t -> Typedtree.core_contract Ident.tbl
val contract_decls : t -> contract_declaration list
val opened_contract_decls : t -> (Path.t * Types.contract_declaration) Ident.tbl
val goalTasks : t -> decl list
val add_tasks : t -> decl list-> t
val update_name : t -> Path.t -> t
val update_contract_name : t -> Path.t -> t
val add_dep_contracts : t -> Ident.t -> Typedtree.core_contract -> t
val add_contract_decl : t -> contract_declaration -> t
val add_axiom : t -> axiom_declaration -> t
val add_goalTasks : t -> decl list -> t
val extend_senv : t -> Ident.t -> expression -> t
val extend_denv : t -> expression -> aval -> t 
val lookup_senv : Ident.t -> t -> aval
val lookup_denv : expression -> t -> aval
