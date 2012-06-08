(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: env.ml 9240 2009-04-28 05:11:54Z garrigue $ *)

(* Environment handling *)

open Config
open Misc
open Asttypes
open Longident
open Path
open Types
open Typedtree
open Why_ptree

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string
  | Need_recursive_types of string * string

exception Error of error

type aval = 
    Inline of expression
  | NotCon of (pattern * expression) list
  | IsCon of Path.t * constructor_description * expression list
  | NoVal 


type t = {
  tasks:   decl list;              (* logical store *)
  name: Path.t;                    (* name of function under contract checking *)
  depth: int;                      (* number of unrollings *)
  contract_name: Path.t;
  dep_contracts: Typedtree.core_contract Ident.tbl; 
  contract_decls: contract_declaration list;
  opened_contract_decls: (Path.t * Types.contract_declaration) Ident.tbl; 
  axioms: axiom_declaration list;
  goalTasks: decl list;
  abinds: (Ident.t, expression) Tbl.t;
  vals: (expression, aval) Tbl.t;         (* variable |-> value *)
}

let init_tasks = 
  let a_list = PPTexternal ([PPTvarid ("'a", Location.none)], "list", 
                           Location.none) in
  let a_b_arrow = PPTexternal ([PPTvarid ("'a", Location.none);
                                PPTvarid ("'b", Location.none)], "arrow", 
                              Location.none) in
  let a_b_tuple = PPTexternal ([PPTvarid ("'a", Location.none);
                                PPTvarid ("'b", Location.none)], "tuple", 
                              Location.none) in
  [TypeDecl (Location.none, ["'a";"'b"], "arrow");
   TypeDecl (Location.none, ["'a";"'b"], "tuple");
   Logic (Location.none, false, ["apply"], 
          PFunction ([a_b_arrow; PPTvarid ("'a", Location.none)],
          PPTvarid ("'b", Location.none)));
   TypeDecl (Location.none, ["'a"], "list");
   Logic (Location.none, false, ["nil"], PFunction ([], a_list));
   Logic (Location.none, false, ["cons"], PFunction ([PPTvarid ("'a", Location.none); a_list], a_list ));
   Logic (Location.none, false, ["tup"], PFunction ([PPTvarid ("'a", Location.none);PPTvarid ("'b", Location.none);], a_b_tuple));
 ]

(* empty environment *)
let initEnv l1 l2 = { 
  tasks  = init_tasks;
  name = Pident (Ident.create "caller");
  depth = 0;
  dep_contracts = Ident.empty; 
  contract_name = Pident (Ident.create "contract caller");
  contract_decls = l1;
  opened_contract_decls = l2;
  axioms = [];
  goalTasks = []; (* represent a goal in terms of tasks *)
  abinds =  Tbl.empty;
  vals = Tbl.empty;
}

(* Insertion of bindings by identifier *)

let extend_senv env id1 id2 = {env with abinds = Tbl.add id1 id2 env.abinds}
let extend_denv env exp aval = {env with vals = Tbl.add exp aval env.vals}

(* the name of the current function under contract checking *)
let update_name env n =  { env with name = n }

let update_contract_name env n =  { env with contract_name = n }

let add_dep_contracts env x c = 
  { env with dep_contracts = Ident.add x c env.dep_contracts }

let add_contract_decl env cdecl = 
  { env with contract_decls = (env.contract_decls)@[cdecl] }

(* add axioms *)
let add_axiom env a = 
  { env with axioms = a::(env.axioms) }

(* add function info to program *)

let add_tasks env decls = 
  { env with tasks = (env.tasks)@decls }

let add_goalTasks env gts = 
  { env with goalTasks = (env.goalTasks)@gts }

(* lookup env *)

let tasks env = env.tasks
let name env = env.name
let contract_name env = env.contract_name
let dep_contracts env = env.dep_contracts
let contract_decls env = env.contract_decls
let opened_contract_decls env = env.opened_contract_decls
let goalTasks env = env.goalTasks
let lookup_senv id env = try let x = Tbl.find id env.abinds in
                             Tbl.find x env.vals 
                         with Not_found -> NoVal

let lookup_denv exp env = try Tbl.find exp env.vals
                          with Not_found -> NoVal


(* Error report *)

open Format

let report_error ppf = function
  | Not_an_interface filename -> fprintf ppf
      "%s@ is not a compiled interface" filename
  | Corrupted_interface filename -> fprintf ppf
      "Corrupted compiled interface@ %s" filename
  | Illegal_renaming(modname, filename) -> fprintf ppf
      "Wrong file naming: %s@ contains the compiled interface for@ %s"
      filename modname
  | Inconsistent_import(name, source1, source2) -> fprintf ppf
      "@[<hov>The files %s@ and %s@ \
              make inconsistent assumptions@ over interface %s@]"
      source1 source2 name
  | Need_recursive_types(import, export) ->
      fprintf ppf
        "@[<hov>Unit %s imports from %s, which uses recursive types.@ %s@]"
        import export "The compilation flag -rectypes is required"
