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
  | IsTuple of expression list
  | IsCon of Path.t * constructor_description * expression list
  | NoVal 


type t = {
  tasks:   decl list;              (* logical store *)
  name: Path.t;                    (* name of function under contract checking *)
  depth: int;                      (* number of unrollings *)
  contract_name: Path.t;
  top_defs: Path.t list;      (* top-level functions called in current definition *)
  descendants: Path.t list ;
  dep_contracts: Typedtree.core_contract Ident.tbl; (* "x:t1" -> t2 *)
  type_decls: (string * type_declaration) list;
  contract_decls: contract_declaration list;
  opened_contract_decls: (Path.t * Types.contract_declaration) Ident.tbl; 
  axioms: axiom_declaration list;
  goalTasks: decl list;
  abinds: (Ident.t, expression) Tbl.t;    (* variable |-> tvalue *)
  vals: (expression, aval) Tbl.t;         (* variable |-> expression *)
  nonrecs : (Ident.t, expression) Tbl.t;  (* top-level nonrecursive functions *)
  recs : (Ident.t, expression) Tbl.t;     (* top-level recursive functions *)
}


let init_tasks = 
  let loc = Location.none in
  let mklexpr desc = {pp_loc = loc; pp_desc = desc} in
  let a = PPTvarid ("'a", loc) in
  let b = PPTvarid ("'b", loc) in
  let c = PPTvarid ("'c", loc) in
  let a_list = PPTexternal ([a], "list", loc) in
  let a_b_arrow = PPTexternal ([a;b], "arrow", loc) in
  let a_b_tuple = PPTexternal ([a;b], "tuple2", loc) in
  let a_b_c_tuple = PPTexternal ([a;b;c], "tuple3", loc) in
  [TypeDecl (loc, ["'a";"'b"], "arrow");
   TypeDecl (loc, ["'a";"'b"], "tuple2");
   TypeDecl (loc, ["'a";"'b";"'c"], "tuple3");
   Logic (loc, false, ["apply"], PFunction ([a_b_arrow; a], b));
   TypeDecl (loc, ["'a"], "list");
   Logic (loc, false, ["nil"], PFunction ([], a_list));
   Logic (loc, false, ["cons"], PFunction ([a; a_list], a_list ));
   Logic (loc, false, ["tup2"], PFunction ([a;b], a_b_tuple));
   Logic (loc, false, ["tup3"], PFunction ([a;b;c], a_b_c_tuple));
   (* axiom list_a0 : forall x:'a. forall l:'a list. nil <> cons(x,l) *)
   Axiom (loc, "list_a0", mklexpr (PPforall(["x"], a, [], mklexpr(PPforall(["l"], a_list, [], mklexpr (PPinfix(mklexpr(PPvar "nil"), PPneq, mklexpr(PPapp("cons", [mklexpr(PPvar "x");mklexpr(PPvar "l")])))))))));
   (* axiom cons_a0 : forall x,y:int. forall l,m:int list. 
      cons(x,l) = cons(y,m) -> x=y and l=m 
   Axiom (loc, "cons_a0", mklexpr *)
 ]

(* empty environment *)
let initEnv x1 x2 x3 x4 = { 
  tasks  = init_tasks;
  name = Pident (Ident.create "caller");
  depth = 0;
  contract_name = Pident (Ident.create "contract caller");
  top_defs = []; (* so far *)
  descendants = x4;
  dep_contracts = Ident.empty; 
  type_decls = x3;
  contract_decls = x1;
  opened_contract_decls = x2;
  axioms = [];
  goalTasks = []; (* represent a goal in terms of tasks *)
  abinds =  Tbl.empty;
  vals = Tbl.empty;
  nonrecs = Tbl.empty;
  recs = Tbl.empty;
}

(* Insertion of bindings by identifier *)

let extend_senv env id1 id2 = {env with abinds = Tbl.add id1 id2 env.abinds}
let extend_denv env exp aval = {env with vals = Tbl.add exp aval env.vals}
let extend_nonrec_env env id1 e = {env with nonrecs = Tbl.add id1 e env.nonrecs}
let extend_rec_env env id1 e = {env with recs = Tbl.add id1 e env.recs}


(* the name of the current function under contract checking *)
let update_name env n =  { env with name = n }

let update_contract_name env n =  { env with contract_name = n }

let decrease_depth env = { env with depth = env.depth - 1 }
let increase_depth env = { env with depth = env.depth + 1 }

let add_top_defs env ds = { env with top_defs = env.top_defs@ds }

let add_dep_contracts env x c = 
  { env with dep_contracts = Ident.add x c env.dep_contracts}

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
let depth env = env.depth
let contract_name env = env.contract_name
let top_defs env = env.top_defs
let descendants env = env.descendants
let dep_contracts env = env.dep_contracts
let type_decls env = env.type_decls
let contract_decls env = env.contract_decls
let opened_contract_decls env = env.opened_contract_decls
let goalTasks env = env.goalTasks
let lookup_senv id env = try let x = Tbl.find id env.abinds in
                             Tbl.find x env.vals 
                         with Not_found -> NoVal

let lookup_denv exp env = try Tbl.find exp env.vals
                          with Not_found -> NoVal

let lookup_nonrec_env id env = Tbl.find id env.nonrecs
let lookup_rec_env id env = Tbl.find id env.recs
let recs env = env.recs
let nonrecs env = env.nonrecs

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
