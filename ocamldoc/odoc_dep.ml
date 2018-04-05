(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Top modules dependencies. *)

open Misc
module Module = Odoc_module
module Type = Odoc_type

let set_to_list s =
  let l = ref [] in
  StringSet.iter (fun e -> l := e :: !l) s;
  !l

let impl_dependencies ast =
  Depend.free_structure_names := StringSet.empty;
  Depend.add_use_file StringMap.empty [Parsetree.Ptop_def ast];
  set_to_list !Depend.free_structure_names

let intf_dependencies ast =
  Depend.free_structure_names := StringSet.empty;
  Depend.add_signature StringMap.empty ast;
  set_to_list !Depend.free_structure_names


module Dep =
  struct
    type id = string

    let set_to_list s =
      let l = ref [] in
      StringSet.iter (fun e -> l := e :: !l) s;
      !l

    type node = {
        id : id ;
        mutable near : StringSet.t ; (** direct children *)
        mutable far : (id * StringSet.t) list ; (** indirect children, from which children path *)
        reflex : bool ; (** reflexive or not, we keep
                           information here to remove the node itself from its direct children *)
      }

    type graph = node list

    let make_node s children =
      let set = List.fold_right
          StringSet.add
          children
          StringSet.empty
      in
      { id = s;
        near = StringSet.remove s set ;
        far = [] ;
        reflex = List.mem s children ;
      }

    let get_node graph s =
      try List.find (fun n -> n.id = s) graph
      with Not_found ->
        make_node s []

    let rec trans_closure graph acc n =
      if StringSet.mem n.id acc then
        acc
      else
        (* potential optimisation: use far field if nonempty? *)
        StringSet.fold
          (fun child -> fun acc2 ->
            trans_closure graph acc2 (get_node graph child))
          n.near
          (StringSet.add n.id acc)

    let node_trans_closure graph n =
      let far = List.map
          (fun child ->
            let set = trans_closure graph StringSet.empty (get_node graph child) in
            (child, set)
          )
          (set_to_list n.near)
      in
      n.far <- far

    let compute_trans_closure graph =
      List.iter (node_trans_closure graph) graph

    let prune_node graph node =
      StringSet.iter
        (fun child ->
          let set_reachables = List.fold_left
              (fun acc -> fun (ch, reachables) ->
                if child = ch then
                  acc
                else
                  StringSet.union acc reachables
              )
              StringSet.empty
              node.far
          in
          let set = StringSet.remove node.id set_reachables in
          if StringSet.exists (fun n2 -> StringSet.mem child (get_node graph n2).near) set then
            (
             node.near <- StringSet.remove child node.near ;
             node.far <- List.filter (fun (ch,_) -> ch <> child) node.far
            )
          else
            ()
        )
        node.near;
      if node.reflex then
        node.near <- StringSet.add node.id node.near
      else
        ()

    let kernel graph =
      (* compute transitive closure *)
      compute_trans_closure graph ;

      (* remove edges to keep a transitive kernel *)
      List.iter (prune_node graph) graph;

      graph

  end

(** [type_deps t] returns the list of fully qualified type names
   [t] depends on. *)
let type_deps t =
  let module T = Odoc_type in
  let l = ref [] in
  let re = Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)" in
  let f s =
    let s2 = Str.matched_string s in
    l := s2 :: !l ;
    s2
  in
  let ty t =
    let s = Odoc_print.string_of_type_expr t in
    ignore (Str.global_substitute re f s)
  in
  (match t.T.ty_kind with
    T.Type_abstract -> ()
  | T.Type_variant cl ->
      List.iter
        (fun c ->
           match c.T.vc_args with
           | T.Cstr_tuple l -> List.iter ty l
           | T.Cstr_record l -> List.iter (fun r -> ty r.T.rf_type) l
        )
        cl
  | T.Type_record rl ->
      List.iter (fun r -> ty r.T.rf_type) rl
  | T.Type_open -> ()
  );

  (match t.T.ty_manifest with
    None -> ()
  | Some (T.Object_type fields) ->
      List.iter (fun r -> ty r.T.of_type) fields
  | Some (T.Other e) ->
      ty e
  );

  !l

(** Modify the module dependencies of the given list of modules,
   to get the minimum transitivity kernel. *)
let kernel_deps_of_modules modules =
  let graph = List.map
      (fun m -> Dep.make_node m.Module.m_name m.Module.m_top_deps)
      modules
  in
  let k = Dep.kernel graph in
  List.iter
    (fun m ->
      let node = Dep.get_node k m.Module.m_name in
      m.Module.m_top_deps <-
        List.filter (fun m2 -> StringSet.mem m2 node.Dep.near) m.Module.m_top_deps)
    modules

(** Return the list of dependencies between the given types,
   in the form of a list [(type, names of types it depends on)].
   @param kernel indicates if we must keep only the transitivity kernel
   of the dependencies. Default is [false].
*)
let deps_of_types ?(kernel=false) types =
  let deps_pre = List.map (fun t -> (t, type_deps t)) types in
  if kernel then
    (
      let graph = List.map
          (fun (t, names) -> Dep.make_node t.Type.ty_name names)
          deps_pre
      in
      let k = Dep.kernel graph in
      List.map
        (fun t ->
           let node = Dep.get_node k t.Type.ty_name in
           (t, Dep.set_to_list node.Dep.near)
        )
        types
    )
  else
    deps_pre
