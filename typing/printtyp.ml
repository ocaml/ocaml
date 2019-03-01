(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Printing functions *)

open Misc
open Ctype
open Format
open Longident
open Path
open Asttypes
open Types
open Btype
open Outcometree

module String = Misc.Stdlib.String

(* Print a long identifier *)

let rec longident ppf = function
  | Lident s -> pp_print_string ppf s
  | Ldot(p, s) -> fprintf ppf "%a.%s" longident p s
  | Lapply(p1, p2) -> fprintf ppf "%a(%a)" longident p1 longident p2

(* Print an identifier avoiding name collisions *)

module Out_name = struct
  let create x = { printed_name = x }
  let print x = x.printed_name
  let set out_name x = out_name.printed_name <- x
end

(* printing environment for path shortening and naming *)
let printing_env = ref Env.empty
let human_unique n id = Printf.sprintf "%s/%d" (Ident.name id) n

type namespace =
  | Type
  | Module
  | Module_type
  | Class
  | Class_type
  | Other (** Other bypasses the unique name identifier mechanism *)

module Namespace = struct

  let id = function
    | Type -> 0
    | Module -> 1
    | Module_type -> 2
    | Class -> 3
    | Class_type -> 4
    | Other -> 5

  let size = 1 + id Other

  let show =
    function
    | Type -> "type"
    | Module -> "module"
    | Module_type -> "module type"
    | Class -> "class"
    | Class_type -> "class type"
    | Other -> ""

  let lookup =
    let to_lookup f lid =
      fst @@ f ?loc:None ?mark:(Some false) (Lident lid) !printing_env in
    function
    | Type -> fun id ->
      Env.lookup_type ?loc:None ~mark:false (Lident id) !printing_env
    | Module -> fun id ->
      Env.lookup_module ~load:true ~mark:false ?loc:None
        (Lident id) !printing_env
    | Module_type -> to_lookup Env.lookup_modtype
    | Class -> to_lookup Env.lookup_class
    | Class_type -> to_lookup Env.lookup_cltype
    | Other -> fun _ -> raise Not_found

  let location namespace id =
    let env = !printing_env in
    let path = Path.Pident id in
    try Some (
        match namespace with
        | Type -> (Env.find_type path env).type_loc
        | Module -> (Env.find_module path env).md_loc
        | Module_type -> (Env.find_modtype path env).mtd_loc
        | Class -> (Env.find_class path env).cty_loc
        | Class_type -> (Env.find_cltype path env).clty_loc
        | Other -> Location.none
      ) with Not_found -> None

  let best_class_namespace = function
    | Papply _ | Pdot _ -> Module
    | Pident c ->
        match location Class c with
        | Some _ -> Class
        | None -> Class_type

end

(** {2 Conflicts printing}
    Conflicts arise when multiple items are attributed the same name,
    the following module stores the global conflict references and
    provides the printing functions for explaining the source of
    the conflicts.
*)
module Conflicts = struct
  module M = String.Map
  type explanation = { kind: namespace; name:string; location:Location.t}
  let explanations = ref M.empty
  let explain namespace n id =
    let name = human_unique n id in
    if not (M.mem name !explanations) then
      match Namespace.location namespace id with
      | None -> ()
      | Some location ->
          explanations :=
            M.add name { kind = namespace; location; name } !explanations

  let pp_explanation ppf r=
    Format.fprintf ppf "@[<v 2>%a:@,Definition of %s %s@]"
      Location.print_loc r.location (Namespace.show r.kind) r.name

  let pp ppf l =
    Format.fprintf ppf "@[<v>%a@]" (Format.pp_print_list pp_explanation) l

  let reset () = explanations := M.empty
  let take () =
    let c = !explanations in
    reset ();
    c |> M.bindings |> List.map snd |> List.sort Stdlib.compare

  let print ppf =
    let sep ppf = Format.fprintf ppf "@ " in
    let l =
      List.filter (* remove toplevel locations, since they are too imprecise *)
        ( fun a ->
            a.location.Location.loc_start.Lexing.pos_fname <> "//toplevel//" )
        (take ()) in
    match l with
    | [] -> ()
    | l -> Format.fprintf ppf "%t%a" sep pp l

  let exists () = M.cardinal !explanations >0
end


module Naming_context = struct

module M = String.Map
module S = String.Set

let enabled = ref true
let enable b = enabled := b

(** Name mapping *)
type mapping =
  | Need_unique_name of int Ident.Map.t
  (** The same name has already been attributed to multiple types.
      The [map] argument contains the specific binding time attributed to each
      types.
  *)
  | Uniquely_associated_to of Ident.t * out_name
    (** For now, the name [Ident.name id] has been attributed to [id],
        [out_name] is used to expand this name if a conflict arises
        at a later point
    *)
  | Associated_to_pervasives of out_name
  (** [Associated_to_pervasives out_name] is used when the item
      [Stdlib.$name] has been associated to the name [$name].
      Upon a conflict, this name will be expanded to ["Stdlib." ^ name ] *)

let hid_start = 0

let add_hid_id id map =
  let new_id = 1 + Ident.Map.fold (fun _ -> max) map hid_start in
  new_id, Ident.Map.add id new_id  map

let find_hid id map =
  try Ident.Map.find id map, map with
  Not_found -> add_hid_id id map

let pervasives name = "Stdlib." ^ name

let map = Array.make Namespace.size M.empty
let get namespace = map.(Namespace.id namespace)
let set namespace x = map.(Namespace.id namespace) <- x

(* Names used in recursive definitions are not considered when determining
   if a name is already attributed in the current environment.
   This is a weaker version of hidden_rec_items used by short-path. *)
let protected = ref S.empty
let add_protected id = protected := S.add (Ident.name id) !protected
let reset_protected () = protected := S.empty

let pervasives_name namespace name =
  if not !enabled then Out_name.create name else
  match M.find name (get namespace) with
  | Associated_to_pervasives r -> r
  | Need_unique_name _ -> Out_name.create (pervasives name)
  | Uniquely_associated_to (id',r) ->
      let hid, map = add_hid_id id' Ident.Map.empty in
      Out_name.set r (human_unique hid id');
      Conflicts.explain namespace hid id';
      set namespace @@ M.add name (Need_unique_name map) (get namespace);
      Out_name.create (pervasives name)
  | exception Not_found ->
      let r = Out_name.create name in
      set namespace @@ M.add name (Associated_to_pervasives r) (get namespace);
      r

(** Lookup for preexisting named item within the current {!printing_env} *)
let env_ident namespace name =
  if S.mem name !protected then None else
  match Namespace.lookup namespace name with
  | Pident id -> Some id
  | _ -> None
  | exception Not_found -> None

(** Associate a name to the identifier [id] within [namespace] *)
let ident_name_simple namespace id =
  if not !enabled then Out_name.create (Ident.name id) else
  let name = Ident.name id in
  match M.find name (get namespace) with
  | Uniquely_associated_to (id',r) when Ident.same id id' ->
      r
  | Need_unique_name map ->
      let hid, m = find_hid id map in
      Conflicts.explain namespace hid id;
      set namespace @@ M.add name (Need_unique_name m) (get namespace);
      Out_name.create (human_unique hid id)
  | Uniquely_associated_to (id',r) ->
      let hid', m = find_hid id' Ident.Map.empty in
      let hid, m = find_hid id m in
      Out_name.set r (human_unique hid' id');
      List.iter (fun (id,hid) -> Conflicts.explain namespace hid id)
        [id, hid; id', hid' ];
      set namespace @@ M.add name (Need_unique_name m) (get namespace);
      Out_name.create (human_unique hid id)
  | Associated_to_pervasives r ->
      Out_name.set r ("Stdlib." ^ Out_name.print r);
      let hid, m = find_hid id Ident.Map.empty in
      set namespace @@ M.add name (Need_unique_name m) (get namespace);
      Out_name.create (human_unique hid id)
  | exception Not_found ->
      let r = Out_name.create name in
      set namespace
      @@ M.add name (Uniquely_associated_to (id,r) ) (get namespace);
      r

(** Same as {!ident_name_simple} but lookup to existing named identifiers
    in the current {!printing_env} *)
let ident_name namespace id =
  begin match env_ident namespace (Ident.name id) with
  | Some id' -> ignore (ident_name_simple namespace id')
  | None -> ()
  end;
  ident_name_simple namespace id

let reset () =
  Array.iteri ( fun i _ -> map.(i) <- M.empty ) map

end
let ident_name = Naming_context.ident_name
let reset_naming_context = Naming_context.reset

let ident ppf id = pp_print_string ppf
    (Out_name.print (Naming_context.ident_name_simple Other id))

(* Print a path *)

let ident_stdlib = Ident.create_persistent "Stdlib"

let non_shadowed_pervasive = function
  | Pdot(Pident id, s) as path ->
      Ident.same id ident_stdlib &&
      (try Path.same path (Env.lookup_type (Lident s) !printing_env)
       with Not_found -> true)
  | _ -> false

let find_double_underscore s =
  let len = String.length s in
  let rec loop i =
    if i + 1 >= len then
      None
    else if s.[i] = '_' && s.[i + 1] = '_' then
      Some i
    else
      loop (i + 1)
  in
  loop 0

let rec module_path_is_an_alias_of env path ~alias_of =
  match Env.find_module path env with
  | { md_type = Mty_alias path'; _ } ->
    Path.same path' alias_of ||
    module_path_is_an_alias_of env path' ~alias_of
  | _ -> false
  | exception Not_found -> false

(* Simple heuristic to print Foo__bar.* as Foo.Bar.* when Foo.Bar is an alias
   for Foo__bar. This pattern is used by the stdlib. *)
let rec rewrite_double_underscore_paths env p =
  match p with
  | Pdot (p, s) ->
    Pdot (rewrite_double_underscore_paths env p, s)
  | Papply (a, b) ->
    Papply (rewrite_double_underscore_paths env a,
            rewrite_double_underscore_paths env b)
  | Pident id ->
    let name = Ident.name id in
    match find_double_underscore name with
    | None -> p
    | Some i ->
      let better_lid =
        Ldot
          (Lident (String.sub name 0 i),
           String.capitalize_ascii
             (String.sub name (i + 2) (String.length name - i - 2)))
      in
      match Env.lookup_module ~load:true better_lid env with
      | exception Not_found -> p
      | p' ->
        if module_path_is_an_alias_of env p' ~alias_of:p then
          p'
        else
          p

let rewrite_double_underscore_paths env p =
  if env == Env.empty then
    p
  else
    rewrite_double_underscore_paths env p

let rec tree_of_path namespace = function
  | Pident id ->
      Oide_ident (ident_name namespace id)
  | Pdot(_, s) as path when non_shadowed_pervasive path ->
      Oide_ident (Naming_context.pervasives_name namespace s)
  | Pdot(p, s) ->
      Oide_dot (tree_of_path Module p, s)
  | Papply(p1, p2) ->
      Oide_apply (tree_of_path Module p1, tree_of_path Module p2)

let tree_of_path namespace p =
  tree_of_path namespace (rewrite_double_underscore_paths !printing_env p)

let path ppf p =
  !Oprint.out_ident ppf (tree_of_path Other p)

let string_of_path p =
  Format.asprintf "%a" path p

let strings_of_paths namespace p =
  reset_naming_context ();
  let trees = List.map (tree_of_path namespace) p in
  List.map (Format.asprintf "%a" !Oprint.out_ident) trees

(* Print a recursive annotation *)

let tree_of_rec = function
  | Trec_not -> Orec_not
  | Trec_first -> Orec_first
  | Trec_next -> Orec_next

(* Print a raw type expression, with sharing *)

let raw_list pr ppf = function
    [] -> fprintf ppf "[]"
  | a :: l ->
      fprintf ppf "@[<1>[%a%t]@]" pr a
        (fun ppf -> List.iter (fun x -> fprintf ppf ";@,%a" pr x) l)

let kind_vars = ref []
let kind_count = ref 0

let rec safe_kind_repr v = function
    Fvar {contents=Some k}  ->
      if List.memq k v then "Fvar loop" else
      safe_kind_repr (k::v) k
  | Fvar r ->
      let vid =
        try List.assq r !kind_vars
        with Not_found ->
          let c = incr kind_count; !kind_count in
          kind_vars := (r,c) :: !kind_vars;
          c
      in
      Printf.sprintf "Fvar {None}@%d" vid
  | Fpresent -> "Fpresent"
  | Fabsent -> "Fabsent"

let rec safe_commu_repr v = function
    Cok -> "Cok"
  | Cunknown -> "Cunknown"
  | Clink r ->
      if List.memq r v then "Clink loop" else
      safe_commu_repr (r::v) !r

let rec safe_repr v = function
    {desc = Tlink t} when not (List.memq t v) ->
      safe_repr (t::v) t
  | t -> t

let rec list_of_memo = function
    Mnil -> []
  | Mcons (_priv, p, _t1, _t2, rem) -> p :: list_of_memo rem
  | Mlink rem -> list_of_memo !rem

let print_name ppf = function
    None -> fprintf ppf "None"
  | Some name -> fprintf ppf "\"%s\"" name

let string_of_label = function
    Nolabel -> ""
  | Labelled s -> s
  | Optional s -> "?"^s

let visited = ref []
let rec raw_type ppf ty =
  let ty = safe_repr [] ty in
  if List.memq ty !visited then fprintf ppf "{id=%d}" ty.id else begin
    visited := ty :: !visited;
    fprintf ppf "@[<1>{id=%d;level=%d;desc=@,%a}@]" ty.id ty.level
      raw_type_desc ty.desc
  end
and raw_type_list tl = raw_list raw_type tl
and raw_type_desc ppf = function
    Tvar name -> fprintf ppf "Tvar %a" print_name name
  | Tarrow(l,t1,t2,c) ->
      fprintf ppf "@[<hov1>Tarrow(\"%s\",@,%a,@,%a,@,%s)@]"
        (string_of_label l) raw_type t1 raw_type t2
        (safe_commu_repr [] c)
  | Ttuple tl ->
      fprintf ppf "@[<1>Ttuple@,%a@]" raw_type_list tl
  | Tconstr (p, tl, abbrev) ->
      fprintf ppf "@[<hov1>Tconstr(@,%a,@,%a,@,%a)@]" path p
        raw_type_list tl
        (raw_list path) (list_of_memo !abbrev)
  | Tobject (t, nm) ->
      fprintf ppf "@[<hov1>Tobject(@,%a,@,@[<1>ref%t@])@]" raw_type t
        (fun ppf ->
          match !nm with None -> fprintf ppf " None"
          | Some(p,tl) ->
              fprintf ppf "(Some(@,%a,@,%a))" path p raw_type_list tl)
  | Tfield (f, k, t1, t2) ->
      fprintf ppf "@[<hov1>Tfield(@,%s,@,%s,@,%a,@;<0 -1>%a)@]" f
        (safe_kind_repr [] k)
        raw_type t1 raw_type t2
  | Tnil -> fprintf ppf "Tnil"
  | Tlink t -> fprintf ppf "@[<1>Tlink@,%a@]" raw_type t
  | Tsubst t -> fprintf ppf "@[<1>Tsubst@,%a@]" raw_type t
  | Tunivar name -> fprintf ppf "Tunivar %a" print_name name
  | Tpoly (t, tl) ->
      fprintf ppf "@[<hov1>Tpoly(@,%a,@,%a)@]"
        raw_type t
        raw_type_list tl
  | Tvariant row ->
      fprintf ppf
        "@[<hov1>{@[%s@,%a;@]@ @[%s@,%a;@]@ %s%B;@ %s%B;@ @[<1>%s%t@]}@]"
        "row_fields="
        (raw_list (fun ppf (l, f) ->
          fprintf ppf "@[%s,@ %a@]" l raw_field f))
        row.row_fields
        "row_more=" raw_type row.row_more
        "row_closed=" row.row_closed
        "row_fixed=" row.row_fixed
        "row_name="
        (fun ppf ->
          match row.row_name with None -> fprintf ppf "None"
          | Some(p,tl) ->
              fprintf ppf "Some(@,%a,@,%a)" path p raw_type_list tl)
  | Tpackage (p, _, tl) ->
      fprintf ppf "@[<hov1>Tpackage(@,%a@,%a)@]" path p
        raw_type_list tl

and raw_field ppf = function
    Rpresent None -> fprintf ppf "Rpresent None"
  | Rpresent (Some t) -> fprintf ppf "@[<1>Rpresent(Some@,%a)@]" raw_type t
  | Reither (c,tl,m,e) ->
      fprintf ppf "@[<hov1>Reither(%B,@,%a,@,%B,@,@[<1>ref%t@])@]" c
        raw_type_list tl m
        (fun ppf ->
          match !e with None -> fprintf ppf " None"
          | Some f -> fprintf ppf "@,@[<1>(%a)@]" raw_field f)
  | Rabsent -> fprintf ppf "Rabsent"

let raw_type_expr ppf t =
  visited := []; kind_vars := []; kind_count := 0;
  raw_type ppf t;
  visited := []; kind_vars := []

let () = Btype.print_raw := raw_type_expr

(* Normalize paths *)

type param_subst = Id | Nth of int | Map of int list

let is_nth = function
    Nth _ -> true
  | _ -> false

let compose l1 = function
  | Id -> Map l1
  | Map l2 -> Map (List.map (List.nth l1) l2)
  | Nth n  -> Nth (List.nth l1 n)

let apply_subst s1 tyl =
  if tyl = [] then []
  (* cf. PR#7543: Typemod.type_package doesn't respect type constructor arity *)
  else
    match s1 with
      Nth n1 -> [List.nth tyl n1]
    | Map l1 -> List.map (List.nth tyl) l1
    | Id -> tyl

type best_path = Paths of Path.t list | Best of Path.t

let printing_depth = ref 0
let printing_cont = ref ([] : Env.iter_cont list)
let printing_old = ref Env.empty
let printing_pers = ref Concr.empty
let printing_map = ref Path.Map.empty

let same_type t t' = repr t == repr t'

let rec index l x =
  match l with
    [] -> raise Not_found
  | a :: l -> if x == a then 0 else 1 + index l x

let rec uniq = function
    [] -> true
  | a :: l -> not (List.memq a l) && uniq l

let rec normalize_type_path ?(cache=false) env p =
  try
    let (params, ty, _) = Env.find_type_expansion p env in
    let params = List.map repr params in
    match repr ty with
      {desc = Tconstr (p1, tyl, _)} ->
        let tyl = List.map repr tyl in
        if List.length params = List.length tyl
        && List.for_all2 (==) params tyl
        then normalize_type_path ~cache env p1
        else if cache || List.length params <= List.length tyl
             || not (uniq tyl) then (p, Id)
        else
          let l1 = List.map (index params) tyl in
          let (p2, s2) = normalize_type_path ~cache env p1 in
          (p2, compose l1 s2)
    | ty ->
        (p, Nth (index params ty))
  with
    Not_found ->
      (Env.normalize_type_path None env p, Id)

let penalty s =
  if s <> "" && s.[0] = '_' then
    10
  else
    match find_double_underscore s with
    | None -> 1
    | Some _ -> 10

let rec path_size = function
    Pident id ->
      penalty (Ident.name id), -Ident.scope id
  | Pdot (p, _) ->
      let (l, b) = path_size p in (1+l, b)
  | Papply (p1, p2) ->
      let (l, b) = path_size p1 in
      (l + fst (path_size p2), b)

let same_printing_env env =
  let used_pers = Env.used_persistent () in
  Env.same_types !printing_old env && Concr.equal !printing_pers used_pers

let set_printing_env env =
  printing_env := env;
  if !Clflags.real_paths ||
     !printing_env == Env.empty ||
     same_printing_env env then
    ()
  else begin
    (* printf "Reset printing_map@."; *)
    printing_old := env;
    printing_pers := Env.used_persistent ();
    printing_map := Path.Map.empty;
    printing_depth := 0;
    (* printf "Recompute printing_map.@."; *)
    let cont =
      Env.iter_types
        (fun p (p', _decl) ->
          let (p1, s1) = normalize_type_path env p' ~cache:true in
          (* Format.eprintf "%a -> %a = %a@." path p path p' path p1 *)
          if s1 = Id then
          try
            let r = Path.Map.find p1 !printing_map in
            match !r with
              Paths l -> r := Paths (p :: l)
            | Best p' -> r := Paths [p; p'] (* assert false *)
          with Not_found ->
            printing_map := Path.Map.add p1 (ref (Paths [p])) !printing_map)
        env in
    printing_cont := [cont];
  end

let wrap_printing_env env f =
  set_printing_env env; reset_naming_context ();
  try_finally f ~always:(fun () -> set_printing_env Env.empty)

let wrap_printing_env ~error env f =
  if error then Env.without_cmis (wrap_printing_env env) f
  else wrap_printing_env env f

let is_unambiguous path env =
  let l = Env.find_shadowed_types path env in
  List.exists (Path.same path) l || (* concrete paths are ok *)
  match l with
    [] -> true
  | p :: rem ->
      (* allow also coherent paths:  *)
      let normalize p = fst (normalize_type_path ~cache:true env p) in
      let p' = normalize p in
      List.for_all (fun p -> Path.same (normalize p) p') rem ||
      (* also allow repeatedly defining and opening (for toplevel) *)
      let id = lid_of_path p in
      List.for_all (fun p -> lid_of_path p = id) rem &&
      Path.same p (Env.lookup_type id env)

let rec get_best_path r =
  match !r with
    Best p' -> p'
  | Paths [] -> raise Not_found
  | Paths l ->
      r := Paths [];
      List.iter
        (fun p ->
          (* Format.eprintf "evaluating %a@." path p; *)
          match !r with
            Best p' when path_size p >= path_size p' -> ()
          | _ -> if is_unambiguous p !printing_env then r := Best p)
              (* else Format.eprintf "%a ignored as ambiguous@." path p *)
        l;
      get_best_path r

let best_type_path p =
  if !printing_env == Env.empty
  then (p, Id)
  else if !Clflags.real_paths
  then (p, Id)
  else
    let (p', s) = normalize_type_path !printing_env p in
    let get_path () = get_best_path (Path.Map.find  p' !printing_map) in
    while !printing_cont <> [] &&
      try fst (path_size (get_path ())) > !printing_depth with Not_found -> true
    do
      printing_cont := List.map snd (Env.run_iter_cont !printing_cont);
      incr printing_depth;
    done;
    let p'' = try get_path () with Not_found -> p' in
    (* Format.eprintf "%a = %a -> %a@." path p path p' path p''; *)
    (p'', s)

(* Print a type expression *)

let names = ref ([] : (type_expr * string) list)
let name_counter = ref 0
let named_vars = ref ([] : string list)

let weak_counter = ref 1
let weak_var_map = ref TypeMap.empty
let named_weak_vars = ref String.Set.empty

let reset_names () = names := []; name_counter := 0; named_vars := []
let add_named_var ty =
  match ty.desc with
    Tvar (Some name) | Tunivar (Some name) ->
      if List.mem name !named_vars then () else
      named_vars := name :: !named_vars
  | _ -> ()

let name_is_already_used name =
  List.mem name !named_vars
  || List.exists (fun (_, name') -> name = name') !names
  || String.Set.mem name !named_weak_vars

let rec new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter))
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           Int.to_string(!name_counter / 26) in
  incr name_counter;
  if name_is_already_used name then new_name () else name

let rec new_weak_name ty () =
  let name = "weak" ^ Int.to_string !weak_counter in
  incr weak_counter;
  if name_is_already_used name then new_weak_name ty ()
  else begin
      named_weak_vars := String.Set.add name !named_weak_vars;
      weak_var_map := TypeMap.add ty name !weak_var_map;
      name
    end

let name_of_type name_generator t =
  (* We've already been through repr at this stage, so t is our representative
     of the union-find class. *)
  try List.assq t !names with Not_found ->
    try TypeMap.find t !weak_var_map with Not_found ->
    let name =
      match t.desc with
        Tvar (Some name) | Tunivar (Some name) ->
          (* Some part of the type we've already printed has assigned another
           * unification variable to that name. We want to keep the name, so try
           * adding a number until we find a name that's not taken. *)
          let current_name = ref name in
          let i = ref 0 in
          while List.exists (fun (_, name') -> !current_name = name') !names do
            current_name := name ^ (Int.to_string !i);
            i := !i + 1;
          done;
          !current_name
      | _ ->
          (* No name available, create a new one *)
          name_generator ()
    in
    (* Exception for type declarations *)
    if name <> "_" then names := (t, name) :: !names;
    name

let check_name_of_type t = ignore(name_of_type new_name t)

let remove_names tyl =
  let tyl = List.map repr tyl in
  names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !names

let visited_objects = ref ([] : type_expr list)
let aliased = ref ([] : type_expr list)
let delayed = ref ([] : type_expr list)

let add_delayed t =
  if not (List.memq t !delayed) then delayed := t :: !delayed

let is_aliased ty = List.memq (proxy ty) !aliased
let add_alias ty =
  let px = proxy ty in
  if not (is_aliased px) then begin
    aliased := px :: !aliased;
    add_named_var px
  end

let aliasable ty =
  match ty.desc with
    Tvar _ | Tunivar _ | Tpoly _ -> false
  | Tconstr (p, _, _) ->
      not (is_nth (snd (best_type_path p)))
  | _ -> true

let namable_row row =
  row.row_name <> None &&
  List.for_all
    (fun (_, f) ->
       match row_field_repr f with
       | Reither(c, l, _, _) ->
           row.row_closed && if c then l = [] else List.length l = 1
       | _ -> true)
    row.row_fields

let rec mark_loops_rec visited ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.memq px visited && aliasable ty then add_alias px else
    let visited = px :: visited in
    match ty.desc with
    | Tvar _ -> add_named_var ty
    | Tarrow(_, ty1, ty2, _) ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Ttuple tyl -> List.iter (mark_loops_rec visited) tyl
    | Tconstr(p, tyl, _) ->
        let (_p', s) = best_type_path p in
        List.iter (mark_loops_rec visited) (apply_subst s tyl)
    | Tpackage (_, _, tyl) ->
        List.iter (mark_loops_rec visited) tyl
    | Tvariant row ->
        if List.memq px !visited_objects then add_alias px else
         begin
          let row = row_repr row in
          if not (static_row row) then
            visited_objects := px :: !visited_objects;
          match row.row_name with
          | Some(_p, tyl) when namable_row row ->
              List.iter (mark_loops_rec visited) tyl
          | _ ->
              iter_row (mark_loops_rec visited) row
         end
    | Tobject (fi, nm) ->
        if List.memq px !visited_objects then add_alias px else
         begin
          if opened_object ty then
            visited_objects := px :: !visited_objects;
          begin match !nm with
          | None ->
              let fields, _ = flatten_fields fi in
              List.iter
                (fun (_, kind, ty) ->
                  if field_kind_repr kind = Fpresent then
                    mark_loops_rec visited ty)
                fields
          | Some (_, l) ->
              List.iter (mark_loops_rec visited) (List.tl l)
          end
        end
    | Tfield(_, kind, ty1, ty2) when field_kind_repr kind = Fpresent ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Tfield(_, _, _, ty2) ->
        mark_loops_rec visited ty2
    | Tnil -> ()
    | Tsubst ty -> mark_loops_rec visited ty
    | Tlink _ -> fatal_error "Printtyp.mark_loops_rec (2)"
    | Tpoly (ty, tyl) ->
        List.iter (fun t -> add_alias t) tyl;
        mark_loops_rec visited ty
    | Tunivar _ -> add_named_var ty

let mark_loops ty =
  normalize_type Env.empty ty;
  mark_loops_rec [] ty;;

let reset_loop_marks () =
  visited_objects := []; aliased := []; delayed := []

let reset_except_context () =
  reset_names (); reset_loop_marks ()

let reset () =
  reset_naming_context (); Conflicts.reset ();
  reset_except_context ()

let reset_and_mark_loops ty =
  reset_except_context (); mark_loops ty

let reset_and_mark_loops_list tyl =
  reset_except_context (); List.iter mark_loops tyl

(* Disabled in classic mode when printing an unification error *)
let print_labels = ref true

let rec tree_of_typexp sch ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.mem_assq px !names && not (List.memq px !delayed) then
   let mark = is_non_gen sch ty in
   let name = name_of_type (if mark then new_weak_name ty else new_name) px in
   Otyp_var (mark, name) else

  let pr_typ () =
    match ty.desc with
    | Tvar _ ->
        (*let lev =
          if is_non_gen sch ty then "/" ^ Int.to_string ty.level else "" in*)
        let non_gen = is_non_gen sch ty in
        let name_gen = if non_gen then new_weak_name ty else new_name in
        Otyp_var (non_gen, name_of_type name_gen ty)
    | Tarrow(l, ty1, ty2, _) ->
        let pr_arrow l ty1 ty2 =
          let lab =
            if !print_labels || is_optional l then string_of_label l else ""
          in
          let t1 =
            if is_optional l then
              match (repr ty1).desc with
              | Tconstr(path, [ty], _)
                when Path.same path Predef.path_option ->
                  tree_of_typexp sch ty
              | _ -> Otyp_stuff "<hidden>"
            else tree_of_typexp sch ty1 in
          Otyp_arrow (lab, t1, tree_of_typexp sch ty2) in
        pr_arrow l ty1 ty2
    | Ttuple tyl ->
        Otyp_tuple (tree_of_typlist sch tyl)
    | Tconstr(p, tyl, _abbrev) ->
        let p', s = best_type_path p in
        let tyl' = apply_subst s tyl in
        if is_nth s && not (tyl'=[]) then tree_of_typexp sch (List.hd tyl') else
        Otyp_constr (tree_of_path Type p', tree_of_typlist sch tyl')
    | Tvariant row ->
        let row = row_repr row in
        let fields =
          if row.row_closed then
            List.filter (fun (_, f) -> row_field_repr f <> Rabsent)
              row.row_fields
          else row.row_fields in
        let present =
          List.filter
            (fun (_, f) ->
               match row_field_repr f with
               | Rpresent _ -> true
               | _ -> false)
            fields in
        let all_present = List.length present = List.length fields in
        begin match row.row_name with
        | Some(p, tyl) when namable_row row ->
            let (p', s) = best_type_path p in
            let id = tree_of_path Type p' in
            let args = tree_of_typlist sch (apply_subst s tyl) in
            let out_variant =
              if is_nth s then List.hd args else Otyp_constr (id, args) in
            if row.row_closed && all_present then
              out_variant
            else
              let non_gen = is_non_gen sch px in
              let tags =
                if all_present then None else Some (List.map fst present) in
              Otyp_variant (non_gen, Ovar_typ out_variant, row.row_closed, tags)
        | _ ->
            let non_gen =
              not (row.row_closed && all_present) && is_non_gen sch px in
            let fields = List.map (tree_of_row_field sch) fields in
            let tags =
              if all_present then None else Some (List.map fst present) in
            Otyp_variant (non_gen, Ovar_fields fields, row.row_closed, tags)
        end
    | Tobject (fi, nm) ->
        tree_of_typobject sch fi !nm
    | Tnil | Tfield _ ->
        tree_of_typobject sch ty None
    | Tsubst ty ->
        tree_of_typexp sch ty
    | Tlink _ ->
        fatal_error "Printtyp.tree_of_typexp"
    | Tpoly (ty, []) ->
        tree_of_typexp sch ty
    | Tpoly (ty, tyl) ->
        (*let print_names () =
          List.iter (fun (_, name) -> prerr_string (name ^ " ")) !names;
          prerr_string "; " in *)
        let tyl = List.map repr tyl in
        if tyl = [] then tree_of_typexp sch ty else begin
          let old_delayed = !delayed in
          (* Make the names delayed, so that the real type is
             printed once when used as proxy *)
          List.iter add_delayed tyl;
          let tl = List.map (name_of_type new_name) tyl in
          let tr = Otyp_poly (tl, tree_of_typexp sch ty) in
          (* Forget names when we leave scope *)
          remove_names tyl;
          delayed := old_delayed; tr
        end
    | Tunivar _ ->
        Otyp_var (false, name_of_type new_name ty)
    | Tpackage (p, n, tyl) ->
        let n =
          List.map (fun li -> String.concat "." (Longident.flatten li)) n in
        Otyp_module (tree_of_path Module_type p, n, tree_of_typlist sch tyl)
  in
  if List.memq px !delayed then delayed := List.filter ((!=) px) !delayed;
  if is_aliased px && aliasable ty then begin
    check_name_of_type px;
    Otyp_alias (pr_typ (), name_of_type new_name px) end
  else pr_typ ()

and tree_of_row_field sch (l, f) =
  match row_field_repr f with
  | Rpresent None | Reither(true, [], _, _) -> (l, false, [])
  | Rpresent(Some ty) -> (l, false, [tree_of_typexp sch ty])
  | Reither(c, tyl, _, _) ->
      if c (* contradiction: constant constructor with an argument *)
      then (l, true, tree_of_typlist sch tyl)
      else (l, false, tree_of_typlist sch tyl)
  | Rabsent -> (l, false, [] (* actually, an error *))

and tree_of_typlist sch tyl =
  List.map (tree_of_typexp sch) tyl

and tree_of_typobject sch fi nm =
  begin match nm with
  | None ->
      let pr_fields fi =
        let (fields, rest) = flatten_fields fi in
        let present_fields =
          List.fold_right
            (fun (n, k, t) l ->
               match field_kind_repr k with
               | Fpresent -> (n, t) :: l
               | _ -> l)
            fields [] in
        let sorted_fields =
          List.sort
            (fun (n, _) (n', _) -> String.compare n n') present_fields in
        tree_of_typfields sch rest sorted_fields in
      let (fields, rest) = pr_fields fi in
      Otyp_object (fields, rest)
  | Some (p, ty :: tyl) ->
      let non_gen = is_non_gen sch (repr ty) in
      let args = tree_of_typlist sch tyl in
      let (p', s) = best_type_path p in
      assert (s = Id);
      Otyp_class (non_gen, tree_of_path Type p', args)
  | _ ->
      fatal_error "Printtyp.tree_of_typobject"
  end

and is_non_gen sch ty =
    sch && is_Tvar ty && ty.level <> generic_level

and tree_of_typfields sch rest = function
  | [] ->
      let rest =
        match rest.desc with
        | Tvar _ | Tunivar _ -> Some (is_non_gen sch rest)
        | Tconstr _ -> Some false
        | Tnil -> None
        | _ -> fatal_error "typfields (1)"
      in
      ([], rest)
  | (s, t) :: l ->
      let field = (s, tree_of_typexp sch t) in
      let (fields, rest) = tree_of_typfields sch rest l in
      (field :: fields, rest)

let typexp sch ppf ty =
  !Oprint.out_type ppf (tree_of_typexp sch ty)

let type_expr ppf ty = typexp false ppf ty

and type_sch ppf ty = typexp true ppf ty

and type_scheme ppf ty = reset_and_mark_loops ty; typexp true ppf ty

(* Maxence *)
let type_scheme_max ?(b_reset_names=true) ppf ty =
  if b_reset_names then reset_names () ;
  typexp true ppf ty
(* End Maxence *)

let tree_of_type_scheme ty = reset_and_mark_loops ty; tree_of_typexp true ty

(* Print one type declaration *)

let tree_of_constraints params =
  List.fold_right
    (fun ty list ->
       let ty' = unalias ty in
       if proxy ty != proxy ty' then
         let tr = tree_of_typexp true ty in
         (tr, tree_of_typexp true ty') :: list
       else list)
    params []

let filter_params tyl =
  let params =
    List.fold_left
      (fun tyl ty ->
        let ty = repr ty in
        if List.memq ty tyl then Btype.newgenty (Tsubst ty) :: tyl
        else ty :: tyl)
      [] tyl
  in List.rev params

let mark_loops_constructor_arguments = function
  | Cstr_tuple l -> List.iter mark_loops l
  | Cstr_record l -> List.iter (fun l -> mark_loops l.ld_type) l

let rec tree_of_type_decl id decl =

  reset_except_context();

  let params = filter_params decl.type_params in

  begin match decl.type_manifest with
  | Some ty ->
      let vars = free_variables ty in
      List.iter
        (function {desc = Tvar (Some "_")} as ty ->
            if List.memq ty vars then ty.desc <- Tvar None
          | _ -> ())
        params
  | None -> ()
  end;

  List.iter add_alias params;
  List.iter mark_loops params;
  List.iter check_name_of_type (List.map proxy params);
  let ty_manifest =
    match decl.type_manifest with
    | None -> None
    | Some ty ->
        let ty =
          (* Special hack to hide variant name *)
          match repr ty with {desc=Tvariant row} ->
            let row = row_repr row in
            begin match row.row_name with
              Some (Pident id', _) when Ident.same id id' ->
                newgenty (Tvariant {row with row_name = None})
            | _ -> ty
            end
          | _ -> ty
        in
        mark_loops ty;
        Some ty
  in
  begin match decl.type_kind with
  | Type_abstract -> ()
  | Type_variant cstrs ->
      List.iter
        (fun c ->
           mark_loops_constructor_arguments c.cd_args;
           may mark_loops c.cd_res)
        cstrs
  | Type_record(l, _rep) ->
      List.iter (fun l -> mark_loops l.ld_type) l
  | Type_open -> ()
  end;

  let type_param =
    function
    | Otyp_var (_, id) -> id
    | _ -> "?"
  in
  let type_defined decl =
    let abstr =
      match decl.type_kind with
        Type_abstract ->
          decl.type_manifest = None || decl.type_private = Private
      | Type_record _ ->
          decl.type_private = Private
      | Type_variant tll ->
          decl.type_private = Private ||
          List.exists (fun cd -> cd.cd_res <> None) tll
      | Type_open ->
          decl.type_manifest = None
    in
    let vari =
      List.map2
        (fun ty v ->
          if abstr || not (is_Tvar (repr ty)) then Variance.get_upper v
          else (true,true))
        decl.type_params decl.type_variance
    in
    (Ident.name id,
     List.map2 (fun ty cocn -> type_param (tree_of_typexp false ty), cocn)
       params vari)
  in
  let tree_of_manifest ty1 =
    match ty_manifest with
    | None -> ty1
    | Some ty -> Otyp_manifest (tree_of_typexp false ty, ty1)
  in
  let (name, args) = type_defined decl in
  let constraints = tree_of_constraints params in
  let ty, priv =
    match decl.type_kind with
    | Type_abstract ->
        begin match ty_manifest with
        | None -> (Otyp_abstract, Public)
        | Some ty ->
            tree_of_typexp false ty, decl.type_private
        end
    | Type_variant cstrs ->
        tree_of_manifest (Otyp_sum (List.map tree_of_constructor cstrs)),
        decl.type_private
    | Type_record(lbls, _rep) ->
        tree_of_manifest (Otyp_record (List.map tree_of_label lbls)),
        decl.type_private
    | Type_open ->
        tree_of_manifest Otyp_open,
        decl.type_private
  in
  let immediate =
    Builtin_attributes.immediate decl.type_attributes
  in
    { otype_name = name;
      otype_params = args;
      otype_type = ty;
      otype_private = priv;
      otype_immediate = immediate;
      otype_unboxed = decl.type_unboxed.unboxed;
      otype_cstrs = constraints }

and tree_of_constructor_arguments = function
  | Cstr_tuple l -> tree_of_typlist false l
  | Cstr_record l -> [ Otyp_record (List.map tree_of_label l) ]

and tree_of_constructor cd =
  let name = Ident.name cd.cd_id in
  let arg () = tree_of_constructor_arguments cd.cd_args in
  match cd.cd_res with
  | None -> (name, arg (), None)
  | Some res ->
      let nm = !names in
      names := [];
      let ret = tree_of_typexp false res in
      let args = arg () in
      names := nm;
      (name, args, Some ret)

and tree_of_label l =
  (Ident.name l.ld_id, l.ld_mutable = Mutable, tree_of_typexp false l.ld_type)

let tree_of_type_declaration id decl rs =
  Osig_type (tree_of_type_decl id decl, tree_of_rec rs)

let type_declaration id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_type_declaration id decl Trec_first)

let constructor_arguments ppf a =
  let tys = tree_of_constructor_arguments a in
  !Oprint.out_type ppf (Otyp_tuple tys)

(* Print an extension declaration *)

let tree_of_extension_constructor id ext es =
  reset_except_context ();
  let ty_name = Path.name ext.ext_type_path in
  let ty_params = filter_params ext.ext_type_params in
  List.iter add_alias ty_params;
  List.iter mark_loops ty_params;
  List.iter check_name_of_type (List.map proxy ty_params);
  mark_loops_constructor_arguments ext.ext_args;
  may mark_loops ext.ext_ret_type;
  let type_param =
    function
    | Otyp_var (_, id) -> id
    | _ -> "?"
  in
  let ty_params =
    List.map (fun ty -> type_param (tree_of_typexp false ty)) ty_params
  in
  let name = Ident.name id in
  let args, ret =
    match ext.ext_ret_type with
    | None -> (tree_of_constructor_arguments ext.ext_args, None)
    | Some res ->
        let nm = !names in
        names := [];
        let ret = tree_of_typexp false res in
        let args = tree_of_constructor_arguments ext.ext_args in
        names := nm;
        (args, Some ret)
  in
  let ext =
    { oext_name = name;
      oext_type_name = ty_name;
      oext_type_params = ty_params;
      oext_args = args;
      oext_ret_type = ret;
      oext_private = ext.ext_private }
  in
  let es =
    match es with
        Text_first -> Oext_first
      | Text_next -> Oext_next
      | Text_exception -> Oext_exception
  in
    Osig_typext (ext, es)

let extension_constructor id ppf ext =
  !Oprint.out_sig_item ppf (tree_of_extension_constructor id ext Text_first)

(* Print a value declaration *)

let tree_of_value_description id decl =
  (* Format.eprintf "@[%a@]@." raw_type_expr decl.val_type; *)
  let id = Ident.name id in
  let ty = tree_of_type_scheme decl.val_type in
  let vd =
    { oval_name = id;
      oval_type = ty;
      oval_prims = [];
      oval_attributes = [] }
  in
  let vd =
    match decl.val_kind with
    | Val_prim p -> Primitive.print p vd
    | _ -> vd
  in
  Osig_value vd

let value_description id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_value_description id decl)

(* Print a class type *)

let method_type (_, kind, ty) =
  match field_kind_repr kind, repr ty with
    Fpresent, {desc=Tpoly(ty, tyl)} -> (ty, tyl)
  | _       , ty                    -> (ty, [])

let tree_of_metho sch concrete csil (lab, kind, ty) =
  if lab <> dummy_method then begin
    let kind = field_kind_repr kind in
    let priv = kind <> Fpresent in
    let virt = not (Concr.mem lab concrete) in
    let (ty, tyl) = method_type (lab, kind, ty) in
    let tty = tree_of_typexp sch ty in
    remove_names tyl;
    Ocsg_method (lab, priv, virt, tty) :: csil
  end
  else csil

let rec prepare_class_type params = function
  | Cty_constr (_p, tyl, cty) ->
      let sty = Ctype.self_type cty in
      if List.memq (proxy sty) !visited_objects
      || not (List.for_all is_Tvar params)
      || List.exists (deep_occur sty) tyl
      then prepare_class_type params cty
      else List.iter mark_loops tyl
  | Cty_signature sign ->
      let sty = repr sign.csig_self in
      (* Self may have a name *)
      let px = proxy sty in
      if List.memq px !visited_objects then add_alias sty
      else visited_objects := px :: !visited_objects;
      let (fields, _) =
        Ctype.flatten_fields (Ctype.object_fields sign.csig_self)
      in
      List.iter (fun met -> mark_loops (fst (method_type met))) fields;
      Vars.iter (fun _ (_, _, ty) -> mark_loops ty) sign.csig_vars
  | Cty_arrow (_, ty, cty) ->
      mark_loops ty;
      prepare_class_type params cty

let rec tree_of_class_type sch params =
  function
  | Cty_constr (p', tyl, cty) ->
      let sty = Ctype.self_type cty in
      if List.memq (proxy sty) !visited_objects
      || not (List.for_all is_Tvar params)
      then
        tree_of_class_type sch params cty
      else
        let namespace = Namespace.best_class_namespace p' in
        Octy_constr (tree_of_path namespace p', tree_of_typlist true tyl)
  | Cty_signature sign ->
      let sty = repr sign.csig_self in
      let self_ty =
        if is_aliased sty then
          Some (Otyp_var (false, name_of_type new_name (proxy sty)))
        else None
      in
      let (fields, _) =
        Ctype.flatten_fields (Ctype.object_fields sign.csig_self)
      in
      let csil = [] in
      let csil =
        List.fold_left
          (fun csil (ty1, ty2) -> Ocsg_constraint (ty1, ty2) :: csil)
          csil (tree_of_constraints params)
      in
      let all_vars =
        Vars.fold (fun l (m, v, t) all -> (l, m, v, t) :: all) sign.csig_vars []
      in
      (* Consequence of PR#3607: order of Map.fold has changed! *)
      let all_vars = List.rev all_vars in
      let csil =
        List.fold_left
          (fun csil (l, m, v, t) ->
            Ocsg_value (l, m = Mutable, v = Virtual, tree_of_typexp sch t)
            :: csil)
          csil all_vars
      in
      let csil =
        List.fold_left (tree_of_metho sch sign.csig_concr) csil fields
      in
      Octy_signature (self_ty, List.rev csil)
  | Cty_arrow (l, ty, cty) ->
      let lab =
        if !print_labels || is_optional l then string_of_label l else ""
      in
      let tr =
       if is_optional l then
         match (repr ty).desc with
         | Tconstr(path, [ty], _) when Path.same path Predef.path_option ->
             tree_of_typexp sch ty
         | _ -> Otyp_stuff "<hidden>"
       else tree_of_typexp sch ty in
      Octy_arrow (lab, tr, tree_of_class_type sch params cty)

let class_type ppf cty =
  reset ();
  prepare_class_type [] cty;
  !Oprint.out_class_type ppf (tree_of_class_type false [] cty)

let tree_of_class_param param variance =
  (match tree_of_typexp true param with
    Otyp_var (_, s) -> s
  | _ -> "?"),
  if is_Tvar (repr param) then (true, true) else variance

let class_variance =
  List.map Variance.(fun v -> mem May_pos v, mem May_neg v)

let tree_of_class_declaration id cl rs =
  let params = filter_params cl.cty_params in

  reset_except_context ();
  List.iter add_alias params;
  prepare_class_type params cl.cty_type;
  let sty = Ctype.self_type cl.cty_type in
  List.iter mark_loops params;

  List.iter check_name_of_type (List.map proxy params);
  if is_aliased sty then check_name_of_type (proxy sty);

  let vir_flag = cl.cty_new = None in
  Osig_class
    (vir_flag, Ident.name id,
     List.map2 tree_of_class_param params (class_variance cl.cty_variance),
     tree_of_class_type true params cl.cty_type,
     tree_of_rec rs)

let class_declaration id ppf cl =
  !Oprint.out_sig_item ppf (tree_of_class_declaration id cl Trec_first)

let tree_of_cltype_declaration id cl rs =
  let params = List.map repr cl.clty_params in

  reset_except_context ();
  List.iter add_alias params;
  prepare_class_type params cl.clty_type;
  let sty = Ctype.self_type cl.clty_type in
  List.iter mark_loops params;

  List.iter check_name_of_type (List.map proxy params);
  if is_aliased sty then check_name_of_type (proxy sty);

  let sign = Ctype.signature_of_class_type cl.clty_type in

  let virt =
    let (fields, _) =
      Ctype.flatten_fields (Ctype.object_fields sign.csig_self) in
    List.exists
      (fun (lab, _, _) ->
         not (lab = dummy_method || Concr.mem lab sign.csig_concr))
      fields
    || Vars.fold (fun _ (_,vr,_) b -> vr = Virtual || b) sign.csig_vars false
  in

  Osig_class_type
    (virt, Ident.name id,
     List.map2 tree_of_class_param params (class_variance cl.clty_variance),
     tree_of_class_type true params cl.clty_type,
     tree_of_rec rs)

let cltype_declaration id ppf cl =
  !Oprint.out_sig_item ppf (tree_of_cltype_declaration id cl Trec_first)

(* Print a module type *)

let wrap_env fenv ftree arg =
  let env = !printing_env in
  set_printing_env (fenv env);
  let tree = ftree arg in
  set_printing_env env;
  tree

let filter_rem_sig item rem =
  match item, rem with
  | Sig_class _, ctydecl :: tydecl1 :: tydecl2 :: rem ->
      ([ctydecl; tydecl1; tydecl2], rem)
  | Sig_class_type _, tydecl1 :: tydecl2 :: rem ->
      ([tydecl1; tydecl2], rem)
  | _ ->
      ([], rem)

let dummy =
  { type_params = []; type_arity = 0; type_kind = Type_abstract;
    type_private = Public; type_manifest = None; type_variance = [];
    type_is_newtype = false; type_expansion_scope = Btype.lowest_level;
    type_loc = Location.none;
    type_attributes = [];
    type_immediate = false;
    type_unboxed = unboxed_false_default_false;
  }

let hide ids env = List.fold_right
    (fun id -> Env.add_type ~check:false (Ident.rename id) dummy)
    ids env

let hide_rec_items = function
  | Sig_type(id, _decl, rs, _) ::rem
    when rs = Trec_first && not !Clflags.real_paths ->
      let rec get_ids = function
          Sig_type (id, _, Trec_next, _) :: rem ->
            id :: get_ids rem
        | _ -> []
      in
      let ids = id :: get_ids rem in
      set_printing_env
        (hide ids !printing_env)
  | _ -> ()

let recursive_sigitem = function
  | Sig_class(id,_,rs,_) -> Some(id,rs,3)
  | Sig_class_type (id,_,rs,_) -> Some(id,rs,2)
  | Sig_type(id, _, rs, _)
  | Sig_module(id, _, _, rs, _) -> Some (id,rs,0)
  | _ -> None

let skip k l = snd (Misc.Stdlib.List.split_at k l)

let protect_rec_items items =
  let rec get_ids recs = function
    | [] -> []
    | item :: rem -> match recursive_sigitem item with
      | Some (id, r, k ) when r = recs -> id :: get_ids Trec_next (skip k rem)
      | _ -> [] in
  List.iter Naming_context.add_protected (get_ids Trec_first items)

let still_in_type_group env' in_type_group item =
  match in_type_group, recursive_sigitem item with
    true, Some (_,Trec_next,_) -> true
  | _, Some (_, (Trec_not | Trec_first),_) ->
      Naming_context.reset_protected ();
      set_printing_env env'; true
  | _ -> Naming_context.reset_protected (); set_printing_env env'; false

let rec tree_of_modtype ?(ellipsis=false) = function
  | Mty_ident p ->
      Omty_ident (tree_of_path Module_type p)
  | Mty_signature sg ->
      Omty_signature (if ellipsis then [Osig_ellipsis]
                      else tree_of_signature sg)
  | Mty_functor(param, ty_arg, ty_res) ->
      let res =
        match ty_arg with None -> tree_of_modtype ~ellipsis ty_res
        | Some mty ->
            wrap_env (Env.add_module ~arg:true param Mp_present mty)
                     (tree_of_modtype ~ellipsis) ty_res
      in
      Omty_functor (Ident.name param,
                    may_map (tree_of_modtype ~ellipsis:false) ty_arg, res)
  | Mty_alias p ->
      Omty_alias (tree_of_path Module p)

and tree_of_signature sg =
  wrap_env (fun env -> env) (tree_of_signature_rec !printing_env false) sg

and tree_of_signature_rec env' in_type_group = function
    [] -> []
  | item :: rem as items ->
      let in_type_group = still_in_type_group env' in_type_group item in
      let (sg, rem) = filter_rem_sig item rem in
      hide_rec_items items;
      protect_rec_items items;
      reset_naming_context ();
      let trees = trees_of_sigitem item in
      let env' = Env.add_signature (item :: sg) env' in
      trees @ tree_of_signature_rec env' in_type_group rem

and trees_of_sigitem = function
  | Sig_value(id, decl, _) ->
      [tree_of_value_description id decl]
  | Sig_type(id, _, _, _) when is_row_name (Ident.name id) ->
      []
  | Sig_type(id, decl, rs, _) ->
      [tree_of_type_declaration id decl rs]
  | Sig_typext(id, ext, es, _) ->
      [tree_of_extension_constructor id ext es]
  | Sig_module(id, _, md, rs, _) ->
      let ellipsis =
        List.exists (function
          | Parsetree.{attr_name = {txt="..."}; attr_payload = PStr []} -> true
          | _ -> false)
          md.md_attributes in
      [tree_of_module id md.md_type rs ~ellipsis]
  | Sig_modtype(id, decl, _) ->
      [tree_of_modtype_declaration id decl]
  | Sig_class(id, decl, rs, _) ->
      [tree_of_class_declaration id decl rs]
  | Sig_class_type(id, decl, rs, _) ->
      [tree_of_cltype_declaration id decl rs]

and tree_of_modtype_declaration id decl =
  let mty =
    match decl.mtd_type with
    | None -> Omty_abstract
    | Some mty -> tree_of_modtype mty
  in
  Osig_modtype (Ident.name id, mty)

and tree_of_module id ?ellipsis mty rs =
  Osig_module (Ident.name id, tree_of_modtype ?ellipsis mty, tree_of_rec rs)

let modtype ppf mty = !Oprint.out_module_type ppf (tree_of_modtype mty)
let modtype_declaration id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_modtype_declaration id decl)

(* For the toplevel: merge with tree_of_signature? *)

(* Refresh weak variable map in the toplevel *)
let refresh_weak () =
  let refresh t name (m,s) =
    if is_non_gen true (repr t) then
      begin
        TypeMap.add t name m,
        String.Set.add name s
      end
    else m, s in
  let m, s =
    TypeMap.fold refresh !weak_var_map (TypeMap.empty ,String.Set.empty)  in
  named_weak_vars := s;
  weak_var_map := m

let print_items showval env x =
  refresh_weak();
  reset_naming_context ();
  Conflicts.reset ();
  let rec print showval in_type_group env = function
  | [] -> []
  | item :: rem as items ->
      let in_type_group = still_in_type_group env in_type_group item in
      let (sg, rem) = filter_rem_sig item rem in
      hide_rec_items items;
      protect_rec_items items;
      reset_naming_context ();
      let trees = trees_of_sigitem item in
      List.map (fun d -> (d, showval env item)) trees @
      print showval in_type_group (Env.add_signature (item :: sg) env) rem in
  print showval false env x

(* Print a signature body (used by -i when compiling a .ml) *)

let print_signature ppf tree =
  fprintf ppf "@[<v>%a@]" !Oprint.out_signature tree

let signature ppf sg =
  fprintf ppf "%a" print_signature (tree_of_signature sg)

(* Print a signature body (used by -i when compiling a .ml) *)
let printed_signature sourcefile ppf sg =
  (* we are tracking any collision event for warning 63 *)
  Conflicts.reset ();
  reset_naming_context ();
  let t = tree_of_signature sg in
  if Warnings.(is_active @@ Erroneous_printed_signature "")
  && Conflicts.exists ()
  then begin
    let conflicts = Format.asprintf "%t" Conflicts.print in
    Location.prerr_warning (Location.in_file sourcefile)
      (Warnings.Erroneous_printed_signature conflicts);
    Warnings.check_fatal ()
  end;
  fprintf ppf "%a" print_signature t

(* Print an unification error *)

let same_path t t' =
  let t = repr t and t' = repr t' in
  t == t' ||
  match t.desc, t'.desc with
    Tconstr(p,tl,_), Tconstr(p',tl',_) ->
      let (p1, s1) = best_type_path p and (p2, s2)  = best_type_path p' in
      begin match s1, s2 with
        Nth n1, Nth n2 when n1 = n2 -> true
      | (Id | Map _), (Id | Map _) when Path.same p1 p2 ->
          let tl = apply_subst s1 tl and tl' = apply_subst s2 tl' in
          List.length tl = List.length tl' &&
          List.for_all2 same_type tl tl'
      | _ -> false
      end
  | _ ->
      false

type 'a diff = Same of 'a | Diff of 'a * 'a

let trees_of_type_expansion (t,t') =
  if same_path t t'
  then begin add_delayed (proxy t); Same (tree_of_typexp false t) end
  else
    let t' = if proxy t == proxy t' then unalias t' else t' in
    (* beware order matter due to side effect,
       e.g. when printing object types *)
    let first = tree_of_typexp false t in
    let second = tree_of_typexp false t' in
    if first = second then Same first
    else Diff(first,second)

let type_expansion ppf = function
  | Same t -> !Oprint.out_type ppf t
  | Diff(t,t') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"  !Oprint.out_type t  !Oprint.out_type t'

module Trace = Ctype.Unification_trace

let trees_of_trace = List.map (Trace.map_diff trees_of_type_expansion)

let trees_of_type_path_expansion (tp,tp') =
  if Path.same tp tp' then Same(tree_of_path Type tp) else
    Diff(tree_of_path Type tp, tree_of_path Type tp')

let type_path_expansion ppf = function
  | Same p -> !Oprint.out_ident ppf p
  | Diff(p,p') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
        !Oprint.out_ident p
        !Oprint.out_ident p'

let rec trace fst txt ppf = function
  | {Trace.got; expected} :: rem ->
      if not fst then fprintf ppf "@,";
      fprintf ppf "@[Type@;<1 2>%a@ %s@;<1 2>%a@] %a"
       type_expansion got txt type_expansion expected
       (trace false txt) rem
  | _ -> ()


type printing_status =
  | Discard
  | Keep
  | Optional_refinement
  (** An [Optional_refinement] printing status is attributed to trace
      elements that are focusing on a new subpart of a structural type.
      Since the whole type should have been printed earlier in the trace,
      we only print those elements if they are the last printed element
      of a trace, and there is no explicit explanation for the
      type error.
  *)

let printing_status  = function
  | Trace.(Diff { got=t1, t1'; expected=t2, t2'}) ->
      if  is_constr_row ~allow_ident:true t1'
       || is_constr_row ~allow_ident:true t2'
      then Discard
      else if same_path t1 t1' && same_path t2 t2' then Optional_refinement
      else Keep
  | _ -> Keep

(** Flatten the trace and remove elements that are always discarded
    during printing *)
let prepare_trace f tr =
  let clean_trace x l = match printing_status x with
    | Keep -> x :: l
    | Optional_refinement when l = [] -> [x]
    | Optional_refinement | Discard -> l
  in
  match Trace.flatten f tr with
  | [] -> []
  | elt :: rem -> (* the first element is always kept *)
      elt :: List.fold_right clean_trace rem []

(** Keep elements that are not [Diff _ ] and take the decision
    for the last element, require a prepared trace *)
let rec filter_trace keep_last = function
  | [] -> []
  | [Trace.Diff d as elt] when printing_status elt = Optional_refinement ->
      if keep_last then [d] else []
  | Trace.Diff d :: rem -> d :: filter_trace keep_last rem
  | _ :: rem -> filter_trace keep_last rem

let type_path_list =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.pp_print_break ppf 2 0)
    type_path_expansion

(* Hide variant name and var, to force printing the expanded type *)
let hide_variant_name t =
  match repr t with
  | {desc = Tvariant row} as t when (row_repr row).row_name <> None ->
      newty2 t.level
        (Tvariant {(row_repr row) with row_name = None;
                   row_more = newvar2 (row_more row).level})
  | _ -> t

let prepare_expansion (t, t') =
  let t' = hide_variant_name t' in
  mark_loops t;
  if not (same_path t t') then mark_loops t';
  (t, t')

let may_prepare_expansion compact (t, t') =
  match (repr t').desc with
    Tvariant _ | Tobject _ when compact ->
      mark_loops t; (t, t)
  | _ -> prepare_expansion (t, t')

let print_tags ppf fields =
  match fields with [] -> ()
  | (t, _) :: fields ->
      fprintf ppf "`%s" t;
      List.iter (fun (t, _) -> fprintf ppf ",@ `%s" t) fields

let is_unit env ty =
  match (Ctype.expand_head env ty).desc with
  | Tconstr (p, _, _) -> Path.same p Predef.path_unit
  | _ -> false

let unifiable env ty1 ty2 =
  let snap = Btype.snapshot () in
  let res =
    try Ctype.unify env ty1 ty2; true
    with Unify _ -> false
  in
  Btype.backtrack snap;
  res

let explanation_diff env t3 t4 : (Format.formatter -> unit) option =
  match t3.desc, t4.desc with
  | Tarrow (_, ty1, ty2, _), _
    when is_unit env ty1 && unifiable env ty2 t4 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[Hint: Did you forget to provide `()' as argument?@]")
  | _, Tarrow (_, ty1, ty2, _)
    when is_unit env ty1 && unifiable env t3 ty2 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[Hint: Did you forget to wrap the expression using \
           `fun () ->'?@]")
  | _ ->
      None

let print_pos ppf = function
  | Trace.First -> fprintf ppf "first"
  | Trace.Second -> fprintf ppf "second"

let explain_variant = function
  | Trace.No_intersection ->
      Some(dprintf "@,These two variant types have no intersection")
  | Trace.No_tags(pos,fields) -> Some(
      dprintf
        "@,@[The %a variant type does not allow tag(s)@ @[<hov>%a@]@]"
        print_pos pos
        print_tags fields
    )
  | Trace.Incompatible_types_for s ->
      Some(dprintf "@,Types for tag `%s are incompatible" s)

let explain_escape intro prev ctx e =
  let pre = match ctx with
    | Some ctx ->  dprintf "@[%t@;<1 2>%a@]" intro type_expr ctx
    | None -> match e, prev with
      | Trace.Univ _, Some(Trace.Incompatible_fields {name; diff}) ->
          dprintf "@,@[The method %s has type@ %a,@ \
                   but the expected method type was@ %a@]" name
            type_expr diff.Trace.got type_expr diff.Trace.expected
      | _ -> ignore in
  match e with
  | Trace.Univ u ->  Some(
      dprintf "%t@,The universal variable %a would escape its scope"
        pre type_expr u)
  | Trace.Constructor p -> Some(
      dprintf
        "%t@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
        pre path p
    )
  | Trace.Module_type p -> Some(
      dprintf
        "%t@,@[The module type@;<1 2>%a@ would escape its scope@]"
        pre path p
    )
  | Trace.Equation (_,t) -> Some(
      dprintf "%t @,@[<hov>This instance of %a is ambiguous:@ %s@]"
        pre type_expr t
        "it would escape the scope of its equation"
    )
  | Trace.Self ->
      Some (dprintf "%t@,Self type cannot escape its class" pre)


let explain_object = function
  | Trace.Self_cannot_be_closed ->
      Some (dprintf "@,Self type cannot be unified with a closed object type")
  | Trace.Missing_field (pos,f) ->
      Some(dprintf "@,@[The %a object type has no method %s@]" print_pos pos f)
  | Trace.Abstract_row pos -> Some(
      dprintf
        "@,@[The %a object type has an abstract row, it cannot be closed@]"
        print_pos pos
    )


let explanation intro prev env = function
  | Trace.Diff { Trace.got = _, s; expected = _,t } -> explanation_diff env s t
  | Trace.Escape {kind;context} -> explain_escape intro prev context kind
  | Trace.Incompatible_fields { name; _ } ->
        Some(dprintf "@,Types for method %s are incompatible" name)
  | Trace.Variant v -> explain_variant v
  | Trace.Obj o -> explain_object o
  | Trace.Rec_occur(x,y) ->
      mark_loops y;
      Some(dprintf "@,@[<hov>The type variable %a occurs inside@ %a@]"
            type_expr x type_expr y)

let mismatch intro env trace =
  let rec mismatch intro env = function
    | [] -> None
    | [h] -> explanation intro None env h
    | h :: (prev :: _ as rem) -> match explanation intro (Some prev) env h with
      | Some _ as m -> m
      | None -> mismatch intro env rem in
  mismatch intro env (List.rev trace)

let explain mis ppf =
  match mis with
  | None -> ()
  | Some explain -> explain ppf

let warn_on_missing_def env ppf t =
  match t.desc with
  | Tconstr (p,_,_) ->
    begin
      try
        ignore(Env.find_type p env : Types.type_declaration)
      with Not_found ->
        fprintf ppf
          "@,@[%a is abstract because no corresponding cmi file was found \
           in path.@]" path p
    end
  | _ -> ()


let prepare_expansion_head empty_tr = function
  | Trace.Diff d ->
      Some(Trace.map_diff (may_prepare_expansion empty_tr) d)
  | _ -> None

let head_error_printer txt_got txt_but = function
  | None -> ignore
  | Some d ->
      let d = Trace.map_diff trees_of_type_expansion d in
      dprintf "%t@;<1 2>%a@ %t@;<1 2>%a"
        txt_got type_expansion d.Trace.got
        txt_but type_expansion d.Trace.expected

let warn_on_missing_defs env ppf = function
  | None -> ()
  | Some {Trace.got=te1,_; expected=te2,_ } ->
      warn_on_missing_def env ppf te1;
      warn_on_missing_def env ppf te2

let unification_error env tr txt1 ppf txt2 ty_expect_explanation =
  reset ();
  let tr = prepare_trace (fun t t' -> t, hide_variant_name t') tr in
  let mis = mismatch txt1 env tr in
  match tr with
  | [] -> assert false
  | elt :: tr ->
    try
      print_labels := not !Clflags.classic;
      let tr = filter_trace (mis = None) tr in
      let head = prepare_expansion_head (tr=[]) elt in
      let tr = List.map (Trace.map_diff prepare_expansion) tr in
      let head_error = head_error_printer txt1 txt2 head in
      let tr = trees_of_trace tr in
      fprintf ppf
        "@[<v>\
          @[%t%t@]%a%t\
         @]"
        head_error
        ty_expect_explanation
        (trace false "is not compatible with type") tr
        (explain mis);
      if env <> Env.empty
      then warn_on_missing_defs env ppf head;
      Conflicts.print ppf;
      print_labels := true
    with exn ->
      print_labels := true;
      raise exn

let report_unification_error ppf env tr
    ?(type_expected_explanation = fun _ -> ())
    txt1 txt2 =
  wrap_printing_env env (fun () -> unification_error env tr txt1 ppf txt2
                            type_expected_explanation)
    ~error:true
;;

(** [trace] requires the trace to be prepared *)
let trace fst keep_last txt ppf tr =
  print_labels := not !Clflags.classic;
  try match tr with
    | elt :: tr' ->
        let elt = match elt with
          | Trace.Diff diff -> [Trace.map_diff trees_of_type_expansion diff]
          | _ -> [] in
        let tr =
          trees_of_trace
          @@ List.map (Trace.map_diff prepare_expansion)
          @@ filter_trace keep_last tr' in
      if fst then trace fst txt ppf (elt @ tr)
      else trace fst txt ppf tr;
      print_labels := true
  | _ -> ()
  with exn ->
    print_labels := true;
    raise exn

let report_subtyping_error ppf env tr1 txt1 tr2 =
  wrap_printing_env ~error:true env (fun () ->
    reset ();
    let tr1 = prepare_trace (fun t t' -> prepare_expansion (t, t')) tr1 in
    let tr2 = prepare_trace (fun t t' -> prepare_expansion (t, t')) tr2 in
    let keep_first = match tr2 with
      | Trace.[Obj _ | Variant _ | Escape _ ] | [] -> true
      | _ -> false in
    fprintf ppf "@[<v>%a" (trace true keep_first txt1) tr1;
    if tr2 = [] then fprintf ppf "@]" else
    let mis = mismatch (dprintf "Within this type") env tr2 in
    fprintf ppf "%a%t%t@]"
      (trace false (mis = None) "is not compatible with type") tr2
      (explain mis)
      Conflicts.print
  )


let report_ambiguous_type_error ppf env tp0 tpl txt1 txt2 txt3 =
  wrap_printing_env ~error:true env (fun () ->
    reset ();
    let tp0 = trees_of_type_path_expansion tp0 in
      match tpl with
      [] -> assert false
    | [tp] ->
        fprintf ppf
          "@[%t@;<1 2>%a@ \
             %t@;<1 2>%a\
           @]"
          txt1 type_path_expansion (trees_of_type_path_expansion tp)
          txt3 type_path_expansion tp0
    | _ ->
        fprintf ppf
          "@[%t@;<1 2>@[<hv>%a@]\
             @ %t@;<1 2>%a\
           @]"
          txt2 type_path_list (List.map trees_of_type_path_expansion tpl)
          txt3 type_path_expansion tp0)

(* Adapt functions to exposed interface *)
let tree_of_path = tree_of_path Other
let tree_of_modtype = tree_of_modtype ~ellipsis:false
let type_expansion ty ppf ty' =
  type_expansion ppf (trees_of_type_expansion (ty,ty'))
let tree_of_type_declaration id td rs =
  wrap_env (hide [id]) (fun () -> tree_of_type_declaration id td rs) ()
