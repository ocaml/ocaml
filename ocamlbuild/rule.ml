(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
(* Original author: Nicolas Pouillard *)
open My_std
open Format
open Log
open Outcome
module Resources = Resource.Resources

exception Exit_rule_error of string

type env = Pathname.t -> Pathname.t
type builder = Pathname.t list list -> (Pathname.t, exn) Outcome.t list
type action = env -> builder -> Command.t

type rule_action =
  | RunCmd of Command.t
  | DigestThunk of string * (bool -> unit)

type 'a gen_rule =
  { name  : string;
    tags  : Tags.t;
    deps  : Pathname.t list;
    prods : 'a list;
    code  : env -> builder -> rule_action }

type rule = Pathname.t gen_rule
type rule_scheme = Resource.resource_pattern gen_rule

let name_of_rule r = r.name
let deps_of_rule r = r.deps
let prods_of_rule r = r.prods

type 'a rule_printer = (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a gen_rule -> unit

let compare _ _ = assert false

let print_rule_name f r = pp_print_string f r.name

let print_resource_list = List.print Resource.print

let print_rule_contents ppelt f r =
  fprintf f "@[<v2>{@ @[<2>name  =@ %S@];@ @[<2>tags  =@ %a@];@ @[<2>deps  =@ %a@];@ @[<2>prods = %a@];@ @[<2>code  = <fun>@]@]@ }"
    r.name Tags.print r.tags print_resource_list r.deps (List.print ppelt) r.prods

let pretty_print ppelt f r =
  fprintf f "@[<hv2>rule@ %S@ ~deps:%a@ ~prods:%a@ <fun>@]"
    r.name print_resource_list r.deps (List.print ppelt) r.prods

let print = print_rule_name

let subst env rule =
  let subst_resources = List.map (Resource.subst env) in
  let subst_resource_patterns = List.map (Resource.subst_pattern env) in
  let finder next_finder p = next_finder (Resource.subst_any env p) in
  { (rule) with name = sbprintf "%s (%a)" rule.name Resource.print_env env;
                prods = subst_resource_patterns rule.prods;
                deps = subst_resources rule.deps;
                code = (fun env -> rule.code (finder env)) }

exception Can_produce of rule

let can_produce target rule =
  try
    List.iter begin fun resource ->
      match Resource.matchit resource target with
      | Some env -> raise (Can_produce (subst env rule))
      | None -> ()
    end rule.prods; None
  with Can_produce r -> Some r

(* let tags_matches tags r = if Tags.does_match tags r.tags then Some r else None *)

let digest_prods r =
  List.fold_right begin fun p acc ->
    let f = Pathname.to_string (Pathname.in_build_dir p) in
    if sys_file_exists f then (f, Digest.file f) :: acc else acc
  end r.prods []

let digest_rule r dyndeps action =
  let buf = Buffer.create 1024 in
  begin match action with
   | RunCmd cmd -> Buffer.add_string buf (Command.to_string_for_digest cmd)
   | DigestThunk(s, _) -> Buffer.add_string buf s
  end;
  let add_resource r = Buffer.add_string buf (Resource.Cache.digest_resource r) in
  Buffer.add_string buf "prods:";
  List.iter add_resource r.prods;
  Buffer.add_string buf "deps:";
  List.iter add_resource r.deps;
  Buffer.add_string buf "dyndeps:";
  Resources.iter add_resource dyndeps;
  Digest.string (Buffer.contents buf)

let print_digest f x = pp_print_string f (Digest.to_hex x)

let exists2 find p rs =
  try Some (find p rs) with Not_found -> None

let all_deps_of_tags = ref []

let cons deps acc =
  List.rev&
    List.fold_left begin fun acc dep ->
      if List.mem dep acc then acc else dep :: acc
    end acc deps

let deps_of_tags tags =
  List.fold_left begin fun acc (xtags, xdeps) ->
    if Tags.does_match tags xtags then cons xdeps acc
    else acc
  end [] !all_deps_of_tags

let set_deps_of_tags tags deps =
  all_deps_of_tags := (tags, deps) :: !all_deps_of_tags

let dep tags deps = set_deps_of_tags (Tags.of_list tags) deps

let build_deps_of_tags builder tags =
  match deps_of_tags tags with
  | [] -> []
  | deps -> List.map Outcome.good (builder (List.map (fun x -> [x]) deps))

let build_deps_of_tags_on_cmd builder x =
  let rec spec x =
    match x with
    | Command.N | Command.A _ | Command.Sh _ | Command.P _ | Command.Px _ | Command.V _ | Command.Quote _ -> ()
    | Command.S l -> List.iter spec l
    | Command.T tags ->
        begin match deps_of_tags tags with
        | [] -> ()
        | deps -> List.iter ignore_good (builder (List.map (fun x -> [x]) deps))
        end in
  let rec cmd x =
    match x with
    | Command.Nop -> ()
    | Command.Cmd(s) -> spec s
    | Command.Seq(s) -> List.iter cmd s in
  cmd x

let call builder r =
  let dyndeps = ref Resources.empty in
  let builder rs =
    let results = builder rs in
    List.map begin fun res ->
      match res with
      | Good res' ->
          let () = dprintf 10 "new dyndep for %S(%a): %S" r.name print_resource_list r.prods res' in
          dyndeps := Resources.add res' !dyndeps;
          List.iter (fun x -> Resource.Cache.add_dependency x res') r.prods;
          res
      | Bad _ -> res
    end results in
  let () = dprintf 5 "start rule %a" print r in
  let action = r.code (fun x -> x) builder in
  begin match action with
  | RunCmd cmd -> build_deps_of_tags_on_cmd builder cmd
  | DigestThunk _ -> ()
  end;
  let dyndeps = !dyndeps in
  let () = dprintf 10 "dyndeps: %a" Resources.print dyndeps in
  let (reason, cached) =
    match exists2 List.find (fun r -> not (Pathname.exists_in_build_dir r)) r.prods with
    | Some r -> (`cache_miss_missing_prod r, false)
    | _ ->
      begin match exists2 List.find Resource.Cache.resource_has_changed r.deps with
      | Some r -> (`cache_miss_changed_dep r, false)
      | _ ->
        begin match exists2 Resources.find Resource.Cache.resource_has_changed dyndeps with
        | Some r -> (`cache_miss_changed_dyn_dep r, false)
        | _ ->
            begin match Resource.Cache.get_digest_for r.name with
            | None -> (`cache_miss_no_digest, false)
            | Some d ->
                begin match action with
                | DigestThunk("", _) ->
                    (`cache_miss_undigest, false)
                | DigestThunk _ | RunCmd _ ->
                    let rule_digest = digest_rule r dyndeps action in
                    if d = rule_digest then (`cache_hit, true)
                    else (`cache_miss_digest_changed(d, rule_digest), false)
                end
            end
        end
      end
  in
  let explain_reason l =
    raw_dprintf (l+1) "mid rule %a: " print r;
    match reason with
    | `cache_miss_missing_prod r ->
          dprintf l "cache miss: a product is not in build dir (%a)" Resource.print r
    | `cache_miss_changed_dep r ->
          dprintf l "cache miss: a dependency has changed (%a)" Resource.print r
    | `cache_miss_changed_dyn_dep r ->
          dprintf l "cache miss: a dynamic dependency has changed (%a)" Resource.print r
    | `cache_miss_no_digest ->
          dprintf l "cache miss: no digest found for %S (the command, a dependency, or a product)"
            r.name
    | `cache_hit -> dprintf (l+1) "cache hit"
    | `cache_miss_digest_changed(old_d, new_d) ->
          dprintf l "cache miss: the digest has changed for %S (the command, a dependency, or a product: %a <> %a)"
            r.name print_digest old_d print_digest new_d
    | `cache_miss_undigest ->
          dprintf l "cache miss: cache not supported for the rule %S" r.name in
  let prod_digests = digest_prods r in
  (if not cached then List.iter Resource.clean r.prods);
  (if !Options.nothing_should_be_rebuilt && not cached then
    (explain_reason (-1);
     let msg = sbprintf "Need to rebuild %a through the rule `%a'" print_resource_list r.prods print r in
     raise (Exit_rule_error msg)));
  explain_reason 3;
  let thunk () =
    try
      begin match action with
      | RunCmd cmd -> if cached then Command.execute ~pretend:true cmd
      | DigestThunk(_, thunk) -> thunk cached
      end;
      List.iter (fun r -> Resource.Cache.resource_built r) r.prods;
      (if not cached then
        let new_rule_digest = digest_rule r dyndeps action in
        let new_prod_digests = digest_prods r in
        let () = Resource.Cache.store_digest r.name new_rule_digest in
        List.iter begin fun p ->
          let f = Pathname.to_string (Pathname.in_build_dir p) in
          (try let digest = List.assoc f prod_digests in
               let new_digest = List.assoc f new_prod_digests in
               if digest <> new_digest then raise Not_found
          with Not_found -> Resource.Cache.resource_changed p)
        end r.prods);
      dprintf 5 "end rule %a" print r
    with exn -> (List.iter Resource.clean r.prods; raise exn)
  in
  match action with
  | RunCmd cmd when not cached ->
      List.iter (fun x -> Resource.Cache.suspend_resource x cmd thunk r.prods) r.prods
  | RunCmd _ | DigestThunk _ -> thunk ()

let (get_rules, add_rule) =
  let rules = ref [] in
  (fun () -> !rules),
  begin fun pos r ->
    try
      let _ = List.find (fun x -> x.name = r.name) !rules in
      raise (Exit_rule_error (sbprintf "Rule.add_rule: already exists: (%a)" print r))
    with Not_found ->
      match pos with
      | `bottom -> rules := !rules @ [r]
      | `top -> rules := r :: !rules
      | `after s ->
          rules :=
            List.fold_right begin fun x acc ->
              if x.name = s then x :: r :: acc else x :: acc
            end !rules []
      | `before s ->
          rules :=
            List.fold_right begin fun x acc ->
              if x.name = s then r :: x :: acc else x :: acc
            end !rules []
  end

let gen_rule name ?(tags=[]) ?(prods=[]) ?(deps=[]) ?prod ?dep ?(insert = `bottom) code =
  let res_add import xs xopt =
    let init =
      match xopt with
      | None -> []
      | Some r -> [import r]
    in
    List.fold_right begin fun x acc ->
      let r = import x in
      if List.mem r acc then
        failwith (sprintf "in rule %s, multiple occurences of the resource %s" name x)
      else r :: acc
    end xs init
  in
  if prods = [] && prod = None then raise (Exit_rule_error "Can't make a rule that produce nothing");
  add_rule insert
  { name  = name;
    tags  = List.fold_right Tags.add tags Tags.empty;
    deps  = res_add Resource.import deps dep;
    prods = res_add Resource.import_pattern prods prod;
    code  = code }

let rule name ?tags ?prods ?deps ?prod ?dep ?insert code =
  gen_rule name ?tags ?prods ?deps ?prod ?dep ?insert
           (fun env build -> RunCmd (code env build))

let file_rule name ?tags ~prod ?deps ?dep ?insert ~cache action =
  gen_rule name ?tags ~prod ?dep ?deps ?insert begin fun env build ->
    let thunk cached =
      if not cached then
        with_output_file (env prod) (action env)
    in
    DigestThunk(cache env build, thunk)
  end

module Common_commands = struct
  open Command
  let mv src dest = Cmd (S [A"mv"; P src; Px dest])
  let cp src dest = Cmd (S [A"cp"; P src; Px dest])
  let cp_p src dest = Cmd (S [A"cp"; A"-p"; P src; Px dest])
  let ln_f pointed pointer = Cmd (S [A"ln"; A"-f"; P pointed; Px pointer])
  let ln_s pointed pointer = Cmd (S[A"ln"; A"-s"; P pointed; Px pointer])
  let rm_f x = Cmd (S [A"rm"; A"-f"; Px x])
  let chmod opts file = Cmd (S[A"chmod"; opts; Px file])
  let cmp a b = Cmd (S[A"cmp"; P a; Px b])
end
open Common_commands

let copy_rule name ?insert src dest =
  rule name ?insert ~prod:dest ~dep:src
       (fun env _ -> cp_p (env src) (env dest))

