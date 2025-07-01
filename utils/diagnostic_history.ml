(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type version = { major:int; minor:int }
let version ~major ~minor = { major; minor }

module Lifetime = struct
  type t = {
    preview: version option;
    publication: version option;
    expansion: version option;
    deprecation: version option;
    deletion: version option;
  }
  type point =
    | Preview
    | Publication
    | Expansion
    | Deprecation
    | Deletion
    | Future

  let next = function
    | Preview -> Publication
    | Publication -> Expansion
    | Expansion -> Deprecation
    | Deprecation -> Deletion
    | Deletion -> Deletion
    | Future -> Future

  let prev = function
    | Preview -> Preview
    | Publication -> Preview
    | Expansion -> Publication
    | Deprecation -> Expansion
    | Deletion -> Deprecation
    | Future -> Future

  let get r = function
    | Preview -> r.preview
    | Publication -> r.publication
    | Expansion -> r.expansion
    | Deprecation -> r.deprecation
    | Deletion -> r.deletion
    | Future -> None

  let rec after r p =
    match get r p with
    | None ->
        if p = Deletion then None
        else after r (next p)
    | Some x -> Some (p, x)

  let rec last_change r p =
    if p = Preview then Preview
    else match get r p with
      | None -> last_change r (prev p)
      | Some _ -> p

  let rec stage_after v current r =
    if current = Deletion then current else
      match after r (next current) with
      | None -> current
      | Some (p,v1) ->
          if v < v1 then current
          else if v = v1 then p
          else stage_after v (next p) r

  let stage_at v r =
    match v, after r Preview with
    | Some _, None -> assert false
    | None, _ -> Publication
    | Some v, Some (p,v1) ->
        if v < v1 then Future
        else if v = v1 then p
        else stage_after v p r
  let stage r = last_change r Deletion


  let make ?deprecation ?deletion ?expansion ?(published=true) preview =
    let preview = Some preview in
    let preview, publication =
      if published then None, preview else preview, None
    in
    { preview; publication; expansion; deprecation; deletion }

  let at_version v lf =
    match lf.preview, lf.publication with
    | None, None -> None
    | Some i, _ | _, Some i ->
      if i > v then None else
      let filter v lfe =
        Option.bind lfe (fun lfe -> if lfe > v then None else Some lfe)
      in
      Some {
        preview = filter v lf.preview;
        publication = filter v lf.publication;
        expansion = filter v lf.expansion;
        deprecation = filter v lf.deprecation;
        deletion = filter v lf.deletion;
      }
end





type error =
  | Duplicate_key of string
  | Time_travel of version * version
  | Inconsistent_change of Lifetime.t * string
  | Invalid_constructor_expansion of string
  | Invalid_publication of string
  | Sealed_version of version

type base_event =
  | Declaration
  | Preview of {base_name:string; new_name:string; typ:string}
  | Publication of string
  | Creation of {name:string; typ:string}
  | Make_required of string
  | Expansion of {name:string; expansion:string}
  | Deprecation of string
  | Deletion of string
  | Seal
  | Error of error

type event =
  { scheme: string; version:version; event:base_event }
type _ t = {
  mutable current: version;
  events: event Dynarray.t
}

type 'a update = {
  v:version;
  history:'a t;
  minor_update:bool;
}
let v x = x.v

let register_event update scheme event =
  let h = update.history in
  Dynarray.add_last h.events { scheme; version=update.v; event}

let error update sch err = register_event update sch (Error err)

let breaking_change update sch = if update.minor_update then
    error update sch (Sealed_version update.history.current)

 let inconsistent_if_not_deprecated u ~scheme key (range:Lifetime.t) =
   match range.deprecation, range.deletion with
   | Some _ , None -> ()
   | None, _ | _, Some _ ->
      error u scheme (Inconsistent_change (range,key))

 let inconsistent_if_inactive u ~scheme key (range:Lifetime.t) =
   match range.deprecation with
   | None -> ()
   | Some _ ->  error u scheme (Inconsistent_change (range,key))

let invalid_publication u ~scheme name =
  error u scheme (Invalid_publication name)

let invalid_constructor_expansion u ~scheme cstr_name =
  error u scheme (Invalid_constructor_expansion (cstr_name))

let zeroth = { major = (-1); minor = 0}

let new_version history version =
  let sv = history.current in
  if version <= sv  then begin
    let error=Error (Time_travel (version, sv)) in
    let event = { scheme=""; version=sv; event=error} in
    Dynarray.add_last history.events event
  end;
  let minor_update = version.major = sv.major in
  history.current <- version;
  { v=version; minor_update; history }

let current_version history = history.current
let events history = Dynarray.to_seq history.events

let pp ppf x = Format.fprintf ppf "v%d.%d" x.major x.minor

module type S = sig
  type id
  val history: id t
  val new_version: version -> id update
end

module Make() = struct
  type id
  let history = {
    current = zeroth;
    events = Dynarray.create ();
  }
  let new_version v = new_version history v
end
