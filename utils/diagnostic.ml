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

module Label_map = Misc.Stdlib.String.Map
module H = Diagnostic_history

type polarity = Positive | Negative
type _ extension = ..

type version = Diagnostic_history.version = { major:int; minor: int}
type 'a update = 'a Diagnostic_history.update

type 'a typ =
  | Unit: unit typ
  | Bool: bool typ
  | Int: int typ
  | Float: float typ
  | String: string typ
  | List: 'a typ -> 'a list typ
  | Pair: 'a typ * 'b typ -> ('a * 'b) typ
  | Triple: 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) typ
  | Quadruple: 'a typ * 'b typ * 'c typ * 'd typ ->
      ('a * 'b * 'c * 'd) typ

  | Sum: 'a t -> 'a sum typ
  | Record: 'id t -> 'id record typ
  | Custom: {
      id :'b extension;
      pull: (version option -> 'b -> 'a);
      default: 'a typ
    } -> 'b typ
and ('a,'b) field = {
  label:string;
  typ:'a typ;
  opt:bool;
  id: 'a Type.Id.t;
  range:Diagnostic_history.Lifetime.t
}
and 'a bound_field = F: ('a,'b) field * 'a -> 'b bound_field
and 'id sum =
    Constr: { name:string; typ:'a typ; arg:'a; approx: 'id sum option }
      -> 'id sum
and any_typ = T: 'a typ -> any_typ
and label_metadata = {
  ltyp: any_typ;
  optional: bool;
  parent: string option;
  desc: string option;
  status:Diagnostic_history.Lifetime.t;
}
and 'a t = {
  name: string;
  description: string;
  mutable labels: (Label_map.key * label_metadata) list;
  polarity: polarity;
}
and 'a record = 'a bound_field Label_map.t ref

type 'a diagnostic = 'a t

type typed_record = R: 'a t * 'a record -> typed_record
and typed_val = V: 'a typ * 'a -> typed_val

let version_range field = field.range
let field_name f = f.label
let field_type field = field.typ
let is_optional r = r.optional

let destruct c f =
  let rec expand nexts (Constr c) =
    let nexts = (c.name, V (c.typ,c.arg)) :: nexts in
      match c.approx with
      | None -> Array.of_list nexts
      | Some t -> expand nexts t
  in
  f (expand [] c)
let scheme_name x = x.name
let scheme_description x = x.description

let field_infos ~version d =
  let all = d.labels in
  let filter v (name,lmd)=
    Option.map (fun status -> (name, {lmd with status}))
      (Diagnostic_history.Lifetime.at_version v lmd.status)
  in
  match version with
  | None -> all
  | Some v ->
      List.filter_map (filter v) all

let field_names d = List.map fst d.labels

let record_scheme: type a. a record typ -> a t  =
  function
  | Custom _ -> assert false
  | Record sch -> sch
  | _ -> .

let record_list_scheme: type a. a record list typ -> a t  =
  function
  | Custom _ -> assert false
  | List r -> record_scheme r
  | _ -> .


let (.!()<-) scheme name metadata =
  let rec update_if_present name metadata = function
    | [] -> None
    | (a_name, _ as a) :: q ->
        if a_name = name then Some ((name,metadata) :: q)
        else
          Option.map (List.cons a) (update_if_present name metadata q)
  in
  let updated = update_if_present name metadata scheme.labels in
  let labels = Option.value ~default:((name,metadata)::scheme.labels) updated in
  scheme.labels <- labels

let rec pp_typ: type a. Format.formatter -> a typ -> unit = fun ppf -> function
| Unit -> Format.pp_print_string ppf ""
| Int -> Format.pp_print_string ppf "i"
| Bool -> Format.pp_print_string ppf "b"
| Float -> Format.pp_print_string ppf "f"
| String -> Format.pp_print_string ppf "s"
| List elt -> Format.fprintf ppf "l %a" with_parens elt
| Pair (x,y) -> Format.fprintf ppf "%a*%a" with_parens x with_parens y
| Triple (x,y,z) ->
    Format.fprintf ppf "%a*%a*%a" with_parens x with_parens y with_parens z
| Quadruple (x,y,z,w) ->
  Format.fprintf ppf "*%a*%a*%a*%a"
    with_parens x with_parens y with_parens z with_parens w
| Sum def -> Format.fprintf ppf "%s" def.name
| Record def -> Format.fprintf ppf "%s" def.name
| Custom r -> pp_typ ppf r.default
and with_parens: type a. Format.formatter -> a typ -> unit = fun ppf elt ->
  let parens_needed =  match elt with
  | Pair _ -> true
  | Triple _ -> true
  | Quadruple _ -> true
  | _ -> false
  in
  if parens_needed then Format.fprintf ppf "(%a)" pp_typ elt else pp_typ ppf elt

let label_metadata ~desc ~optional ?parent update typ = {
    status = H.(Lifetime.make @@ v update);
    optional;
    parent;
    desc;
    ltyp = T typ
  }

let register_label_metadata ~desc ~optional update scheme name typ =
  begin match scheme.polarity with
  | Positive -> ()
  | Negative -> Diagnostic_history.breaking_change update scheme.name
  end;
  if List.mem_assoc name scheme.labels then
    Diagnostic_history.(error update scheme.name (Duplicate_key name));
  let metadata = label_metadata ~desc ~optional update typ in
  scheme.!(name) <- metadata;
  Diagnostic_history.register_event update scheme.name
    (Creation {
        name;
        typ=Format.asprintf "%s%a" (if optional then "?" else "") pp_typ typ
      };
    )

module type Def = sig
  type id
  type vl
  type 'a label
  type definition
  type t = id diagnostic
  type raw_type = definition

  val scheme: t
  val raw_type: raw_type typ

  val deprecate: vl update -> 'a label -> 'a label
  val delete: vl update -> 'a label -> 'a label
  val seal: vl update -> unit
end

module type Record = sig
  type id
  type nonrec 'a field = ('a,id) field
  include Def
    with type id := id
     and type definition = id record
     and type 'a label :='a field
  val new_field:
    ?opt:bool ->  ?desc:string -> vl update  -> string -> 'a typ -> 'a field
  val new_field_opt: ?desc:string -> vl update  -> string -> 'a typ -> 'a field
  val make_required: vl update -> 'a field -> unit
  type record_fragment
  val make:
    Diagnostic_history.version option -> record_fragment list -> definition
  val (^=): 'a field -> 'a -> record_fragment
  val (^=?): 'a field -> 'a option -> record_fragment
end

type ('elt,'id) constructor =
  { cname: string;
    typ: 'elt typ;
    projection: ('elt,'id) constructor_projection option;
  }
and ('current,'id) constructor_projection =
  | Proj: {
      map: 'current -> 'old;
      old: ('old,'id) constructor;
      version: version;
    } -> ('current,'id) constructor_projection

let is_expansion c (Proj p) = c.cname = p.old.cname

let rec select_version:
  type t id. version -> (t,id) constructor -> t -> id sum =
  fun v c x ->
  match c.projection with
  | None -> Constr { name = c.cname; typ=c.typ; arg=x; approx = None}
  | Some (Proj p) ->
      if v >= p.version then
        Constr { name = c.cname; typ=c.typ; arg=x; approx=None}
      else select_version v p.old (p.map x)

let rec expand_all_approx: type t id. (t,id) constructor -> t -> id sum =
  fun c x ->
  match c.projection with
  | None -> Constr { name = c.cname; typ=c.typ; arg=x; approx = None}
  | Some (Proj p) ->
    let approx = Some (expand_all_approx p.old (p.map x)) in
    Constr { name = c.cname; typ=c.typ; arg=x; approx}

let app v c x =
  match v with
  | None -> expand_all_approx c x
  | Some v -> select_version v c x

module type Sum = sig
  type id
  type 'a constructor
  include Def
    with type id := id
     and type definition := id sum
     and type 'a label := 'a constructor
  val app: version option -> 'a constructor -> 'a -> raw_type
  val new_constr:
    ?desc:string -> vl update -> string -> 'a typ -> 'a constructor
  val new_constr0: ?desc:string -> vl update -> string -> unit constructor

  val refine:
    ?desc:string -> vl update -> 'a constructor -> ('b -> 'a)
    -> string -> 'b typ -> 'b constructor
  val expand:
    vl update -> 'a constructor -> ('b->'a) -> 'b typ -> 'b constructor
  val publish: vl update -> 'a constructor -> 'a constructor
end



module New_local_def() = struct
  type id
  type t = id diagnostic
end

let (.?()) scheme lbl = List.assoc_opt lbl scheme.labels
let field_info sch f = sch.?(f.label)
let field_dyninfo sch name = sch.?(name)

let (let&?) x f = Option.iter f x

let make_required u f scheme =
  let&? kmd = scheme.?(f.label) in
  H.inconsistent_if_inactive u ~scheme:scheme.name f.label kmd.status;
  H.register_event u scheme.name (Make_required f.label);
  scheme.!(f.label) <- { kmd with optional = false }

let register_constructor_expansion u old new_typ scheme =
  let&? kmd = scheme.?(old.cname) in
  H.inconsistent_if_inactive u ~scheme:scheme.name old.cname kmd.status;
  begin match old.projection with
  | None -> ()
  | Some p ->
      if is_expansion old p then
        H.invalid_constructor_expansion u ~scheme:scheme.name old.cname
  end;
  H.register_event u scheme.name
    (Expansion {name=old.cname;
                expansion = Format.asprintf "%a" pp_typ new_typ});
  let status = { kmd.status with expansion = Some (H.v u) } in
  scheme.!(old.cname) <- { kmd with status; ltyp=T new_typ }


let register_constructor_inception ~desc u old new_name new_typ scheme =
  let&? kmd = scheme.?(old.cname) in
  H.inconsistent_if_inactive u ~scheme:scheme.name old.cname kmd.status;
  H.register_event u scheme.name
    (Inception {
        base_name=old.cname;
        new_name;
        typ = Format.asprintf "%a" pp_typ new_typ
      }
    );
  let status = H.(Lifetime.make ~published:false @@ v u) in
  let lmd = label_metadata ~desc ~parent:old.cname ~optional:false u new_typ in
  scheme.!(new_name) <- { lmd with status }

let register_constructor_publication u name scheme =
  let&? kmd = scheme.?(name) in
  begin match H.Lifetime.stage kmd.status with
  | Inception -> ()
  | _ -> H.error u scheme.name (Invalid_publication name)
  end;
  H.register_event u scheme.name (Publication name);
  let status = { kmd.status with publication = Some (H.v u) } in
  scheme.!(name) <- { kmd with status }



let deprecate_lbl u lbl scheme =
  let&? kmd = scheme.?(lbl) in
  H.inconsistent_if_inactive u ~scheme:scheme.name lbl kmd.status;
  H.register_event u scheme.name (Deprecation lbl);
  let status = { kmd.status with deprecation = Some (H.v u) } in
  scheme.!(lbl) <- { kmd with status }

let delete_lbl u lbl scheme =
  let&? kmd = scheme.?(lbl) in
  H.inconsistent_if_not_deprecated u ~scheme:scheme.name lbl kmd.status;
  H.register_event u scheme.name (Deletion lbl);
  let status = { kmd.status with deletion = Some (H.v u) } in
  scheme.!(lbl) <- { kmd with status }

let seal update scheme =
  H.register_event update scheme.name Seal

module type Info = sig
  type vl
  val name: string
  val description: string
  val update: vl update
end

module Record_construction = struct
  type 'a bfield = version option -> 'a bound_field option
  let field f x v =
    match H.Lifetime.stage_at v f.range with
    | Inception | Publication | Expansion | Deprecation -> Some (F(f,x))
    | Future | Deletion -> None
  let opt_field f x v = match x with
    | None -> None
    | Some x -> field f x v
  let (^=) = field
  let (^=?) = opt_field

  let field_name (F (f,_)) = f.label

  let make v fields =
    let fields = List.fold_left (fun fields field ->
        match field v with
        | None -> fields
        | Some field ->  Label_map.add (field_name field) field fields
      ) Label_map.empty fields
    in
    ref fields
end

module Record_introspection = struct
  open Record_construction
  let empty () = ref Label_map.empty
  let fields x = !x
  let all_fields x = Seq.map snd @@ Label_map.to_seq (fields x)

  let set:
    type ty s.
      s record -> version option -> field:(ty,s) field -> ty -> unit
    = fun store v ~field:f x ->
        let name = f.label in
        Option.iter (fun field ->
        store := Label_map.add name field !store
        ) (field f x v)

  let get (type a b) (st:b record) (field: (a,b) field): a option =
    match Label_map.find_opt field.label (fields st) with
    | None -> None
    | Some (F(f,x)) ->
        match Type.Id.provably_equal f.id field.id with
        | None -> None
        | Some Type.Equal -> Some x

  let dynamic_get st name =
    Label_map.find_opt name (fields st)
    |> Option.map (fun (F(k,x)) -> V (k.typ,x))

  let cons: type ty s.
    s record -> version option -> field:(ty list,s) field -> ty -> unit =
    fun store v ~field:f x ->
      let l = match get store f with
        | None -> [x]
        | Some l -> x :: l
      in
      let bf = field f l v in
      Option.iter (fun bfield ->
          store := Label_map.add f.label bfield (fields store)
        ) bf

   let reset f = f := Label_map.empty
end

module New_record(Vl:H.S)(Info:Info with type vl:=Vl.id)() = struct
  include New_local_def ()
  type definition = id record
  type nonrec 'a field = ('a,id) field
  type raw_type = id record
  let scheme = {
    name = Info.name;
    description = Info.description;
    labels = [];
    polarity=Positive;
  }
  let raw_type = Record scheme

  let () = H.register_event Info.update Info.name Declaration

  let new_field ?(opt=false) ?desc  (type t) u label (ty:t typ): t field =
    register_label_metadata ~desc ~optional:opt u scheme label ty;
    {
      label;
      typ = ty;
      opt;
      id = Type.Id.make ();
      range = H.(Lifetime.make @@ v u)
    }
  let new_field_opt ?desc v name ty = new_field ~opt:true ?desc v name ty
  let deprecate u f =
    deprecate_lbl u f.label scheme;
    let range = { f.range with deprecation = Some (H.v u) } in
    { f with range }
  let delete u f =
    delete_lbl u f.label scheme;
    let range = { f.range with deletion = Some (H.v u) } in
    { f with range }

  let make_required u f = make_required u f scheme
  let seal u = seal u scheme
  type record_fragment = id Record_construction.bfield
  let make = Record_construction.make
  let (^=) = Record_construction.(^=)
  let (^=?) = Record_construction.(^=?)
end

module New_sum(Vl:H.S)(Info:Info with type vl:=Vl.id)() = struct
  include New_local_def ()
  type raw_type = id sum
  let scheme = {
    name = Info.name;
    description = Info.description;
    labels = [];
    polarity = Negative;
  }
  let raw_type = Sum scheme
  type nonrec 'a constructor = ('a,id) constructor
  let () = H.register_event Info.update Info.name Declaration
  let new_constr ?desc u name (ty:'a typ): 'a constructor  =
    register_label_metadata ~desc ~optional:false u scheme name ty;
    { cname = name;
      typ = ty;
      projection = None;
    }
  let new_constr0 ?desc u name = new_constr ?desc u name Unit
  let app = app

  let expand u old map new_ty =
    let () = register_constructor_expansion u old new_ty scheme in
    let projection = Some(Proj {map;old;version=H.v u}) in
    { old with typ=new_ty; projection }

  let refine ?desc u old map new_name new_ty =
    let () =
      register_constructor_inception ~desc u old new_name new_ty scheme
    in
    let projection = Some(Proj {map;old;version=H.v u}) in
    { cname=new_name; typ=new_ty; projection }

  let publish u c =
    register_constructor_publication u c.cname scheme;
    c

  let deprecate u c = deprecate_lbl u c.cname scheme; c
  let delete u c = delete_lbl u c.cname scheme; c
  let seal u = seal u scheme
end

let fields labels r =
  let field rfields label =
    rfields
    |> Label_map.find_opt label
    |> Option.map (fun (F (k,v)) -> k.label, k.opt, V(k.typ,v))
  in
  List.filter_map (field @@ Record_introspection.fields r) (List.rev labels)

module Metadata_versions = struct
  include H.Make()
  let v1 = new_version { major = 1; minor = 0}
end
module Metadata = New_record(Metadata_versions)(struct
    let name = "metadata"
    let description = "Diagnostic metadata"
    let update = Metadata_versions.v1
  end)()
let universal_metafield () =
  {
    range = H.(Lifetime.make @@ v Metadata_versions.v1);
    label = "metadata";
    opt=false;

    typ = Metadata.raw_type;
    id = Type.Id.make ()
  }
let metakey =
  let desc =
    Some
      "This field describes the scheme version used to generate the diagnostic \
       instance, and if this instance is valid according to this scheme."
  in
  "metadata",
  label_metadata ~desc ~optional:false Metadata_versions.v1 Metadata.raw_type
