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

(** [ocamldiaginfo] provides a way to print metadata information about all
    diagnostics printed by the compiler and REPL

  - [ocamldiaginfo -history] prints the full history of diagnostics across all
  versions
  - [ocamldiaginfo -schema <name>] prints the schema of a diagnostic, by default
  in an annotated ADT format or as a json schema with the [-schema-format json]
  flag
  - [ocamldiaginfo -list] prints all known schema
*)

open Diagnostic_history
module V = Compiler_diagnostic.V

let pp_version ppf v = Format.fprintf ppf "%d.%d" v.major v.minor

let failf fmt =
  Format.eprintf "@[<v>Error:";
  Format.kfprintf
    (fun ppf -> Format.fprintf ppf "@]@."; exit 2)
    Format.err_formatter fmt

module Options = struct
  type schema_format = Json | Adt
  type schema_name = All | One of string
  let name = ref None
  let with_deps = ref true
  let parse_name s = name := Some (One s)
  let list () = name := Some All

  let format = ref Adt
  let format_list = ["json"; "adt"]
  let parse_format = function
    | "json" -> format := Json
    | "adt" -> format := Adt
    | _ -> failf "Unknown schema format"

  let filename_template = ref None
  let parse_file_template s =
    match Scanf.format_from_string s "%s" with
    | x -> filename_template := Some (fun s -> Format.asprintf x s)
    | exception Scanf.Scan_failure _ -> filename_template := Some (fun _ -> s)

  let output_template = ref None
  let parse_template s =
    match Scanf.format_from_string s "%a" with
    | x ->
        output_template :=
          Some (fun ppf printer arg -> Format.fprintf ppf x printer arg)
    | exception Scanf.Scan_failure _ ->
        failf "Invalid format template: \"%s\"" s

  let version = ref None
  let parse_version () =
    match !version with
    | None -> Diagnostic_history.current_version V.history
    | Some v ->
        match Scanf.sscanf_opt v "%d.%d"
                (fun major minor -> {Diagnostic_history.major;minor})
        with
        | Some v -> v
        | None -> failf "Invalid version format: %s" v

  let history = ref false
let args =
  Arg.align
  [ "-schema-format", Arg.Symbol (format_list, parse_format),
    "print the schema in <name> format";
    "-schema", Arg.String parse_name, " print the schema <name>";
    "-list", Arg.Unit list, " print all known schema";
    "-history", Arg.Set history, " print log format history";
    "-version", Arg.String (fun x -> version := Some x),
    "<version> schema version";
    "-o", Arg.String parse_file_template,
    "<template> template name for output files";
    "-template", Arg.String parse_template, "<template> output %a-template";
    "-with-deps", Arg.Bool ( (:=) with_deps),
    "<bool> include dependencies in the printed schema"
  ]

end

module String_map = Misc.Stdlib.String.Map
module String_set = Misc.Stdlib.String.Set



(** Collect sum and record definitions from a scheme *)
module Defs = struct


  let metadata_version =
    Diagnostic_history.current_version Diagnostic.Metadata_versions.history

  let adjust_version name v =
    if name = "metadata" then Some metadata_version else v

  open Diagnostic
  let union map a = List.fold_left (fun m add -> add m) map a
  let rec add_refs: type a. version option -> a typ -> _ -> _  =
    fun v ty map -> match ty with
      | Sum x ->
          let name = scheme_name x in
          if String_map.mem name map then map
          else
            let map = String_map.add name (v, T ty) map in
            subrefs v (field_infos ~version:None x) map
      | Record x ->
          let name = scheme_name x in
          if String_map.mem name map then map
          else
            let v = adjust_version name v in
            let map = String_map.add name (v, T ty) map in
            subrefs v (field_infos ~version:None x) map
      | Int -> map
      | Bool -> map
      | String -> map
      | Unit -> map
      | Float -> map
      | List elt -> add_refs v elt map
      | Pair (x,y) -> union map [add_refs v x; add_refs v y]
      | Triple (x,y,z) -> union map [add_refs v x; add_refs v y; add_refs v z]
      | Quadruple (x,y,z,w) ->
          union map [add_refs v x; add_refs v y; add_refs v z; add_refs v w]
      | Custom t -> add_refs v t.default map
  and subrefs v keys map =
      union map @@
      List.map (fun (_, { ltyp = T t; _}) -> add_refs v t) keys

  let refs v typ = add_refs v typ String_map.empty

end

module JSchema = struct
  open Diagnostic
  module Pp = Diagnostic_backends.Pp
  open Pp
  let string s ppf = Format.fprintf ppf "%S" s
  let bool = Pp.bool json
  let item = Pp.item json


  let header v name  =
    let uri =
      let raw = "https://github.com/ocaml/ocaml/blob/trunk" in
      Format.dprintf {|"%s/diagnostic_schemes/%s.json"|} raw name
    in
      [
        (item ~key:"$schema" @@
         string "https://json-schema.org/draft/2020-12/schema");
        (item ~key:"$id" uri);
        item ~key:"version" (Format.dprintf {|"%a"|} pp_version v)
      ]

  let tfield  x = item ~key:"type" (string x)
  let obj prs = record json prs
  let array prs = list json prs

  let sref x =
    item ~key:"$ref" @@ Format.dprintf {|"#/$defs/%s"|} (scheme_name x)

  let tuple l =
    Format.dprintf "%t,@ %t"
      (tfield  {|array|})
      (item ~key:"prefixItems" @@ array l)

  let tuple_typ l = tuple (List.map (fun x -> obj [x]) l)

  let rec typ: type a. a typ -> Format.formatter -> unit = function
    | Int -> tfield {|integer|}
    | Bool -> tfield {|boolean|}
    | Unit -> tfield {|int|}
    | String -> tfield {|string|}
    | Float -> tfield "number"
    | List e ->
        Format.dprintf "%t,@ %t"
          (tfield  {|array|})
          (item ~key:"items" @@ obj [typ e] )
    | Pair (x,y) -> tuple_typ [typ x; typ y]
    | Triple (x,y,z) -> tuple_typ [typ x; typ y; typ z]
    | Quadruple (x,y,z,w) -> tuple_typ [typ x;typ y; typ z; typ w]
    | Sum x -> sref x
    | Record x -> sref x
    | Custom x -> typ x.default

  let any_typ = tfield {|object|}

  let desc_field d = item ~key:"description" @@ string d
  let with_desc desc l = match desc with
    | None -> l
    | Some d -> desc_field d :: l

  let obj_typ = item ~key:"type" (string "object")
  let record_type desc fields required =
    [ obj_typ;
      desc;
      item ~key:"properties" @@ obj fields;
      item ~key:"required" @@ array required
    ]

  let one_of l = item ~key:"oneOf" (array l)
  let const name = item ~key:"const" @@ string name
  let sum ~v ~desc x =
    let brule desc name core =
      let name = const name in
      let forward_record =
        let kcontents = "contents" in
        let contents = item ~key:kcontents (obj [tuple_typ core]) in
        let next = item ~key:"next" (obj [any_typ]) in
        let desc = desc_field "expanded record for forward compatibility" in
        record_type desc [contents; next] [string kcontents]
      in
      obj [one_of [
        obj (with_desc desc [tuple_typ (name::core)]);
        obj (with_desc desc [tuple [obj [name]; obj forward_record]])
      ]]
    in
    let constructor (name, kty) =
      match kty.ltyp with
      | T Unit -> obj [const name]
      | T (Pair(x,y)) -> brule kty.desc name [typ x; typ y]
      | T (Triple(x,y,z)) -> brule kty.desc name [typ x; typ y; typ z]
      | T (Quadruple(x,y,z,w)) ->
          brule kty.desc name [typ x; typ y; typ z; typ w]
      | T ty -> brule kty.desc name [typ ty]
    in
    obj [
      desc_field desc;
      one_of (List.map constructor (field_infos ~version:(Some v) x))
    ]

  let field (v, (key, {status; ltyp=T ty; desc })) =
    let stage = Diagnostic_history.Lifetime.stage_at (Some v) status in
    match stage with
    | Future | Deletion -> None
    | _ ->
        let typ = typ ty in
        let fields =
          match stage with
          | Deprecation ->
              let deprecated = item ~key:"deprecated" (bool true) in
              (with_desc desc [typ; deprecated])
          | _ -> (with_desc desc [typ])
        in
        Some (item ~key (obj fields))

  let fields x = List.filter_map field x

  let required_fields x =
    List.filter_map
      (fun (_, (k, kinfo)) ->
         if is_optional kinfo then None else Some(string k)
      )
      x

  let schema_field =
    item ~key:"schema" @@ obj [obj_typ]

  let record_fields ~desc x =
    record_type (desc_field desc) (fields x) (required_fields x)

  let simple_record ~desc x = obj (record_fields ~desc x)


  let uniform_version v l = List.map (fun x -> v,x) l

  let def_printer v = function
      | T (Sum x) -> sum ~v ~desc:(scheme_description x) x
      | T (Record x) ->
        simple_record ~desc:(scheme_description x)
          (uniform_version v @@ field_infos ~version:(Some v) x)
      | _ -> ignore



   let pp v ~with_deps ~roots sch ppf =
     let keys = uniform_version v @@ field_infos ~version:(Some v) sch in
     let root = String_set.mem (scheme_name sch) roots in
     let keys =
       if root then (Defs.metadata_version, metakey) :: keys else keys
     in
     let defs =
       if not with_deps then [] else
         let refs = Defs.subrefs None (List.map snd keys) String_map.empty in
         if String_map.is_empty refs then []
         else
           let prs =
             List.map (fun (key,(vo,ty)) ->
                 let v = Option.value ~default:v vo in
                 item ~key (def_printer v ty)
               )
               (String_map.bindings refs)
           in
           [item ~key:"$defs" @@ obj prs]
     in
     obj (
       header v (scheme_name sch)
       @ defs
       @ schema_field :: record_fields ~desc:(scheme_description sch) keys
     ) ppf

   let pp_type v ~with_deps ~roots ppf ty = match ty with
     | T (Sum sch) -> pp ~with_deps ~roots v sch ppf
     | T (Record sch) -> pp ~with_deps ~roots v sch ppf
     | _ -> ()

  end

module Annotated_adt = struct

  open Diagnostic
  let time ppf () = Format.fprintf ppf "@ *@ "
  let tuple ~parentheses components ppf =
    let pr = Format.pp_print_list ~pp_sep:time (|>) in
    if parentheses then Format.fprintf ppf "@[(%a)@]" pr components
    else Format.fprintf ppf "@[%a@]" pr components

  let string s ppf = Format.pp_print_string ppf s

  let rec typ: type a.  parentheses:bool -> a typ -> Format.formatter -> unit =
    fun ~parentheses x ->
    let t x = typ ~parentheses:true x in
    let tuple = tuple ~parentheses in
    match x with
    | Int -> string "int"
    | Bool ->  string "bool"
    | Unit -> string "int"
    | String -> string "string"
    | Float -> string "float"
    | List e ->
        Format.dprintf "%t array" (typ ~parentheses  e)
    | Pair (x,y) -> tuple [t x; t y]
    | Triple (x,y,z) -> tuple [t x; t y; t z]
    | Quadruple (x,y,z,w) -> tuple [t x;t y; t z; t w]
    | Sum x -> string (scheme_name x)
    | Record x -> string (scheme_name x)
    | Custom x -> typ ~parentheses x.default

  let break_if_not_empty ppf = function
    | [] -> ()
    | _ -> Format.pp_print_space ppf ()

  let pp_stage ?parent lifetime_phases ppf x =
    let pp_phase ppf (name,v) = match parent, name with
      | Some parent, "preview" ->
          Format.fprintf ppf "[@@preview %d.%d %s]" v.major v.minor parent
      | _ ->
        Format.fprintf ppf "[@@%s %d.%d]" name v.major v.minor
    in
    let group (name,proj) = Option.map (fun v -> name, v ) (proj x) in
    let phases = List.filter_map group lifetime_phases in
    break_if_not_empty ppf phases;
    Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_phase ppf phases

  let factorize_stage l (name,proj as p) = match l with
    | [] -> Either.Right p
    | (_,a) :: q ->
        match proj a.status with
        | None -> Either.Right p
        | Some f as sf ->
            if List.for_all (fun (_,x) -> sf = proj x.status) q then
              Either.Left (name, f)
            else Either.Right p

  let split_stages stages l =
    List.partition_map (factorize_stage l) stages

  let pp_common_stage ppf l =
    let pp_stage ppf (name,v) =
      Format.fprintf ppf "[@@@@%s %d.%d]" name v.major v.minor
    in
    Format.pp_print_list pp_stage ~pp_sep:Format.pp_print_cut ppf l

  let pp_version_attribute ppf v = Format.fprintf ppf
      {|[@@@@version %a]|} pp_version v

  let lifetime_phases =
    let open Lifetime in
    [ "preview", (fun x -> x.inception);
      "since", (fun x -> x.publication);
      "expanded", (fun x -> x.expansion);
      "deprecated", (fun x -> x.deprecation);
      "deleted", (fun x -> x.deletion);
    ]

  let pp_desc ppf desc = Option.iter (Format.fprintf ppf "@ (**%s*)") desc

  let sum ~version x ppf =
    let constructor stages ppf (name, kty) =
      match kty.ltyp with
      | T Unit ->
          Format.fprintf ppf "@ | %s%a%a"
          name (pp_stage ?parent:kty.parent stages) kty.status
          pp_desc kty.desc
      | T t ->
        Format.fprintf ppf "@ @[<2>| %s of@ %t%a%a@]"
          name (typ ~parentheses:false t)
          (pp_stage ?parent:kty.parent stages) kty.status
          pp_desc kty.desc
    in
    let fields = field_infos ~version:(Some version) x in
    let common, specific = split_stages lifetime_phases fields in
    List.iter (constructor specific ppf) fields;
    Format.fprintf ppf "@]%a%a@ %a@]"
    break_if_not_empty common
    pp_common_stage common
    pp_version_attribute version


  let record ~version ~root x ppf =
    let pp_typ_field opt ppf ty =
      if opt then
        Format.fprintf ppf "%t option [@@optional]" (typ ~parentheses:true ty)
      else typ ~parentheses:false ty ppf
    in
    let field phases ppf (name, { ltyp=T ty; optional; status; desc }) =
      Format.fprintf ppf "@ @[<2>%s:@ %a%a%a;@]"
        name (pp_typ_field optional) ty
        (pp_stage phases) status
        pp_desc desc
    in
    let fields = field_infos ~version:(Some version) x in
    let common, specific = split_stages lifetime_phases fields in
    Format.fprintf ppf " {";
    if root then
      Format.fprintf ppf "@ @[<2>metadata: metadata%a;@]"
        pp_desc (snd metakey).desc;
    List.iter (field specific ppf) fields;
    Format.fprintf ppf "@;<1 -2>}@] @]%a@,%a"
      pp_common_stage common
      pp_version_attribute version

  let def version roots (T x) = match x with
    | Sum x -> Some (scheme_name x, sum ~version x)
    | Record x ->
        let name = scheme_name x in
        let root = String_set.mem name roots in
        Some (name, record ~version ~root x)
    | _ -> None

  let pp_def v roots ppf ty =
    Option.iter (fun (name,pr) ->
        Format.fprintf ppf "@[@[<hv 2>type %s =%t" name pr
      ) (def v roots ty)

  let pp v ~with_deps ~roots ppf (T ty as rty) =
    let pp_def = pp_def v roots in
    if not with_deps then
      Format.fprintf ppf "%a" pp_def rty
    else
      let defs = Defs.refs (Some v) ty in
      let pp_sep ppf () = Format.fprintf ppf "@,@," in
      Format.fprintf ppf "@[<v>%a@]"
        Format.(pp_print_seq ~pp_sep pp_def)
        (Seq.map snd @@ Seq.map snd @@ String_map.to_seq defs)
 end

let with_formatter name f =
  match !Options.filename_template with
  | None -> f Format.std_formatter
  | Some file ->
      Out_channel.with_open_bin (file name) (fun out ->
          let ppf = Format.formatter_of_out_channel out in
          f ppf;
          Format.pp_print_flush ppf ()
        )

open Compiler_diagnostic


module Pp = struct
  open Format
  module Vmap = Map.Make(struct
      type t = Diagnostic_history.version
      let compare: t -> t -> int = Stdlib.compare
    end)

  module String_map = Map.Make(String)

  let group_by_version_then_scheme event_seq =
    let open Diagnostic_history in
    let add (m,errors) e =
      let map_at_v =
        Option.value ~default:String_map.empty (Vmap.find_opt e.version m) in
      let prev =
        Option.value ~default:[] (String_map.find_opt e.scheme map_at_v)
      in
      let map_at_v = String_map.add e.scheme (e.event::prev) map_at_v in
      let errors = match e.event with Error e -> e :: errors | _ -> errors in
      Vmap.add e.version map_at_v m, errors
    in
    Seq.fold_left add (Vmap.empty,[]) event_seq


  let status ppf range =
    match Lifetime.stage range with
    | Lifetime.Inception -> fprintf ppf "refined"
    | Lifetime.Publication -> fprintf ppf "created"
    | Lifetime.Expansion -> fprintf ppf "expanded"
    | Lifetime.Deprecation -> fprintf ppf "deprecated"
    | Lifetime.Deletion -> fprintf ppf "deleted"
    | Lifetime.Future -> fprintf ppf "future"


  let error ppf =
    let open Diagnostic_history in
    function
    | Time_travel (v,x) ->
        fprintf ppf "Error: future key (%a<%a)" pp v pp x
    | Duplicate_key s -> fprintf ppf "Error: duplicate %s" s
    | Invalid_constructor_expansion s ->
        fprintf ppf "Error: second constructor expansion %s" s
    | Invalid_publication s ->
        fprintf ppf "Error: second constructor publication %s" s
    | Inconsistent_change (range,key_name) ->
        fprintf ppf "Error inconsistent change of the %a key %s"
          status range
          key_name
    | Sealed_version v -> fprintf ppf "Error: seal breach %a" pp v

  let base_event ppf =
    function
    | Inception r ->
        fprintf ppf "Inception: %s>%s,%s" r.base_name r.new_name r.typ
    | Declaration -> fprintf ppf "Declaration"
    | Publication name -> fprintf ppf "Publication %s" name
    | Creation {name;typ} ->
        if typ = "" then fprintf ppf "New label %s" name
        else fprintf ppf "New label %s, %s" name typ
    | Expansion {name;expansion} ->
        fprintf ppf "Constructor %s>%s" name expansion
    | Make_required name -> fprintf ppf "Newly required %s" name
    | Deprecation name -> fprintf ppf "Deprecation %s" name
    | Seal -> fprintf ppf "Seal"
    | Deletion name -> fprintf ppf "Deletion %s" name
    | Error e -> error ppf e

  let scheme_at_v ppf (scheme_name,events) =
    Format.fprintf ppf "@[<v 2>%s@,%a@]"
      scheme_name
      (pp_print_list base_event) (List.rev events)

  let events_by_version_then_scheme ppf (version, map_at_v) =
    Format.fprintf ppf "@[<v 2>%a@," pp version;
    pp_print_seq scheme_at_v ppf (String_map.to_seq map_at_v);
    Format.fprintf ppf "@]"

  let errors ppf = function
    | [] -> ()
    | errors ->
        fprintf ppf "@[<v 2>Invalid diagnostic history@,%a@]"
          (pp_print_list error) errors

  let history ppf h =
    let events = events h in
    let m, err = group_by_version_then_scheme events in
    fprintf ppf "@[<v>%a%a@]"
      errors err
      (pp_print_seq events_by_version_then_scheme) (Vmap.to_seq m);
    if not (List.is_empty err) then exit 2
end

let history () =
  if !Options.history then
    with_formatter "history"
    (Format.dprintf
      "@[<v 2>Metadata:@,%a@;<0 -2>\
      Config:@,%a@;<0 -2>\
       Main:@,%a@]@."
      Pp.history Diagnostic.Metadata_versions.history
      Pp.history Conf_diagnostic.Versions.history
      Pp.history V.history
    )


let config_version =
  Diagnostic_history.current_version Conf_diagnostic.Versions.history


let schemas compiler_version =
  String_map.empty
  |> Defs.add_refs
    (Some Defs.metadata_version)
    (Record Diagnostic.Metadata.scheme)
  |> Defs.add_refs (Some config_version) (Record Conf_diagnostic.scheme)
  |> Defs.add_refs (Some compiler_version) (Record Compiler_diagnostic.scheme)
  |> Defs.add_refs (Some compiler_version) (Record Toplevel_diagnostic.scheme)

let roots =
  String_set.of_list Diagnostic.[
    scheme_name Conf_diagnostic.scheme;
    scheme_name Compiler_diagnostic.scheme;
    scheme_name Toplevel_diagnostic.scheme;
  ]

let pp_schema version schemas name =
  match String_map.find_opt name schemas with
  | None ->  Format.eprintf "Unknown schema name: %s@." name
  | Some (v, sch) ->
      let version = Option.value ~default:version v in
      let with_deps = !Options.with_deps in
      let printer = match !Options.format with
        | Options.Json -> JSchema.pp_type ~roots ~with_deps version
        | Options.Adt -> Annotated_adt.pp ~roots ~with_deps version
      in
      with_formatter name (fun ppf ->
      match !Options.output_template with
      | None -> Format.fprintf ppf "%a@." printer sch
      | Some template -> template ppf printer sch
        )

let () =
  Arg.parse Options.args ignore "print log information";
  let version = Options.parse_version () in
  let schemas = schemas version in
  history ();
  match !Options.name with
  | None -> ()
  | Some (Options.All) ->
      String_map.iter (fun name _ -> pp_schema version schemas name) schemas
  | Some (Options.One x) -> pp_schema version schemas x
