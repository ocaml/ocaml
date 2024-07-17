(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Hugo Heuzard                              *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Style = Misc.Style

type compunit = string

type filename = string

type compunit_and_source = {
  compunit  : compunit;
  filename : filename;
}

module Compunit_and_source = struct
  type t = compunit_and_source
  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
end

type refs = Compunit_and_source.Set.t

type t = {
  complete : bool;
  missing_compunits : (compunit, refs) Hashtbl.t;
  provided_compunits : (compunit, filename list) Hashtbl.t;
  badly_ordered_deps : (Compunit_and_source.t, refs) Hashtbl.t;
}

type error =
  | Missing_implementations of (compunit * compunit_and_source list) list
  | Wrong_link_order of (compunit_and_source * compunit_and_source list) list
  | Multiple_definitions of (compunit * filename list) list

let create ~complete = {
  complete;
  missing_compunits = Hashtbl.create 17;
  provided_compunits = Hashtbl.create 17;
  badly_ordered_deps = Hashtbl.create 17;
}

let required t compunit = Hashtbl.mem t.missing_compunits compunit

let update t k f =
  let v = Hashtbl.find_opt t k in
  Hashtbl.replace t k (f v)

let add_required t by (name : string) =
  let add s =
    Compunit_and_source.Set.add by
      (Option.value s ~default:Compunit_and_source.Set.empty) in
  (try
     let filename = List.hd (Hashtbl.find t.provided_compunits name) in
     update t.badly_ordered_deps {compunit = name; filename } add
   with Not_found -> ());
  update t.missing_compunits name add

let add t ~filename ~compunit ~provides ~requires =
  List.iter (add_required t {compunit; filename}) requires;
  List.iter (fun p ->
    Hashtbl.remove t.missing_compunits p;
    let l = Option.value ~default:[]
        (Hashtbl.find_opt t.provided_compunits p) in
    Hashtbl.replace t.provided_compunits p (filename :: l)) provides

let check t =
  let of_seq s =
    Seq.map (fun (k,v) -> k, Compunit_and_source.Set.elements v) s
    |> List.of_seq
  in
  let missing = of_seq (Hashtbl.to_seq t.missing_compunits) in
  let badly_ordered_deps = of_seq (Hashtbl.to_seq t.badly_ordered_deps) in
  let duplicated =
    Hashtbl.to_seq t.provided_compunits
    |> Seq.filter (fun (_, files) -> List.compare_length_with files 1 > 0)
    |> List.of_seq
  in
  match duplicated, badly_ordered_deps, missing with
  | [], [], [] -> None
  | [], [], l ->
      if t.complete
      then Some (Missing_implementations l)
      else None
  | [], l,  _  ->
      Some (Wrong_link_order l)
  | l, _, _ ->
      Some (Multiple_definitions l)

(* Error report *)

open Format_doc

let print_reference print_fname ppf {compunit; filename} =
  fprintf ppf "%a (%a)" Style.inline_code compunit print_fname filename

let pp_list_comma f =
  pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") f

let report_error_doc ~print_filename ppf = function
  | Missing_implementations l ->
      let print_modules ppf =
        List.iter
          (fun (md, rq) ->
             fprintf ppf "@ @[<hov 2>%a referenced from %a@]"
               Style.inline_code md
               (pp_list_comma (print_reference print_filename)) rq)
      in
      fprintf ppf
        "@[<v 2>No implementation provided for the following modules:%a@]"
        print_modules l
  | Wrong_link_order l ->
      let depends_on ppf (dep, depending) =
        fprintf ppf "@ @[<hov 2>%a depends on %a@]"
          (pp_list_comma (print_reference print_filename)) depending
          (print_reference print_filename) dep
      in
      fprintf ppf "@[<hov 2>Wrong link order:%a@]"
        (pp_list_comma depends_on) l
  | Multiple_definitions l ->
      let print ppf (compunit, files) =
        fprintf ppf
          "@ @[<hov>Multiple definitions of module %a in files %a@]"
          Style.inline_code compunit
          (pp_list_comma (Style.as_inline_code print_filename)) files

      in
      fprintf ppf "@[<hov 2> Duplicated implementations:%a@]"
        (pp_list_comma print) l

let report_error ~print_filename =
  Format_doc.compat (report_error_doc ~print_filename)
