(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* To print values *)

open Format
open Parser_aux
open Types

(* To name printed and ellipsed values *)

let named_values =
  (Hashtbl.create 29 : (int, Debugcom.Remote_value.t * type_expr) Hashtbl.t)
let next_name = ref 1

let reset_named_values () =
  Hashtbl.clear named_values;
  next_name := 1

let name_value v ty =
  let name = !next_name in
  incr next_name;
  Hashtbl.add named_values name (v, ty);
  name

let find_named_value name =
  Hashtbl.find named_values name

let check_depth depth obj ty =
  if depth <= 0 then begin
    let n = name_value obj ty in
    Some (Outcometree.Oval_stuff ("$" ^ Int.to_string n))
  end else None

module EvalPath =
  struct
    type valu = Debugcom.Remote_value.t
    exception Error
    let rec eval_address = function
    | Env.Aident id ->
      begin match Symtable.Global.of_ident id with
        | Some global ->
          begin
            try Debugcom.Remote_value.global (Symtable.get_global_position
              global)
            with Symtable.Error _ -> raise Error
          end
        | None -> raise Error
      end
    | Env.Adot(root, pos) ->
        let v = eval_address root in
        if not (Debugcom.Remote_value.is_block v)
        then raise Error
        else Debugcom.Remote_value.field v pos
    let same_value = Debugcom.Remote_value.same
  end

module Printer = Genprintval.Make(Debugcom.Remote_value)(EvalPath)

let max_printer_depth = ref 20
let max_printer_steps = ref 300

let descriptors = lazy (
  let index = Introspect.Index.make () in
  begin match Debugcom.remote_block_descriptors () with
  | Ok descs -> Introspect.Index.register_list index descs
  | Error msg -> prerr_endline ("Descriptors initialization: " ^ msg)
  end;
  index
)

module H = Hashtbl.Make(struct type t = Obj.t let equal = (==) let hash = Hashtbl.hash end)

let opaque_printer _kind obj =
  match Debugcom.Remote_value.obj obj with
  | Result.Error msg ->
      Some (Outcometree.Oval_stuff msg)
  | Result.Ok obj ->
      let obj = Obj.repr obj in
      let open Introspect in
      let list_fields f fields =
        let acc = ref [] in
        for i = Dyn.field_count fields - 1 downto 0 do
          acc := f (Dyn.field_get fields i) :: !acc
        done;
        !acc
      in
      let table = H.create 7 in
      let rec print depth obj =
        if depth <= 0 then
          Outcometree.Oval_ellipsis
        else if H.mem table (Dyn.get_obj obj) then
          Outcometree.Oval_stuff "<cycle>"
        else
          let open Outcometree in
          let oide_ident printed_name = Oide_ident {printed_name} in
          match Dyn.view ~index:(Lazy.force descriptors) obj with
          | String str ->
              Oval_string (str, 70, Ostr_string)
          | Float f -> Oval_float f
          | Int_or_constant (n, []) -> Oval_int n
          | Char n -> Oval_char n
          | Int_or_constant (n, name :: _) ->
              Oval_stuff (Printf.sprintf "%d or `%s" n name)
          | Constant names ->
              Oval_stuff (String.concat " or " names)
          | Array fields ->
              H.add table (Dyn.get_obj obj) ();
              Oval_array (list_fields (print (depth - 1)) fields, Mutable)
          | Tuple {name; fields} ->
              H.add table (Dyn.get_obj obj) ();
              let pf (k,v) =
                let k = if k = "" then None else Some k in
                (k, print (depth - 1) v)
              in
              let tuple = list_fields pf fields in
              if name = "" then
                Oval_tuple tuple
              else begin
                let prj = function (None, v) -> v | (Some _, _) -> raise Exit in
                match List.map prj tuple with
                | fields -> Oval_constr (oide_ident name, fields)
                | exception Exit ->
                    Oval_constr (oide_ident name, [Oval_tuple tuple])
              end
          | Record {name; fields} ->
              H.add table (Dyn.get_obj obj) ();
              let pf (k,v) = (oide_ident k, print (depth - 1) v) in
              let record = Oval_record (list_fields pf fields) in
              if name = ""
              then record
              else Oval_constr (oide_ident name, [record])
          | Polymorphic_variant (name, tuple) ->
              H.add table (Dyn.get_obj obj) ();
              Oval_variant (name, Some (print (depth - 1) tuple))
          | Closure  -> Oval_stuff "<closure>"
          | Lazy     -> Oval_stuff "<lazy>"
          | Abstract -> Oval_stuff "<abstract>"
          | Custom   -> Oval_stuff "<custom>"
          | Unknown  -> Oval_stuff "<unknown>"
      in
      Some (print !max_printer_depth (Dyn.lift obj))

let print_exception ppf obj =
  let t = Printer.outval_of_untyped_exception ~opaque_printer obj in
  !Oprint.out_value ppf t

let print_value max_depth env obj (ppf : Format.formatter) ty =
  let t =
    Printer.outval_of_value ~opaque_printer
      !max_printer_steps max_depth check_depth env obj ty
  in
  !Oprint.out_value ppf t

let print_named_value max_depth exp env obj ppf ty =
  let print_value_name ppf = function
  | E_ident lid ->
      Printtyp.longident ppf lid
  | E_name n ->
      fprintf ppf "$%i" n
  | _ ->
      let n = name_value obj ty in
      fprintf ppf "$%i" n in
  fprintf ppf "@[<2>%a:@ %a@ =@ %a@]@."
  print_value_name exp
  Printtyp.type_expr ty
  (print_value max_depth env obj) ty
