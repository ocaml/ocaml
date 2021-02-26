(** Check reference to manual section in ml files

    [cross-reference-cheker -auxfile tex.aux src.ml ]
    checks that all expression and let bindings in [src.ml] annotated
    with [[@manual.ref "tex_label"]] are integer tuple literals, e.g
    {[
      let[@manual.ref "sec:major"] ref = 1, 1
      (* or *)
      let ref = (3 [@manual.ref "ch:pentatonic"])
    ]}
    and that their values are consistent with the computed references for the
    payload labels (e.g "sec:major", "ch:pentatonic") present in the TeX
    auxiliary file [tex.aux]

*)


(** {1 Error printing } *)
type error =
  | Reference_mismatch of
      {loc:Location.t; label:string; ocaml:int list; tex:int list}
  | Unknown_label of Location.t * string
  | Tuple_expected of Location.t
  | No_aux_file
  | Wrong_attribute_payload of Location.t

let pp_ref ppf = Format.pp_print_list ~pp_sep:( fun ppf () ->
    Format.pp_print_string ppf ".") Format.pp_print_int ppf

let print_error error =
  Location.print_report Format.std_formatter @@ match error with
  | Tuple_expected loc ->
      Location.errorf ~loc
        "Integer tuple expected after manual reference annotation@."
  | Unknown_label (loc,label) ->
    Location.errorf ~loc
      "@[<hov>Unknown manual label:@ %s@]@." label
  | Reference_mismatch r ->
    Location.errorf ~loc:r.loc
      "@[<v 2>References for label %S do not match:@,\
       OCaml side %a,@,\
       manual     %a@]@."
      r.label
      pp_ref r.ocaml
      pp_ref r.tex
  | No_aux_file ->
      Location.errorf "No aux file provided@."
  | Wrong_attribute_payload loc ->
      Location.errorf ~loc "Wrong payload for \"@manual.ref\"@."


(** {1 Main types} *)

(** Maps of ocaml reference to manual labels *)
module Refs = Map.Make(String)

(** Reference extracted from TeX aux files *)
type tex_reference =
  { label: string;
    pos: int list;
    level: string
  }

type status = Ok | Bad | Unknown

(** Reference extracted from OCaml source files *)
type ml_reference = { loc: Location.t; pos: int list; status:status }

(** {1 Consistency check } *)

let check_consistency (ref:tex_reference) {loc; pos; _ } =
  if ref.pos = pos then
    { loc; pos; status = Ok }
  else begin
    print_error @@ Reference_mismatch {loc;label=ref.label;tex=ref.pos;ocaml=pos};
    {loc; pos;  status = Bad }
  end

let rec check_final_status label error = function
  | { status = Ok; _ } -> error
  | { status = Bad; _ } -> true
  | { status = Unknown; loc; _} ->
      print_error (Unknown_label (loc,label));
      true

(** {1 Data extraction from TeX side} *)

module TeX = struct

  (** Read reference information from a line of the aux file *)
  let scan s =
    try
      Scanf.sscanf s
        "\\newlabel{%s@}{{%s@}{%_d}{%_s@}{%s@.%_s@}{%_s@}}"
        (fun label position_string level ->
           let pos =
             List.map int_of_string (String.split_on_char '.' position_string) in
           Some {label;level;pos} )
    with
    | Scanf.Scan_failure _ -> None
    | Failure _ -> None

  let check_line refs line =
    match scan line with
    | None -> refs
    | Some ref ->
        match Refs.find_opt ref.label refs with
        | None -> refs
        | Some l ->
            Refs.add ref.label
              (List.map (check_consistency ref)  l)
              refs

  let check_all aux refs =
    let chan = open_in aux in
    let rec lines refs =
      let s = try Some (input_line chan)  with End_of_file -> None in
      match s with
      | None -> refs
      | Some line ->
          lines @@ check_line refs line in
    let refs = lines refs in
    close_in chan;
    let error = Refs.fold (fun label ocaml_refs error ->
        List.fold_left (check_final_status label) error ocaml_refs)
        refs false in
    if error then exit 2 else exit 0
end

(** {1 Extract references from Ocaml source files} *)
module OCaml_refs = struct

  let parse sourcefile  =
    Pparse.parse_implementation ~tool_name:"manual_cross_reference_check"
      sourcefile

  (** search for an attribute [[@manual.ref "tex_label_name"]] *)
  let manual_reference_attribute attr =
    let open Parsetree in
    if attr.attr_name.Location.txt <> "manual.ref"
    then None
    else begin match attr.attr_payload with
      | PStr [{pstr_desc= Pstr_eval
                 ({ pexp_desc = Pexp_constant Pconst_string (s,_,_) },_) } ] ->
          Some s
      | _ -> print_error (Wrong_attribute_payload attr.attr_loc);
          Some "" (* triggers an error *)
    end

  let rec label_from_attributes = function
    | [] -> None
    | a :: q -> match manual_reference_attribute a with
      | Some _ as x -> x
      | None -> label_from_attributes q

  let int e =
    let open Parsetree in
    match e.pexp_desc with
    | Pexp_constant Pconst_integer (s, _ ) -> int_of_string s
    | _ -> raise Exit

  let int_list l =
    try Some (List.map int l) with
    | Exit -> None

  (** We keep a list of OCaml-side references to the same label *)
  let add_ref label ref refs =
    let l = match Refs.find_opt label refs with
      | None -> [ref]
      | Some l -> ref :: l in
    Refs.add label l refs

  let inner_expr loc e =
    let tuple_expected () = print_error (Tuple_expected loc) in
    match e.Parsetree.pexp_desc with
          | Parsetree.Pexp_tuple l ->
              begin match int_list l with
              | None -> tuple_expected (); []
              | Some pos -> pos
              end
          | Parsetree.Pexp_constant Pconst_integer (n,_) ->
              [int_of_string n]
          | _ -> tuple_expected (); []

  (** extract from [let[@manual.ref "label"] x= 1, 2] *)
  let value_binding m iterator vb =
    let open Parsetree in
    begin match label_from_attributes vb.pvb_attributes with
    | None -> ()
    | Some label ->
        let pos = inner_expr vb.pvb_loc vb.pvb_expr in
        m := add_ref label {loc = vb.pvb_loc; pos; status = Unknown } !m
    end;
    iterator.Ast_iterator.expr iterator vb.pvb_expr


  (** extract from [ (1,2)[@manual.ref "label"]] *)
  let expr m iterator e =
    let open Parsetree in
    begin match label_from_attributes e.pexp_attributes with
    | None -> ()
    | Some label ->
        let pos = inner_expr e.pexp_loc e in
        m := add_ref label {loc = e.pexp_loc; pos; status = Unknown } !m
    end;
    Ast_iterator.default_iterator.expr iterator e

  let from_ast m ast =
    let iterator =
      let value_binding = value_binding m in
      let expr = expr m in
      Ast_iterator.{ default_iterator with value_binding; expr } in
    iterator.structure iterator ast

  let from_file m f =
    from_ast m @@ parse f
end


(** {1 Argument handling and main function } *)

let usage =
  "cross-reference-check -auxfile [file.aux] file_1 ... file_n checks that \
   the cross reference annotated with [@manual_cross_reference] are consistent \
   with the provided auxiliary TeX file"

(** the auxiliary file containing reference to be checked *)
let aux_file = ref None

let args =
  [
    "-auxfile",Arg.String (fun s -> aux_file := Some s),
    "set the reference file"
  ]

let () =
  let m = ref Refs.empty in
  Arg.parse args (OCaml_refs.from_file m) usage;
  match !aux_file with
  | None -> print_error No_aux_file; exit 2
  |  Some aux ->
      let error = TeX.check_all aux !m  in
      if error then exit 2 else exit 0
