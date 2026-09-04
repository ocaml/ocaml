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

open Diagnostic
module V = Diagnostic_history.Make()
let v1 = V.new_version { major=0; minor=1}

module type Record = Record with type vl := V.id
module type Sum = Sum with type vl := V.id

type doc = Format_doc.t
module Structured_text = struct
  module Doc = Format_doc.Doc
  module Box_type = struct
    include New_sum(V)
        (struct
          let name = "box_type"
          let description = "Format box kind (h,v,hv, b, hov)"
          let update = v1
        end
        )()
    let h = new_constr0 v1 "H"
    let v = new_constr0 v1 "V"
    let hv = new_constr0 v1 "HV"
    let hov = new_constr0 v1 "HoV"
    let b = new_constr0 v1 "B"
    let () = seal v1
    type _ extension += Box_type: Doc.box_type extension
    let typ =
      let pull version = function
        | Doc.H -> app version h ()
        | Doc.V -> app version v ()
        | Doc.HoV -> app version hov ()
        | Doc.HV -> app version hv ()
        | Doc.B -> app version b ()
      in
      Custom { id = Box_type; pull; default = raw_type}
  end

  module Format_tag = struct
    include New_sum(V)
        (struct
          let name = "format_tag"
          let description = "Format semantic tag"
          let update = v1
        end
        )()

    let unknown = new_constr v1 "Unknown_format_tag" String
    let string_tag = new_constr v1 "String_tag" String

    type _ extension += Format_tag: Format.stag extension
    type format_tag_serializer =
      Diagnostic_history.version option -> Format.stag -> raw_type
    let map: (Obj.Extension_constructor.t, format_tag_serializer) Hashtbl.t =
      Hashtbl.create 5
    let register_tag ext conv = Hashtbl.replace map ext conv
    let typ =
      let pull v = function
        | Format.String_tag s -> app v string_tag s
        | x ->
            let ext = Obj.Extension_constructor.of_val x in
            match Hashtbl.find map ext with
            | exception Not_found ->
                app v unknown (Obj.Extension_constructor.name ext)
            | f -> f v x
      in
      Custom { id = Format_tag; pull; default = raw_type}

    let register_tag0 v ext =
      let name = Obj.Extension_constructor.name ext in
      let name = match String.rindex name '.' with
        | exception Not_found -> name
        | dot -> String.sub name (dot+1) (String.length name - dot -1)
      in
      let constr = new_constr0 v name in
      register_tag ext (fun v _ -> app v constr ())

   let () =
      Array.iter (register_tag0 v1)
        Misc.Style.[|
          [%extension_constructor Error];
          [%extension_constructor Warning];
          [%extension_constructor Loc];
          [%extension_constructor Inline_code];
          [%extension_constructor Hint];
          [%extension_constructor Deletion];
          [%extension_constructor Insertion];
          [%extension_constructor Modification];
          [%extension_constructor Preservation];
        |];
      seal v1

  end


  include New_sum(V)
    (struct
      let name = "structured_text"
      let description =
        "Structured text, using Format_doc.t as a serialization format for \
         Format printers."
      let update = v1
    end)
    ()

  let text = new_constr v1 "Text" String
  let tab_break = new_constr v1 "Tab_break" (Pair(Int,Int))
  let set_tab = new_constr0 v1 "Set_tab"
  let simple_break = new_constr v1 "Simple_break" (Pair(Int,Int))
  let break =
    let alt = Triple(String,Int,String) in
    new_constr v1 "Break" (Pair(alt,alt))
  let flush = new_constr v1 "Flush" Bool
  let newline = new_constr0 v1 "Newline"
  let if_newline = new_constr0 v1 "If_newline"



  let deprecated = new_constr v1 "Deprecated" String

  type _ extension += Doc: Doc.t extension
  let with_size = new_constr v1 "With_size" (Pair(Int,String))
  let box = new_constr v1 "Box" (Triple(Box_type.typ,Int,List raw_type))
  let tag = new_constr v1 "Tag" (Pair(Format_tag.typ,List raw_type))
  let tbox = new_constr v1 "Tbox" (List raw_type)

  let typ =
    let rec tree_pull v =
      let open Doc.Tree in
      function
      | Core (Text x) -> app v text x
      | With_size {size;text} -> app v with_size (size, text)
      | Box r -> app v box (r.kind, r.indent, trees v r.subtrees)
      | Tagged t -> app v tag (t.tag, trees v t.subtrees)
      | Tbox s -> app v tbox (trees v s)
      | Core (Tab_break t) -> app v tab_break (t.width,t.offset)
      | Core Set_tab -> app v set_tab ()
      | Core (Simple_break r) -> app v simple_break (r.spaces, r.indent)
      | Core (Break r) -> app v break (r.fits, r.breaks)
      | Core (Flush r) -> app v flush r.newline
      | Core Newline -> app v newline ()
      | Core If_newline -> app v if_newline ()
      | Core (Deprecated pr) -> app v deprecated (Format.asprintf "%t" pr)
    and trees v = List.map (tree_pull v) in
    let default = List raw_type in
    let pull v d = trees v (Doc.Tree.parse d) in
    Custom {id = Doc; default; pull }
  let () = seal v1



  let register_tag = Format_tag.register_tag
  let register_tag0 = Format_tag.register_tag0

 end



module Debug = struct
  include New_record(V)
      (struct
        let name = "debug"
        let description = "Debugging output for compiler developers"
        let update = v1
      end)
      ()
  let slist = List String

  let parsetree = new_field_opt v1 "parsetree" String
  let source = new_field_opt v1 "source" String
  let typedtree = new_field_opt v1 "typedtree" String
  let shape = new_field_opt v1 "shape" String
  let instr = new_field_opt v1 "instr" String
  let lambda = new_field_opt v1 "lambda" String
  let raw_lambda = new_field_opt v1 "rawlambda" String
  let flambda = new_field_opt v1 "flambda" slist
  let raw_flambda = new_field_opt v1 "rawflambda" slist
  let clambda = new_field_opt v1 "clambda" slist
  let raw_clambda = new_field_opt v1 "raw_clambda" slist
  let cmm = new_field_opt v1 "cmm" slist
  let remove_free_vars_equal_to_args =
    new_field_opt v1 "remove_free_vars_equal_to_args" slist
  let unbox_free_vars_of_closures =
    new_field_opt v1 "unbox_free_vars_of_closures" slist
  let unbox_closures = new_field_opt v1 "unbox_closures" slist
  let unbox_specialised_args = new_field_opt v1 "unbox_specialised_args" slist
  let mach = new_field_opt v1 "mach" slist
  let linear = new_field_opt v1 "linear" slist
  let cmm_invariant = new_field_opt v1 "cmm_invariant" String
end

module Error =
  New_record(V)
    (struct
      let name = "error_report"
      let description = "Error and warning reports"
      let update = v1
    end)
    ()

include New_record(V)
    (struct
      let name = "compiler"
      let description = "Compiler diagnostic for the OCaml compiler"
      let update = v1
    end)
    ()
let debug = new_field_opt v1  "debug" (Record Debug.scheme)

let doc = Structured_text.typ
let ldoc = List Structured_text.typ
