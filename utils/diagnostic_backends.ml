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
type version = Diagnostic_history.version

type 'a printer = Format.formatter -> 'a -> unit
type pr = Format.formatter -> unit

module Pp = struct

  type list_convention = {
    list_open: pr;
    list_close: pr;
    sep: pr;
  }

  type assoc = {
    assoc_open:pr;
    assoc_close: pr;
    sep: unit printer;
    open_with_label: pr;
    label_sep: pr;
    close_with_label: pr;
  }

  type conv = {
    string:string printer;
    inline_constant_constructor: bool;
    atom: string -> pr;
    assoc:assoc;
    list:list_convention;
  }

  let bool _conv b ppf = Format.pp_print_bool ppf b

  let escape_string ppf str =
    Format.fprintf ppf {|"|};
    for i = 0 to String.length str - 1 do
      match str.[i] with
      | '\\' -> Format.fprintf ppf {|\\|}
      | '\"' -> Format.fprintf ppf {|\"|}
      | '\n' -> Format.fprintf ppf {|\n|}
      | '\t' -> Format.fprintf ppf {|\t|}
      | '\r' -> Format.fprintf ppf {|\r|}
      | '\b' -> Format.fprintf ppf {|\b|}
      | '\x00' .. '\x1F' | '\x7F' as c ->
          Format.fprintf ppf "\\u%04X" (Char.code c)
      | c -> Format.fprintf ppf "%c" c
    done;
    Format.fprintf ppf {|"|}

  let item conv ~key elt ppf =
    conv.assoc.open_with_label ppf;
    conv.atom key ppf;
    conv.assoc.label_sep ppf;
    elt ppf;
    conv.assoc.close_with_label ppf

  let list conv prs ppf =
    let pp_sep ppf () = conv.list.sep ppf in
    conv.list.list_open ppf;
    Format.pp_print_list ~pp_sep (fun ppf pr -> pr ppf) ppf prs;
    conv.list.list_close ppf

  let tuple ~inline conv prs ppf =
    let pp_sep ppf () = conv.list.sep ppf in
    if not inline then conv.list.list_open ppf;
    Format.pp_print_list ~pp_sep (fun ppf pr -> pr ppf) ppf prs;
    if not inline then conv.list.list_close ppf

  let record conv fields ppf =
    if List.is_empty fields then () else begin
      conv.assoc.assoc_open ppf;
      Format.pp_print_list ~pp_sep:conv.assoc.sep
        (fun ppf pr -> pr ppf) ppf fields;
      conv.assoc.assoc_close ppf
    end


  let direct = {
    atom = (fun _s -> ignore);
    inline_constant_constructor = true;
    string = Format.pp_print_string;
    list = {
      list_open = ignore;
      list_close = ignore ;
      sep = Format.dprintf "@ ";
    };
    assoc = {
      assoc_open = Format.dprintf "@[<v>";
      assoc_close = Format.dprintf "@]";
      open_with_label = ignore;
      label_sep = ignore;
      sep = (fun ppf () -> Format.fprintf ppf "@,");
      close_with_label = ignore;
    }
  }

  let direct_with_fields =
    let assoc =
      { direct.assoc with label_sep = Format.dprintf ": " }
    in
    { direct with atom = Format.dprintf "%s"; assoc  }

  let sexp =
    let list_open = Format.dprintf "@[<hov 1>("
    and list_close = Format.dprintf "@,)@]"
    and sep = Format.dprintf "@ " in
    {
      atom = (fun s ppf -> Format.pp_print_string ppf s);
      string = escape_string;
      inline_constant_constructor = true;
      list = {list_open; list_close; sep };
      assoc = {
        assoc_open = list_open;
        assoc_close = list_close;
        open_with_label = Format.dprintf "@[<hov 1>(";
        sep = (fun ppf () -> sep ppf);
        label_sep = sep;
        close_with_label = Format.dprintf "@;<0 -1>)@]";
      }
    }

  let json =
    {
      string = escape_string;
      atom = (fun s ppf -> escape_string ppf s);
      inline_constant_constructor = false;
      list = {
        list_open=Format.dprintf "@[<b 2>[";
        list_close = Format.dprintf "]@]";
        sep = Format.dprintf ",@ ";
      };
      assoc = {
        assoc_open = Format.dprintf "@[<hv 2>{@ ";
        assoc_close = Format.dprintf "@;<1 -2>}@]";
        open_with_label = Format.dprintf "@[<b 2>";
        label_sep = Format.dprintf "@ :@ ";
        sep = (fun ppf () -> Format.fprintf ppf ",@ ");
        close_with_label = Format.dprintf "@]";
      }
    }


end

  type extension_printer =
    { extension: 'b. 'b Diagnostic.extension -> 'b printer option}

module Fmt = struct

  let no_extension = { extension = fun _ -> None }
  let doc_printer (type a): a extension -> a printer option =
    function
    | Compiler_diagnostic.Structured_text.Doc -> Some Format_doc.Doc.format
    | _ -> None
  let doc_extension = { extension = doc_printer  }
  let chain_extensions x y =
    let chain ext =
      match x.extension ext with
      | None -> y.extension ext
      |  Some _ as p -> p
    in
    { extension = chain }

  let extensions = ref doc_extension
  let add_extension x =
    extensions := chain_extensions x !extensions

  type ctx = {
    conv:Pp.conv;
    ext_printer:extension_printer;
    version:Diagnostic.version option
  }

  let rec scrap_custom: type t.
    version option -> t Diagnostic.typ -> t -> Diagnostic.typed_val =
    fun v t x ->
    match t with
    | Custom r -> scrap_custom v r.default (r.pull v x)
    | t -> V(t,x)

  open Pp
  let rec elt : type a. ?inline:bool -> ctx -> a Diagnostic.typ -> a -> pr =
    fun ?(inline=false) ctx typ x ppf ->
    match typ with
    | Unit -> Format.pp_print_int ppf 0
    | Int -> Format.pp_print_int ppf x
    | Float -> Format.pp_print_float ppf x
    | Bool -> bool ctx.conv x ppf
    | String -> ctx.conv.string ppf x
    | Pair (a,b) ->
        let x,y = x in
        tuple ~inline ctx.conv [
        elt ctx a x;
        elt ctx b y;
      ] ppf
    | Triple (a,b,c) ->
        let x, y, z = x in
        tuple ~inline ctx.conv [
        elt ctx a x;
        elt ctx b y;
        elt ctx c z;
      ] ppf
    | Quadruple (a,b,c,d) ->
        let x, y, z ,w = x in
        tuple ~inline ctx.conv [
        elt ctx a x;
        elt ctx b y;
        elt ctx c z;
        elt ctx d w
      ] ppf
    | Custom {pull; default; id } -> begin
        match ctx.ext_printer.extension id with
        | Some pr -> pr ppf x
        | None -> elt ctx default (pull ctx.version x) ppf
      end
    | List e -> list ctx.conv (List.map (elt ~inline:false ctx e) x) ppf
    | Sum _ -> destruct x (fun cstrs -> sum ctx 0 cstrs ppf)
    | Record m -> elt_record ctx (field_names m,x) ppf
  and sum ctx pos nested_c ppf =
    let name, V(typ,x) = nested_c.(pos) in
    if pos = Array.length nested_c - 1 then
      begin match typ with
      | Unit ->
          if ctx.conv.inline_constant_constructor then ctx.conv.atom name ppf
          else tuple ~inline:false ctx.conv [ctx.conv.atom name ] ppf
      | _ ->
          tuple ~inline:false ctx.conv
            [ ctx.conv.atom name; elt ~inline:true ctx typ x ]
            ppf
      end
   else
     let next_name, _ = nested_c.(pos+1) in
     if name = next_name then sum ctx (pos+1) nested_c ppf
     else
      let next = item ctx.conv ~key:"next" (sum ctx (pos+1) nested_c) in
      let arg =
        match scrap_custom ctx.version typ x with
        | V(Record r,x) ->
            let fields = next :: fields ctx (field_names r,x) in
            record ctx.conv fields
        | _ -> record ctx.conv [
            next;
            elt_item ctx ~key:"contents" typ x
          ]
      in
       tuple ~inline:false ctx.conv
         [ ctx.conv.atom name; arg] ppf

  and trim_item: type a.
    ctx -> key:string -> optional:bool -> a Diagnostic.typ -> a -> pr option =
    fun ctx ~key ~optional ty x ->
    if not optional then Some (elt_item ctx ~key ty x) else
      match ty, x with
      | List _ , [] -> None
      | Record def, _ ->
          begin match fields ctx (field_names def,x) with
          | [] -> None
          | _ :: _ as fields ->
              Some (item ctx.conv ~key @@ record ctx.conv fields)
          end
      | Custom {pull;default;id}, _ ->
          begin
            match ctx.ext_printer.extension id with
            | Some pr -> Some (fun ppf -> pr ppf x)
            | None -> trim_item ctx ~key ~optional default (pull ctx.version x)
          end
      | _ -> Some (elt_item ctx  ~key ty x)
  and elt_item: type a. ctx -> key:string -> a Diagnostic.typ -> a -> pr =
    fun ctx ~key ty x ppf -> item ctx.conv ~key (elt ctx ty x) ppf
  and fields: type p. ctx -> (string list * p Diagnostic.record)  -> pr list
    = fun ctx (keys,prod) ->
      let fields = Diagnostic.fields keys prod in
      let pp_field (name, optional, V(typ,x)) =
        trim_item ctx ~optional ~key:name typ x in
      List.filter_map pp_field fields
  and elt_record: type p. ctx -> (string list * p Diagnostic.record) -> pr =
    fun ctx x -> record ctx.conv (fields ctx x)

end

  let with_conv ~streaming ~extension conv settings version ppf scheme =
    let ctx = {
      Fmt.version=(Diagnostic_validation.exact_version version);
      conv; ext_printer=extension}
    in
    let record ppf (R(def, r)) =
      let field_names = field_names def in
      let fs = Diagnostic.fields field_names r in
      if List.is_empty fs then () else
        let fields =
          let meta =
            Fmt.fields ctx (["metadata"],r) in
          meta @ Fmt.fields ctx (field_names,r)
        in
        Format.fprintf ppf "%t@." (Pp.record ctx.conv fields)
    in
    let item ppf (name, V(typ,r)) =
      Fmt.elt_item ctx ~key:name typ r ppf
    in
    Log.make ~streaming ~printer:{record;item} settings version scheme ppf

  let structured conv ?color ~version ~device sch =
    with_conv ~streaming:false ~extension:Fmt.no_extension conv
      color version device sch
  let sexp ?color ~version ~device sch =
    structured Pp.sexp ?color ~version ~device sch
  let json ?color ~version ~device sch =
    structured Pp.json ?color ~version ~device sch
  let direct ?color ~version ~device sch =
    with_conv ~streaming:true ~extension:(!Fmt.extensions)
      Pp.direct color version device sch
  let direct_with_fields ?color ~version ~device sch =
    with_conv ~streaming:true ~extension:(!Fmt.extensions)
      Pp.direct_with_fields color version device sch


  type t = {
    name:string;
    make: 'a. ?color:Misc.Color.setting -> version:Diagnostic_validation.version
      -> device:Log.Device.t -> 'a Diagnostic.t -> 'a Log.t;
  }
  let fmt = { name="direct"; make = direct }
  let fmt_with_fields = { name="direct_with_fields"; make = direct_with_fields }
  let add_extension = Fmt.add_extension
  let sexp = { name="sexp" ; make = sexp }
  let json = { name = "json"; make = json }
