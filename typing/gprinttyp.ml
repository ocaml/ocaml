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


(* Print a raw type expression, with sharing *)
open Format

module String_set = Set.Make(String)

module Decoration = struct
  type color =
    | Named of string
    | HSL of {h:float;s:float;l:float}

  let red = Named "red"
  let blue = Named "blue"
  let green = Named "green"
  let purple = Named "purple"
  let lightgrey = Named "lightgrey"
  let hsl ~h ~s ~l = HSL {h;s;l}

  type style =
    | Filled of color option
    | Dotted
    | Dash

  type shape =
    | Ellipse
    | Circle
    | Diamond

  type property =
    | Color of color
    | Font_color of color
    | Style of style
    | Label of string list
    | Shape of shape

  let filled c = Style (Filled (Some c))

  type r = {
    color: color option;
    font_color:color option;
    style: style option;
    label: string list;
    shape: shape option;
  }

  let update r l = match l with
    | Color c -> { r with color = Some c}
    | Style s -> { r with style = Some s}
    | Label s -> { r with label = s}
    | Font_color c -> { r with font_color = Some c}
    | Shape s -> { r with shape = Some s }

  let none = { color=None; font_color=None; style=None; shape=None; label = [] }

  let make l = List.fold_left update none l

  let label r = if r.label = [] then None else Some (Label r.label)
  let color r = Option.map (fun x -> Color x) r.color
  let font_color r = Option.map (fun x -> Font_color x) r.font_color
  let style r = Option.map (fun x -> Style x) r.style
  let shape r = Option.map (fun x -> Shape x) r.shape

  let decompose r =
  let (@?) x l = match x with
    | None -> l
    | Some x -> x :: l
   in
  label r @? color r @? font_color r @? style r @? shape r @? []

  let alt x y = match x with
  | None -> y
  | Some _ -> x

  let merge_label l r =
    let r' = String_set.of_list r in
    let l' = String_set.of_list l in
    List.filter (fun x -> not (String_set.mem x r') ) l
    @ List.filter (fun x -> not (String_set.mem x l') ) r

  let merge l r =
    { color = alt l.color r.color;
      style = alt l.style r.style;
      label = merge_label l.label r.label;
      font_color = alt l.font_color r.font_color;
      shape = alt l.shape r.shape;
    }
  let txt t = Label [t]

end
type decoration = Decoration.r

type dir = Toward | From

let txt = Decoration.txt
let std = Decoration.none
let dotted = Decoration.(make [Style Dotted])
let memo = Decoration.(make [txt "expand"; Style Dash] )


type params = {
  short_ids:bool;
  elide_links:bool;
  expansion_as_hyperedge:bool;
  colorize:bool;
  follow_expansions:bool;
}

let elide_links ty =
  let rec follow_safe visited t =
    let t = Types.Transient_expr.coerce t in
    if List.memq t visited then t
    else match t.Types.desc with
      | Tlink t' -> follow_safe (t::visited) t'
      | _ -> t
  in
  follow_safe [] ty

let repr params ty =
  if params.elide_links then elide_links ty
  else Types.Transient_expr.coerce ty

module Index: sig
  type t = private
    | Main of int
    | Synthetic of int
    | Named_subnode of { id:int; synth:bool; name:string }
  type level_and_scope = { level:int; scope: int }
  type 'a desc = {
    id: 'a;
    color: Decoration.color option;
    desc: Types.type_desc;
    lvl:level_and_scope;
  }
  val subnode: name:string -> t -> t
  val either_ext: Types.row_field_cell ->  t
  val split: params -> Types.type_expr -> t desc
  val colorize: params -> t -> Decoration.color option
end = struct
  type t =
    | Main of int
    | Synthetic of int
    | Named_subnode of { id:int; synth:bool; name:string }
  type level_and_scope = { level:int; scope: int }
  type 'a desc = {
    id: 'a;
    color: Decoration.color option;
    desc: Types.type_desc;
    lvl:level_and_scope;
  }

  type name_map = {
    (* We keep the main and synthetic and index space separate to avoid index
       collision when we use the typechecker provided [id]s as main indices *)
    main_last: int ref;
    synthetic_last: int ref;
    either_cell_ids: (Types.row_field_cell * int) list ref;
    tbl: (int,int) Hashtbl.t;
  }

  let id_map = {
    main_last = ref 0;
    synthetic_last = ref 0;
    either_cell_ids = ref [];
    tbl = Hashtbl.create 20;
  }

  let fresh_main_id () =
    incr id_map.main_last;
    !(id_map.main_last)

  let fresh_synthetic_id () =
    incr id_map.synthetic_last;
    !(id_map.synthetic_last)

  let stable_id = function
    | Main id | Synthetic id | Named_subnode {id;_} -> id

  let pretty_id params id =
    if not params.short_ids then Main id else
      match Hashtbl.find_opt id_map.tbl id with
      | Some x -> Main x
      | None ->
          let last = fresh_main_id () in
          Hashtbl.replace id_map.tbl id last;
          Main last

  (** Generate color from the node id to keep the color stable inbetween
      different calls to the typechecker on the same input. *)
  let colorize_id params id =
    if not params.colorize then None
    else
      (* Generate pseudo-random color by cycling over 200 hues while keeping
         pastel level of saturation and lightness *)
      let nhues = 200 in
      (* 17 and 200 are relatively prime, thus 17 is of order 200 in Z/200Z. A
         step size around 20 makes it relatively easy to spot different hues. *)
      let h = float_of_int (17 * id mod nhues) /. float_of_int nhues in
      (* Add a modulation of period 3 and 7 to the saturation and lightness *)
      let s = match id mod 3 with
        | 0 -> 0.3
        | 1 -> 0.5
        | 2 | _ -> 0.7
      in
      let l = match id mod 7 with
        | 0 -> 0.5
        | 1 -> 0.55
        | 2 -> 0.60
        | 3 -> 0.65
        | 4 -> 0.70
        | 5 -> 0.75
        | 6 | _ -> 0.8
      in
      (* With 3, 7 and 200 relatively prime, we cycle over the full parameter
         space with 4200 different colors. *)
      Some (Decoration.hsl ~h ~s ~l)

  let colorize params index = colorize_id params (stable_id index)

  let split params x =
    let x = repr params x in
    let color = colorize_id params x.id in
    let scope = Types.Transient_expr.get_scope x in
    let level = x.level in
    { id = pretty_id params x.id;
      color;
      desc = x.desc;
      lvl = {level;scope}
    }

  let subnode ~name x = match x with
    | Main id -> Named_subnode {id;name;synth=false}
    | Named_subnode r -> Named_subnode {r with name}
    | Synthetic id -> Named_subnode {id;name;synth=true}

  let either_ext r =
    let either_ids = !(id_map.either_cell_ids) in
    match List.assq_opt r either_ids with
    | Some n -> Synthetic n
    | None ->
        let n = fresh_synthetic_id () in
        id_map.either_cell_ids := (r,n) :: either_ids;
        Synthetic n

end


type index = Index.t
module Node_set = Set.Make(struct
    type t = Index.t
    let compare = Stdlib.compare
end)

module Edge_set = Set.Make(struct
    type t = Index.t * Index.t
    let compare = Stdlib.compare
end)

module Hyperedge_set = Set.Make(struct
    type t = (dir * Decoration.r * index) list
    let compare = Stdlib.compare
end)

type subgraph =
  {
    nodes: Node_set.t;
    edges: Edge_set.t;
    hyperedges: Hyperedge_set.t;
    subgraphes: (Decoration.r * subgraph) list;
  }


let empty_subgraph=
  { nodes = Node_set.empty;
    edges=Edge_set.empty;
    hyperedges = Hyperedge_set.empty;
    subgraphes = [];
  }


type 'index elt =
  | Node of 'index
  | Edge of 'index * 'index
  | Hyperedge of (dir * Decoration.r * 'index) list
type element = Types.type_expr elt


module Elt_map = Map.Make(struct
    type t = Index.t elt
    let compare = Stdlib.compare
  end)
let (.%()) map e =
  Option.value ~default:Decoration.none @@
  Elt_map.find_opt e map

type digraph = {
  elts: Decoration.r Elt_map.t;
  graph: subgraph
}

module Pp = struct

  let semi ppf () = fprintf ppf ";@ "
  let space ppf () = fprintf ppf "@ "
  let empty ppf () = fprintf ppf ""
  let string =pp_print_string
  let list ~sep = pp_print_list ~pp_sep:sep
  let seq ~sep = pp_print_seq ~pp_sep:sep
  let rec longident ppf = function
    | Longident.Lident s -> fprintf ppf "%s" s
    | Longident.Ldot (l,s) -> fprintf ppf "%a.%s"  longident l s
    | Longident.Lapply(f,x) -> fprintf ppf "%a(%a)" longident f  longident x

  let color ppf = function
    | Decoration.Named s -> fprintf ppf "%s" s
    | Decoration.HSL r -> fprintf ppf "%1.3f %1.3f %1.3f" r.h r.s r.l

  let style ppf = function
    | Decoration.Filled _ -> fprintf ppf "filled"
    | Decoration.Dash -> fprintf ppf "dashed"
    | Decoration.Dotted -> fprintf ppf "dotted"

  let shape ppf = function
    | Decoration.Circle -> fprintf ppf "circle"
    | Decoration.Diamond -> fprintf ppf "diamond"
    | Decoration.Ellipse -> fprintf ppf "ellipse"

  let property ppf = function
    | Decoration.Color c -> fprintf ppf {|color="%a"|} color c
    | Decoration.Font_color c -> fprintf ppf {|fontcolor="%a"|} color c
    | Decoration.Style s ->
        fprintf ppf {|style="%a"|} style s;
        begin match s with
        | Filled (Some c) -> fprintf ppf {|;@ fillcolor="%a"|} color c;
        | _ -> ()
        end;
    | Decoration.Shape s -> fprintf ppf {|shape="%a"|} shape s
    | Decoration.Label s ->
        fprintf ppf {|label=<%a>|} (list ~sep:space string) s

  let inline_decoration ppf r =
    match Decoration.decompose r with
    | [] -> ()
    | l -> fprintf ppf "@[<v>%a@]" (list ~sep:semi property) l

  let decoration ppf r =
    match Decoration.decompose r with
    | [] -> ()
    | l -> fprintf ppf "[@[<h>%a@]]" (list ~sep:semi property) l

  let row_fixed ppf = function
    | None -> fprintf ppf ""
    | Some Types.Fixed_private -> fprintf ppf "private"
    | Some Types.Rigid -> fprintf ppf "rigid"
    | Some Types.Univar _t -> fprintf ppf "univar"
    | Some Types.Reified _p -> fprintf ppf "reified"

  let field_kind ppf v =
    match Types.field_kind_repr v with
    | Fpublic -> fprintf ppf "public"
    | Fabsent -> fprintf ppf "absent"
    | Fprivate -> fprintf ppf "private"

  let index ppf = function
    | Index.Main id -> fprintf ppf "i%d" id
    | Index.Synthetic id -> fprintf ppf "s%d" id
    | Index.Named_subnode r ->
        fprintf ppf "%s%dRF%s" (if r.synth then "s" else "i") r.id r.name

  let prettier_index ppf = function
    | Index.Main id -> fprintf ppf "%d" id
    | Index.Synthetic id -> fprintf ppf "[%d]" id
    | Index.Named_subnode r -> fprintf ppf "%d(%s)" r.id r.name

  let hyperedge_id ppf l =
    let sep ppf () = fprintf ppf "h" in
    let elt ppf (_,_,x) = index ppf x in
    fprintf ppf "h%a" (list ~sep elt) l

  let node graph ppf x =
    let d = graph.%(Node x) in
    fprintf ppf "%a%a;@ " index x decoration d

  let edge graph ppf (x,y) =
    let d = graph.%(Edge (x,y)) in
    fprintf ppf "%a->%a%a;@ " index x index y decoration d

  let hyperedge graph ppf l =
    let d = graph.%(Hyperedge l) in
    fprintf ppf "%a%a;@ " hyperedge_id l decoration d;
    List.iter (fun (dir,d,x) ->
        match dir with
        | From ->
            fprintf ppf "%a->%a%a;@ " index x hyperedge_id l decoration d
        | Toward ->
            fprintf ppf "%a->%a%a;@ " hyperedge_id l index x decoration d
      ) l

  let cluster_counter = ref 0
  let pp_cluster ppf =
    incr cluster_counter;
    fprintf ppf "cluster_%d" !cluster_counter

  let exponent_of_label ppf = function
    | Asttypes.Nolabel -> ()
    | Asttypes.Labelled s -> fprintf ppf "<SUP>%s</SUP>" s
    | Asttypes.Optional s -> fprintf ppf "<SUP>?%s</SUP>" s

  let pretty_var ppf name =
    let name = Option.value ~default:"_" name in
    let name' =
      match name with
      | "a" -> "𝛼"
      | "b" -> "𝛽"
      | "c" -> "𝛾"
      | "d" -> "𝛿"
      | "e" -> "𝜀"
      | "f" -> "𝜑"
      | "t" -> "𝜏"
      | "r" -> "𝜌"
      | "s" -> "𝜎"
      | "p" -> "𝜋"
      | "i" -> "𝜄"
      | "h" -> "𝜂"
      | "k" -> "𝜅"
      | "l" -> "𝜆"
      | "m" -> "𝜇"
      | "x" -> "𝜒"
      | "n" -> "𝜐"
      | "o" -> "𝜔"
      | name -> name
    in
    if name = name' then
      fprintf ppf "'%s" name
    else pp_print_string ppf name'

  let rec subgraph elts ppf (d,sg) =
    fprintf ppf
      "@[<v 2>subgraph %t {@,\
       %a;@ \
       %a%a%a%a}@]@."
      pp_cluster
      inline_decoration d
      (seq ~sep:empty (node elts)) (Node_set.to_seq sg.nodes)
      (seq ~sep:empty (edge elts)) (Edge_set.to_seq sg.edges)
      (seq ~sep:empty (hyperedge elts)) (Hyperedge_set.to_seq sg.hyperedges)
      (list ~sep:empty (subgraph elts)) sg.subgraphes

  let graph ppf {elts;graph} =
    fprintf ppf "@[<v 2>digraph {@,%a%a%a%a}@]@."
    (seq ~sep:empty (node elts)) (Node_set.to_seq graph.nodes)
    (seq ~sep:empty (edge elts)) (Edge_set.to_seq graph.edges)
    (seq ~sep:empty (hyperedge elts)) (Hyperedge_set.to_seq graph.hyperedges)
    (list ~sep:empty (subgraph elts)) graph.subgraphes

end


module Digraph = struct

  type t = digraph = {
    elts: Decoration.r Elt_map.t;
    graph: subgraph
  }

  let empty = { elts = Elt_map.empty; graph = empty_subgraph }

  let add_to_subgraph s = function
    | Node ty ->
        let nodes = Node_set.add ty s.nodes in
        { s with nodes }
    | Edge (x,y) ->
        let edges = Edge_set.add (x,y) s.edges in
        { s with edges }
    | Hyperedge l ->
        let hyperedges = Hyperedge_set.add l s.hyperedges in
        { s with hyperedges }

  let add_subgraph sub g =
    { g with subgraphes = sub :: g.subgraphes }

  let add ?(override=false) d entry dg =
    match Elt_map.find_opt entry dg.elts with
    | Some d' ->
        let d =
          if override then Decoration.merge d d'
          else Decoration.merge d' d
        in
        { dg with elts = Elt_map.add entry d dg.elts }
    | None ->
        let elts = Elt_map.add entry d dg.elts in
        { elts; graph = add_to_subgraph dg.graph entry }

  let rec hyperedges_of_memo ty params id abbrev dg =
    match abbrev with
    | Types.Mnil -> dg
    | Types.Mcons (_priv, _p, t1, t2, rem) ->
        let s, dg = ty params t1 dg in
        let exp, dg = ty params t2 dg in
        dg |>
        add memo
          (Hyperedge
             [From, dotted, id;
              Toward, dotted, s;
              Toward, Decoration.make [txt "expand"], exp
             ])
        |> hyperedges_of_memo ty params id rem
    | Types.Mlink rem -> hyperedges_of_memo ty params id !rem dg

  let rec edges_of_memo ty params abbrev dg =
    match abbrev with
    | Types.Mnil -> dg
    | Types.Mcons (_priv, _p, t1, t2, rem) ->
        let x, dg = ty params t1 dg in
        let y, dg = ty params t2 dg in
        dg |> add memo (Edge (x,y)) |> edges_of_memo ty params rem
    | Types.Mlink rem -> edges_of_memo ty params !rem dg

  let expansions ty params id memo dg =
    if params.expansion_as_hyperedge then
      hyperedges_of_memo ty params id memo dg
    else
      edges_of_memo ty params memo dg

  let labelk k fmt = kasprintf (fun s -> k  [txt s]) fmt
  let labelf fmt = labelk Fun.id fmt
  let labelr fmt = labelk Decoration.make fmt

  (* Use unicode superscript digit to circumvent graphviz limited support for
     superscript. *)
  let superscript_digit ppf n =
    let s = match n with
    | 1 -> "¹"
    | 2 -> "²"
    | 3 -> "³"
    | 0 -> "⁰"
    | 4 -> "⁴"
    | 5 -> "⁵"
    | 6 -> "⁶"
    | 7 -> "⁷"
    | 8 -> "⁸"
    | 9 -> "⁹"
    | _ -> assert false
    in
    Format.pp_print_string ppf s

  let rec superscript ppf n =
    if n < 10 then
      superscript_digit ppf n
    else begin
      superscript ppf (n/10);
      superscript_digit ppf (n mod 10)
    end

  let superscript_level ppf lvl =
    (* avoid a dependency on Btype *)
    if lvl = Ident.highest_scope then Format.pp_print_string ppf "᪲"
    else superscript ppf lvl

  let add_node explicit_d color id ?lvl tynode dg =
    let d = match lvl with
      | None -> labelf "<SUB>%a</SUB>" Pp.prettier_index id
      | Some {Index.level; scope=0} ->
          labelf "<SUB>%a %a</SUB>"
            Pp.prettier_index id superscript_level level
      | Some {Index.level; scope} ->
          labelf "<SUB>%a %a⁺%a</SUB>"
            Pp.prettier_index id
            superscript_level level
            superscript scope
    in
    let d = match color with
    | None -> Decoration.make d
    | Some x -> Decoration.(make (filled x :: d))
    in
    let d = Decoration.merge explicit_d d in
    add d tynode dg

  let field_node color lbl rf =
    let col = match color with
      | None -> []
      | Some c -> [Decoration.Color c]
    in
    let pr_lbl ppf = match lbl with
      | None -> ()
      | Some lbl -> fprintf ppf "`%s" lbl
    in
    let lbl =
      Types.match_row_field
        ~absent:(fun _ -> labelf "`-%t" pr_lbl)
        ~present:(fun _ -> labelf "&gt;%t" pr_lbl)
        ~either:(fun c _tl m _e ->
            labelf "%s%t%s"
              (if m then "?" else "")
              pr_lbl
              (if c then "(∅)" else "")
          )
        rf
    in
    Decoration.(make (Shape Diamond::col@lbl))

  let group ty id0 lbl l dg =
    match l with
    | [] -> dg
    | first :: l ->
      let sub = { dg with graph = empty_subgraph } in
      let id, sub = ty first sub in
      let sub = List.fold_left (fun dg t -> snd (ty t dg)) sub l in
      let dg = { sub with graph = add_subgraph (lbl,sub.graph) dg.graph } in
      dg |> add std (Edge(id0,id))

  let split_fresh_typ params ty0 g =
    let {Index.id; _ } as desc = Index.split params ty0 in
    let tynode = Node id in
    if Elt_map.mem tynode g then id, None
    else id, Some { desc with id = tynode }

  let pp_path = Format_doc.compat Path.print

  let rec inject_typ params ty0 dg =
    let id, next = split_fresh_typ params ty0 dg.elts in
    match next with
    | None -> id, dg
    | Some Index.{id=tynode; color; desc; lvl} ->
        id, node params color ~lvl id tynode desc dg
  and edge params id0 lbl ty gh =
    let id, gh = inject_typ params ty gh in
    add lbl (Edge(id0,id)) gh
  and poly_edge ~color params id0 gh ty =
    let id, gh = inject_typ params ty gh in
    match color with
    | None -> add (labelr "bind") (Edge (id0,id)) gh
    | Some c ->
        let d = Decoration.(make [txt "bind"; Color c]) in
        let gh = add d (Edge (id0,id)) gh in
        add ~override:true Decoration.(make [filled c]) (Node id) gh
  and numbered_edge params id0 (i,gh) ty =
    let l = labelr "%d" i in
    i + 1, edge params id0 l ty gh
  and numbered_edges params id0 l gh =
    snd @@ List.fold_left
      (numbered_edge params id0)
      (0,gh) l
  and node params color ~lvl id tynode desc dg =
    let add_tynode l = add_node l color ~lvl id tynode dg in
    let mk fmt = labelk (fun l -> add_tynode (Decoration.make l)) fmt in
    let numbered = numbered_edges params id in
    let edge = edge params id in
    let std_edge = edge std in
    match desc with
    | Types.Tvar name -> mk "%a" Pp.pretty_var name
    | Types.Tarrow(l,t1,t2,_) ->
       mk "→%a" Pp.exponent_of_label l |> numbered [t1; t2]
    | Types.Ttuple tl ->
        mk "*" |> numbered tl
    | Types.Tconstr (p,tl,abbrevs) ->
        let constr = mk "%a" pp_path p |> numbered tl in
        if not params.follow_expansions then
          constr
        else
          expansions inject_typ params id !abbrevs constr
    | Types.Tobject (t, name) ->
        let dg =
          begin match !name with
          | None -> mk "[obj]"
          | Some (p,[]) -> (* invalid format *)
              mk "[obj(%a)]" pp_path p
          | Some (p, (rv_or_nil :: tl)) ->
              match Types.get_desc rv_or_nil with
              | Tnil ->
                  mk "[obj(%a)]" pp_path p |> std_edge t |> numbered tl
              | _ ->
                  mk "[obj(#%a)]" pp_path p
                  |> edge (labelr "row variable") rv_or_nil
                  |> numbered tl
          end
        in
        begin match split_fresh_typ params t dg.elts with
        | _, None -> dg
        | next_id, Some {Index.color; desc; lvl; _ } ->
            group_fields ~params ~prev_id:id ~lvl
              dg.elts dg.graph empty_subgraph
              ~id:next_id ~color ~desc
        end
    | Types.Tfield _ ->
        group_fields ~params ~prev_id:id ~lvl
          dg.elts dg.graph empty_subgraph
          ~color ~id ~desc
    | Types.Tnil -> mk "[Nil]"
    | Types.Tlink t -> add_tynode Decoration.(make [Style Dash]) |> std_edge t
    | Types.Tsubst (t, o) ->
        let dg = add_tynode (labelr "[Subst]") |> std_edge t in
        begin match o with
        | None -> dg
        | Some row -> edge (labelr "parent polyvar") row dg
        end
    | Types.Tunivar name ->
        mk "%a<SUP>∀</SUP>" Pp.pretty_var name
    | Types.Tpoly (t, tl) ->
        let dg = mk "∀" |> std_edge t in
        List.fold_left (poly_edge ~color params id) dg tl
    | Types.Tvariant row ->
        let Row {fields; more; name; fixed; closed} = Types.row_repr row in
        let closed = if closed then "<SUP>closed</SUP>" else "" in
        let dg = match name with
          | None -> mk "[Row%s]" closed
          | Some (p,tl) ->
              mk "[Row %a%s]" pp_path p closed
              |> numbered tl
        in
        let more_lbl = labelr "%a row variable" Pp.row_fixed fixed in
        let dg = dg |> edge more_lbl more in
        let elts, main, fields =
          List.fold_left (variant params id)
            (dg.elts, dg.graph, empty_subgraph)
            fields
        in
        { elts; graph = add_subgraph (labelr "polyvar", fields) main }
    | Types.Tpackage (p, fl) ->
        let types = List.map snd fl in
        mk "[mod %a with %a]"
          pp_path p
          Pp.(list ~sep:semi longident) (List.map fst fl)
        |> numbered types
  and variant params id0 (elts,main,fields) (name,rf)  =
    let id = Index.subnode ~name id0 in
    let fnode = Node id in
    let color = Index.colorize params id in
    let fgraph = { elts; graph=fields } in
    let fgraph = add (field_node color (Some name) rf) fnode fgraph  in
    let { elts; graph=fields} = add dotted (Edge(id0,id)) fgraph in
    let mgraph = { elts; graph=main } in
    let {elts; graph=main} =
      variant_inside params id rf mgraph
    in
    elts, main, fields
  and variant_inside params id rf dg =
    Types.match_row_field
      ~absent:(fun () -> dg)
      ~present:(function
          | None -> dg
          | Some arg -> numbered_edges params id [arg] dg
        )
      ~either:(fun _ tl _ (cell,e) ->
          let dg = match tl with
            | [] -> dg
            | [x] -> edge params id std x dg
            | _ :: _ as tls ->
                let label = Decoration.(make [txt "⋀"; filled lightgrey]) in
                group (inject_typ params) id label tls dg
          in
          match e with
          | None -> dg
          | Some f ->
              let id_ext = Index.either_ext cell in
              let color = Index.colorize params id_ext in
              let dg = add (field_node color None f) (Node id_ext) dg in
              let dg = add std (Edge(id,id_ext)) dg in
              variant_inside params id_ext f dg
        )
      rf
  and group_fields ~params ~prev_id elts main fields
      ~color ~lvl ~id ~desc =
    let add_tynode dg l = add_node l color id (Node id) dg in
    let mk dg fmt = labelk (fun l -> add_tynode dg (Decoration.make l)) fmt in
    let merge elts ~main ~fields =
      {elts; graph= add_subgraph (labelr "fields", fields) main }
    in
    match desc with
    | Types.Tfield (f, k,typ, next) ->
        let fgraph = { elts; graph=fields } in
        let fgraph = mk fgraph "%s<SUP>%a</SUP>" f Pp.field_kind k in
        let {elts; graph=fields} = add dotted (Edge (prev_id,id)) fgraph in
        let {elts; graph=main} =
          edge params id (labelr "method type") typ
            {elts; graph= main}
        in
        let id_next, next = split_fresh_typ params next elts in
        begin match next with
        | None -> {elts; graph=main}
        | Some {Index.color; desc; lvl; _} ->
            group_fields ~params ~prev_id:id ~lvl
              elts main fields
              ~id:id_next ~desc ~color
        end
    | Types.Tvar name ->
        let dg  = mk {elts; graph= fields } "%a" Pp.pretty_var name in
        let {elts; graph=fields} =
          add (labelr "row variable") (Edge(prev_id,id)) dg
        in
        merge elts ~main ~fields
    | Types.Tnil -> merge elts ~main ~fields
    | _ ->
        let dg = merge elts ~main ~fields in
        node params color ~lvl id (Node id) desc dg
end

let params
    ?(elide_links=true)
    ?(expansion_as_hyperedge=false)
    ?(short_ids=true)
    ?(colorize=true)
    ?(follow_expansions=true)
    () =
  {
    expansion_as_hyperedge;
    short_ids;
    elide_links;
    colorize;
    follow_expansions;
  }

let update_params ?elide_links
    ?expansion_as_hyperedge
    ?short_ids
    ?colorize
    ?follow_expansions
    params =
  {
    elide_links = Option.value ~default:params.elide_links elide_links;
    expansion_as_hyperedge =
      Option.value ~default:params.expansion_as_hyperedge
        expansion_as_hyperedge;
    short_ids = Option.value ~default:params.short_ids short_ids;
    colorize = Option.value ~default:params.colorize colorize;
    follow_expansions =
      Option.value ~default:params.follow_expansions follow_expansions;
  }


let translate params dg (label,entry) =
  let node, dg = match entry with
    | Node ty ->
        let id, dg = Digraph.inject_typ params ty dg in
        Node id, dg
    | Edge (ty,ty') ->
        let id, dg = Digraph.inject_typ params ty dg in
        let id', dg = Digraph.inject_typ params ty' dg in
        Edge(id,id'), dg
    | Hyperedge l ->
        let l, dg = List.fold_left (fun (l,dg) (d,lbl,ty) ->
            let id, dg = Digraph.inject_typ params ty dg in
            (d,lbl,id)::l, dg
          ) ([],dg) l
        in
       Hyperedge l, dg
  in
  Digraph.add ~override:true label node dg

let add params ts dg =
  List.fold_left (translate params) dg ts


let make params ts =
  add params ts Digraph.empty
let pp = Pp.graph

let add_subgraph params d elts dg =
  let sub = add params elts { dg with graph = empty_subgraph } in
  { sub with graph = Digraph.add_subgraph (d,sub.graph) dg.graph }

let group_nodes (decoration, {graph=sub; elts=_}) ({elts;graph=main} as gmain) =
  let nodes = Node_set.inter sub.nodes main.nodes in
  if Node_set.cardinal nodes > 1 then
  let sub = { empty_subgraph with nodes } in
  let graph =
    { main with
      nodes = Node_set.diff main.nodes sub.nodes;
      subgraphes = (decoration,sub) :: main.subgraphes
    }
  in { graph; elts}
  else gmain

let file_counter = ref 0

let compact_loc ppf (loc:Warnings.loc) =
  let startline = loc.loc_start.pos_lnum in
  let endline = loc.loc_end.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_end.pos_bol in
  if startline = endline then
    fprintf ppf "l%d[%d-%d]" startline startchar endchar
  else
    fprintf ppf "l%d-%d[%d-%d]" startline endline startchar endchar

type 'a context = 'a option ref * (Format.formatter -> 'a -> unit)

let set_context (r,_pr) x = r := Some x
let pp_context (r,pr) ppf = match !r with
  | None -> ()
  | Some x -> fprintf ppf "%a" pr x

let with_context (r,_) x f =
  let old = !r in
  r:= Some x;
  Fun.protect f ~finally:(fun () -> r := old)

let global = ref None, pp_print_string
let loc = ref None, compact_loc
let context = [pp_context global; pp_context loc]
let dash ppf () = fprintf ppf "-"

let node_register = ref []
let register_type (label,ty) =
  node_register := (label,Node ty) :: !node_register

let subgraph_register = ref []
let default_style = Decoration.(make [filled lightgrey])
let register_subgraph params ?(decoration=default_style) tys =
  let node x = Decoration.none, Node x in
  let subgraph = make params (List.map node tys) in
  subgraph_register := (decoration, subgraph) :: !subgraph_register

let forget () =
  node_register := [];
  subgraph_register := []

let node x = Node x
let edge x y = Edge(x,y)
let hyperedge l = Hyperedge l

let nodes ~title params ts =
  incr file_counter;
  let filename =
    match !Clflags.dump_dir with
    | None -> asprintf "%04d-%s.dot"  !file_counter title
    | Some d ->
        asprintf "%s%s%04d-%s-%a.dot"
          d Filename.dir_sep
          !file_counter
          title
          Pp.(list ~sep:dash (fun ppf pr -> pr ppf)) context
  in
  Out_channel.with_open_bin filename (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let ts = List.map (fun (l,t) -> l, t) ts in
      let g = make params (ts @ !node_register) in
      let g =
        List.fold_left (fun g sub -> group_nodes sub g) g !subgraph_register
      in
      Pp.graph ppf g
    )

let types ~title params ts =
  nodes ~title params (List.map (fun (lbl,ty) -> lbl, Node ty) ts)

let make params elts = make params elts
let add params elts = add params elts


(** Debugging hooks *)
let debug_on = ref (fun () -> false)
let debug f = if !debug_on () then f ()

let debug_off f =
  let old = !debug_on in
  debug_on := Fun.const false;
  Fun.protect f
    ~finally:(fun () -> debug_on := old)
