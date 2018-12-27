(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*             Mark Shinwell and Leo White, Jane Street Europe            *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

module Option = Misc.Stdlib.Option

module Code_range = struct
  type t = {
    file : string;
    line : int;
    char_start : int;
    char_end : int;
  }

  let create ~file ~line ~char_start ~char_end =
    if line < 0 || char_start < 0 then begin
      Misc.fatal_errorf "Bad line (%d) or starting char (%d) for \
          [Debuginfo.Code_range.create]"
        line
        char_start
    end;
    { file;
      line;
      char_start;
      char_end;
    }

  let none = create ~file:"" ~line:0 ~char_start:0 ~char_end:0

  let file t = t.file
  let line t = t.line
  let char_start t = t.char_start
  let char_end t = t.char_end

  let to_string t =
    Printf.sprintf "%s:%d,%d-%d" t.file t.line t.char_start t.char_end

  let of_line ~file ~line =
    (* 80 seems like a reasonable default to cover a line. *)
    create ~file ~line ~char_start:0 ~char_end:80

  let of_location (loc : Location.t) =
    if Location.is_none loc then
      none
    else
      { file = loc.loc_start.pos_fname;
        line = loc.loc_start.pos_lnum;
        char_start = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
        char_end =
          if String.equal loc.loc_end.pos_fname loc.loc_start.pos_fname
          then loc.loc_end.pos_cnum - loc.loc_start.pos_bol
          else loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
      }

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf ({ file; line; char_start; char_end; } as t) =
      if t == none then
        Format.pp_print_string ppf "<none>"
      else
        Format.fprintf ppf "@[<hov 1>(\
            @[<hov 1>(file@ %s)@]@ \
            @[<hov 1>(line@ %d)@]@ \
            @[<hov 1>(char_start@ %d)@]@ \
            @[<hov 1>(char_end@ %d)@])@]"
          file
          line
          char_start
          char_end

    let output chan t =
      Format.fprintf (Format.formatter_of_out_channel chan) "%a%!" print t

    let compare t1 t2 =
      let c = String.compare t1.file t2.file in
      if c <> 0 then c
      else
        let c = compare t1.line t2.line in
        if c <> 0 then c
        else
          let c = compare t1.char_end t2.char_end in
          if c <> 0 then c
          else compare t1.char_start t2.char_start

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t = Hashtbl.hash t
  end)

  let print_compact ppf t =
    Format.fprintf ppf "%a:%i"
      Location.print_filename t.file
      t.line;
    if t.char_start >= 0 then begin
      Format.fprintf ppf ",%i--%i" t.char_start t.char_end
    end

  let to_location t : Location.t =
    let loc_start : Lexing.position =
      { pos_fname = t.file;
        pos_lnum = t.line;
        pos_bol = 0;
        pos_cnum = t.char_start;
      }
    in
    let loc_end = { loc_start with pos_cnum = t.char_end; } in
    { loc_ghost = false; loc_start; loc_end; }

  module Option = struct
    type nonrec t = t option

    include Identifiable.Make (struct
      type nonrec t = t

      let compare t1 t2 =
        match t1, t2 with
        | None, None -> 0
        | None, Some _ -> -1
        | Some _, None -> 1
        | Some range1, Some range2 -> compare range1 range2

      let equal t1 t2 = (compare t1 t2 = 0)

      let hash t =
        match t with
        | None -> 0
        | Some range -> Hashtbl.hash (1, hash range)

      let print ppf t = Misc.Stdlib.Option.print print ppf t

      let output chan t =
        Format.fprintf (Format.formatter_of_out_channel chan) "%a%!" print t
    end)
  end
end

module Id = struct
  type t = {
    stamp : int;
    compilation_unit : Compilation_unit.t;
  }

  let next_stamp = ref 0

  let get_next_stamp () =
    let stamp = !next_stamp in
    incr next_stamp;
    stamp

  let create () =
    { stamp = get_next_stamp ();
      compilation_unit = Compilation_unit.get_current_exn ();
    }

  let compilation_unit t = t.compilation_unit

  let to_string_for_dwarf_die_name t =
    Format.sprintf "%s_%d"
      (Compilation_unit.string_for_printing t.compilation_unit)
      t.stamp

  include Identifiable.Make (struct
    type nonrec t = t

    let compare { stamp = stamp1; compilation_unit = compilation_unit1; }
          { stamp = stamp2; compilation_unit = compilation_unit2; } =
      let c = Stdlib.compare stamp1 stamp2 in
      if c <> 0 then c
      else Compilation_unit.compare compilation_unit1 compilation_unit2

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash { stamp; compilation_unit; } =
      Hashtbl.hash (stamp, Compilation_unit.hash compilation_unit)

    let print ppf { stamp; compilation_unit; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(stamp@ %d)@]@ \
        @[<hov 1>(compilation_unit@ %a)@])@]"
      stamp
      Compilation_unit.print compilation_unit

    let output chan t =
      Format.fprintf (Format.formatter_of_out_channel chan) "%a%!" print t
  end)
end

module Function = struct
  module Id = Id

  type t = {
    id : Id.t;
    position : Code_range.t;
    human_name : string;
    module_path : Path.t option;
    dwarf_die_present : bool;
  }

  let create position ~human_name ~module_path =
    let dwarf_die_present =
      match !Clflags.debug_full with
      | None -> false
      | Some _ -> true
    in
    { id = Id.create ();
      position;
      human_name;
      module_path;
      dwarf_die_present;
    }

  let create_from_location loc ~human_name ~module_path =
    create (Code_range.of_location loc) ~human_name ~module_path

  let create_from_line ~file ~line ~human_name ~module_path =
    create (Code_range.of_line ~file ~line) ~human_name ~module_path

  let with_position t position =
    { t with position; }

  let id t = t.id
  let position t = t.position
  let human_name t = t.human_name
  let module_path t = t.module_path
  let dwarf_die_present t = t.dwarf_die_present

  let name t =
    match t.module_path with
    | None ->
      begin match t.human_name with
      | "" -> "<anon>"
      | name -> name
      end
    | Some path ->
      let path = Printtyp.string_of_path path in
      match path with
      | "_Ocaml_startup" ->
        begin match t.human_name with
        | "" -> "<anon>"
        | name -> name
        end
      | _ ->
        match t.human_name with
        | "" -> path
        | name -> path ^ "." ^ name

  let is_visible_externally t =
    (* Not strictly accurate---should probably depend on the .mli, but
       this should suffice for now. *)
    match t.module_path with
    | None -> false
    | Some _ -> true

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 = Id.compare t1.id t2.id
    let equal t1 t2 = Id.equal t1.id t2.id
    let hash t = Id.hash t.id

    let print ppf { id; position; human_name; module_path;
                    dwarf_die_present; } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(id@ %a)@]@ \
          @[<hov 1>(position@ %a)@]@ \
          @[<hov 1>(human_name@ %s)@]@ \
          @[<hov 1>(module_path@ %a)@]@ \
          @[<hov 1>(dwarf_die_present@ %b)@])@]"
        Id.print id
        Code_range.print position
        human_name
        (Option.print Path.print) module_path
        dwarf_die_present

    let output chan t =
      Format.fprintf (Format.formatter_of_out_channel chan) "%a%!" print t
  end)

  let compare_on_source_position_only { position = position1; _ }
        { position = position2; _ } =
    Code_range.compare position1 position2
end

module Call_site = struct
  type t = {
    fun_dbg : Function.t;
    position : Code_range.t;
  }

  let create_from_location fun_dbg loc =
    { fun_dbg;
      position = Code_range.of_location loc;
    }

  let fun_dbg t = t.fun_dbg
  let position t = t.position

  let to_string t = Code_range.to_string t.position

  let print ppf { fun_dbg; position; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(fun_dbg@ %a)@]@ \
        @[<hov 1>(position@ %a)@])@]"
      Function.print fun_dbg
      Code_range.print position
end

module Block = struct
  module Id = Id

  type t = {
    id : Id.t;
    frame_location : Call_site.t option;
    (* CR-someday mshinwell: We could perhaps have a link to the parent
       _frame_, if such exists. *)
    parent : t option;
    parents_transitive : t list;
  }

  let parents_transitive_from_parent ~parent =
    match parent with
    | None -> []
    | Some parent -> parent :: parent.parents_transitive

  let create_lexical_scope ~parent =
    let parents_transitive = parents_transitive_from_parent ~parent in
    { id = Id.create ();
      frame_location = None;
      parent;
      parents_transitive;
    }

  let create_non_inlined_frame call_site =
    { id = Id.create ();
      frame_location = Some call_site;
      parent = None;
      parents_transitive = [];
    }

  let create_inlined_frame call_site ~parent =
    let parents_transitive = parents_transitive_from_parent ~parent in
    { id = Id.create ();
      frame_location = Some call_site;
      parent;
      parents_transitive;
    }

  let create_and_reparent ~like:t ~new_parent =
    let parent = new_parent in
    let parents_transitive = parents_transitive_from_parent ~parent in
    { t with
      id = Id.create ();
      parent;
      parents_transitive;
    }

  let parent t = t.parent
  let unique_id t = t.id

  let rec frame_list_innermost_first t =
    match t.parent with
    | None ->
      begin match t.frame_location with
      | None -> []
      | Some range -> [range]
      end
    | Some parent ->
      begin match t.frame_location with
      | None -> frame_list_innermost_first parent
      | Some range -> range::(frame_list_innermost_first parent)
      end

  type frame_classification =
    | Lexical_scope_only
    | Inlined_frame of Call_site.t

  let frame_classification t =
    match t.frame_location with
    | None -> Lexical_scope_only
    | Some call_site -> Inlined_frame call_site

  let rec iter_innermost_first t ~f =
    f t;
    match t.parent with
    | None -> ()
    | Some parent -> iter_innermost_first parent ~f

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 = Id.compare t1.id t2.id
    let equal t1 t2 = Id.equal t1.id t2.id
    let hash t = Id.hash t.id

    let rec print ppf { id; frame_location; parent; parents_transitive = _; } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(id@ %a)@]@ \
          @[<hov 1>(frame_location@ %a)@]@ \
          @[<hov 1>(parent@ %a)@])@]"
        Id.print id
        (Option.print Call_site.print) frame_location
        (Option.print print) parent

(*
        (match parent with
          | None -> "()"
          | Some parent -> Format.asprintf "%a" Id.print parent.id)
*)

    let output chan t =
      Format.fprintf (Format.formatter_of_out_channel chan) "%a%!" print t
  end)

  module Set = struct
    include Set

    let diff t1 t2 =
      if t1 == t2 then empty
      else diff t1 t2

    let union t1 t2 =
      if t1 == t2 then t1
      else union t1 t2

    let inter t1 t2 =
      if t1 == t2 then t1
      else inter t1 t2
  end

  let print_id ppf { id; _ } =
    Format.fprintf ppf "block %a" Id.print id

  let parents_transitive t = t.parents_transitive
end

module Current_block = struct
  type t = Block.t option

  let toplevel = None

  type to_block =
    | Toplevel
    | Block of Block.t

  let to_block t =
    match t with
    | None -> Toplevel
    | Some block -> Block block

  let add_scope t =
    Some (Block.create_lexical_scope ~parent:t)

  let add_inlined_frame t call_site =
    Some (Block.create_inlined_frame call_site ~parent:t)

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | None, None -> 0
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some block1, Some block2 -> Block.compare block1 block2

    let equal t1 t2 = (compare t1 t2 = 0)
    let hash t = Hashtbl.hash t

    let print ppf t = Misc.Stdlib.Option.print Block.print ppf t

    let output chan t =
      Format.fprintf (Format.formatter_of_out_channel chan) "%a%!" print t
  end)
end

type t =
  | Empty
  | Non_empty of {
      block : Block.t option;
      position : Code_range.t;
    }

type debuginfo = t

let none = Empty

let is_none = function
  | Empty -> true
  | Non_empty _ -> false

let to_string_frames_only_innermost_last t =
  match t with
  | Empty -> ""
  | Non_empty { block; position; } ->
    let frames =
      match block with
      | None -> []
      | Some block ->
        List.map (fun range -> Call_site.to_string range)
          (Block.frame_list_innermost_first block)
    in
    let ranges_innermost_last =
      List.rev ((Code_range.to_string position) :: frames)
    in
    "{" ^ String.concat ";" ranges_innermost_last ^ "}"

let of_line ~file ~line ~scope =
  let position = Code_range.of_line ~file ~line in
  Non_empty { block = scope; position; }

let with_position t position =
  match t with
  | Empty -> Empty
  | Non_empty { block; position = _; } -> Non_empty { block; position; }

let of_location loc ~scope =
  let position = Code_range.of_location loc in
  Non_empty { block = scope; position; }

let to_location t =
  match t with
  | Empty -> Location.none
  | Non_empty { block = _; position; } -> Code_range.to_location position

let of_function fun_dbg =
  Non_empty {
    block = None;
    position = Function.position fun_dbg;
  }

let innermost_block t =
  match t with
  | Empty -> None
  | Non_empty { block; position = _; } -> block

let position t =
  match t with
  | Empty -> None
  | Non_empty { block = _; position; } -> Some position

let iter_position_and_blocks_innermost_first t ~f_position ~f_blocks =
  match t with
  | Empty -> ()
  | Non_empty { block; position; } ->
    f_position position;
    match block with
    | None -> ()
    | Some block -> Block.iter_innermost_first block ~f:f_blocks

let iter_position_and_frames_innermost_first t ~f =
  iter_position_and_blocks_innermost_first t
    ~f_position:f
    ~f_blocks:(fun block ->
      match Block.frame_classification block with
      | Lexical_scope_only -> ()
      | Inlined_frame call_site -> f (Call_site.position call_site))

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf t =
    match t with
    | Empty -> Format.pp_print_string ppf "()"
    | Non_empty { block; position; } ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(position@ %a)@]@ \
          @[<hov 1>(block@ %a)@])@]"
        Code_range.print position
        (Misc.Stdlib.Option.print Block.print) block

  let output chan t =
    Format.fprintf (Format.formatter_of_out_channel chan) "%a%!" print t

  let compare t1 t2 =
    match t1, t2 with
    | Empty, Empty -> 0
    | Empty, Non_empty _ -> -1
    | Non_empty _, Empty -> 1
    | Non_empty { block = block1; position = position1; },
      Non_empty { block = block2; position = position2; } ->
      let c = 
        match block1, block2 with
        | None, None -> 0
        | None, Some _ -> -1
        | Some _, None -> 1
        | Some block1, Some block2 ->
          Block.compare block1 block2
      in
      if c <> 0 then c
      else Code_range.compare position1 position2

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t = Hashtbl.hash t
end)

module Apply = struct
  type t = {
    fun_dbg : Function.t;
    dbg : debuginfo;
  }

  let create fun_dbg dbg =
    { fun_dbg;
      dbg;
    }

  let fun_dbg t = t.fun_dbg
  let dbg t = t.dbg
end

module Block_subst = struct
  type t = Block.t Block.Map.t

  let empty = Block.Map.empty

  let rec find_or_add_block t (old_block : Block.t)
        ~(at_call_site : Current_block.t) : t * Block.t =
    match Block.Map.find old_block t with
    | exception Not_found ->
      let old_parent = Block.parent old_block in
      let t, new_parent =
        match old_parent with
        | None -> t, at_call_site
        | Some old_parent ->
          let t, block =
            find_or_add_block t old_parent ~at_call_site
          in
          t, Some block
      in
      let new_block =
        Block.create_and_reparent ~like:old_block ~new_parent
      in
      let t = Block.Map.add old_block new_block t in
      t, new_block
    | new_block -> t, new_block

  let find_or_add t (old_debuginfo : debuginfo) ~at_call_site =
    match old_debuginfo with
    | Empty -> t, Empty
    | Non_empty { block; position; } ->
      match block with
      | None ->
        t, Non_empty { block = at_call_site; position; }
      | Some block ->
        let t, block = find_or_add_block t block ~at_call_site in
        t, Non_empty { block = Some block; position; }

  let find_or_add_for_apply t (apply : Apply.t) ~at_call_site =
    let t, dbg = find_or_add t apply.dbg ~at_call_site in
    let apply =
      { apply with
        dbg;
      }
    in
    t, apply
end
