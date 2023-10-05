(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*             Mark Shinwell and Leo White, Jane Street Europe            *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module String = struct
  include String
  module Set = Set.Make (String)
  module Map = struct
    include Map.Make (String)

    let keys t =
      fold (fun key _data keys -> Set.add key keys) t Set.empty
  end
end

module Make (P : Dynlink_platform_intf.S) = struct
  module DT = Dynlink_types
  module UH = P.Unit_header

  type interface_dep =
    | Name  (* the only use of the interface can be via a module alias *)
    | Contents of Digest.t

  type implem = Digest.t option * DT.filename * DT.implem_state

  module State = struct
    type t = {
      ifaces : (interface_dep * DT.filename) String.Map.t;
      (* Interfaces that have been depended upon. *)
      implems : implem String.Map.t;
      (* Implementations that exist in the main program or have been
         dynamically loaded. *)
      defined_symbols : String.Set.t;
      (* Symbols corresponding to compilation units or packed modules (cf.
         [Asmpackager.build_package_cmx]).  Used as a sanity check. *)
      allowed_units : String.Set.t;
      (* Units that are allowed to be referenced by a subsequently-loaded
         dynamic library. *)
      main_program_units : String.Set.t;
      (* Units forming part of the main program (i.e. not dynamically
         linked). *)
      public_dynamically_loaded_units : String.Set.t;
      (* All units that have been dynamically linked, not including those that
         were privately loaded. *)
    }

    let empty = {
      ifaces = String.Map.empty;
      implems = String.Map.empty;
      defined_symbols = String.Set.empty;
      allowed_units = String.Set.empty;
      main_program_units = String.Set.empty;
      public_dynamically_loaded_units = String.Set.empty;
    }
  end

(* Limit the number of concurrent users to one *)
  module Global: sig
    type t = {
      mutable state:State.t;
      mutable inited:bool;
      mutable unsafe_allowed:bool;
    }
    val lock: Mutex.t
    val with_lock: (t->'a) -> 'a
  end
  = struct
    let lock = Mutex.create ()
    type t = {
      mutable state:State.t;
      mutable inited:bool;
      mutable unsafe_allowed:bool;
    }
    let state = {
      state = State.empty;
      inited = false;
      unsafe_allowed = false;

    }
    let with_lock0 f =
      Mutex.lock lock;
      Fun.protect f
        ~finally:(fun () -> Mutex.unlock lock)
    let with_lock f = with_lock0 (fun () -> f state)
  end
  open Global

  let allow_unsafe_modules b =
    with_lock (fun global -> global.unsafe_allowed <- b)

  let check_symbols_disjoint ~descr syms1 syms2 =
    let exe = Sys.executable_name in
    let overlap = String.Set.inter syms1 syms2 in
    if not (String.Set.is_empty overlap) then begin
      let msg =
        Format.asprintf "%s: symbols multiply-defined %s: %a"
          exe (Lazy.force descr)
          (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
            Format.pp_print_string)
          (String.Set.elements overlap)
      in
      failwith msg
    end

  let default_available_units global =
    let exe = Sys.executable_name in
    let ifaces, implems, defined_symbols =
      P.fold_initial_units
        ~init:(String.Map.empty, String.Map.empty, String.Set.empty)
        ~f:(fun (ifaces, implems, defined_symbols)
                ~compunit ~interface ~implementation
                ~defined_symbols:defined_symbols_this_unit ->
          let ifaces =
            match interface with
            | None -> String.Map.add compunit (Name, exe) ifaces
            | Some crc -> String.Map.add compunit (Contents crc, exe) ifaces
          in
          let implems =
            match implementation with
            | None -> implems
            | Some (crc, state) ->
              String.Map.add compunit (crc, exe, state) implems
          in
          let defined_symbols_this_unit =
            String.Set.of_list defined_symbols_this_unit
          in
          check_symbols_disjoint ~descr:(lazy "in the executable file")
            defined_symbols_this_unit defined_symbols;
          let defined_symbols =
            String.Set.union defined_symbols_this_unit defined_symbols
          in
          ifaces, implems, defined_symbols)
    in
    let main_program_units = String.Map.keys implems in
    let state : State.t =
      { ifaces;
        implems;
        defined_symbols;
        allowed_units = main_program_units;
        main_program_units;
        public_dynamically_loaded_units = String.Set.empty;
      }
    in
    global.state <- state

  let init () =
    with_lock (fun global ->
    if not global.inited then begin
      P.init ();
      default_available_units global;
      global.inited <- true
    end)

  let set_loaded_implem filename ui implems =
    String.Map.add (UH.name ui) (UH.crc ui, filename, DT.Loaded) implems

  let set_loaded filename ui (state : State.t) =
    { state with implems = set_loaded_implem filename ui state.implems }

  let check_interface_imports filename ui ifaces =
    List.fold_left (fun ifaces (name, crc) ->
        match String.Map.find name ifaces with
        | exception Not_found -> begin
            match crc with
            | None -> String.Map.add name (Name, filename) ifaces
            | Some crc -> String.Map.add name (Contents crc, filename) ifaces
          end
        | old_crc, _old_src ->
          match old_crc, crc with
          | (Name | Contents _), None -> ifaces
          | Name, Some crc ->
            String.Map.add name (Contents crc, filename) ifaces
          | Contents old_crc, Some crc ->
            if old_crc <> crc then raise (DT.Error (Inconsistent_import name))
            else ifaces)
      ifaces
      (UH.interface_imports ui)

  let check_implementation_imports ~allowed_units filename ui implems =
    List.iter (fun (name, crc) ->
      if not (String.Set.mem name allowed_units) then begin
        raise (DT.Error (Unavailable_unit name))
      end;
      match String.Map.find name implems with
      | exception Not_found -> raise (DT.Error (Unavailable_unit name))
      | ((old_crc, _old_src, unit_state) : implem) ->
        begin match old_crc, crc with
        | (None | Some _), None -> ()
        | None, Some _crc ->
          (* The [None] behaves like a CRC different from every other. *)
          raise (DT.Error (Inconsistent_implementation name))
        | Some old_crc, Some crc ->
          if old_crc <> crc then begin
            raise (DT.Error (Inconsistent_implementation name))
          end
        end;
        match unit_state with
        | Not_initialized ->
          raise (DT.Error (Linking_error (
            filename, Uninitialized_global name)))
        | Check_inited i ->
          if P.num_globals_inited () < i then begin
            raise (DT.Error (Linking_error (
              filename, Uninitialized_global name)))
          end
        | Loaded -> ())
      (UH.implementation_imports ui)

  let check_name filename ui priv ifaces implems =
    let name = UH.name ui in
    if String.Map.mem name implems then begin
      raise (DT.Error (Module_already_loaded name))
    end;
    if priv && String.Map.mem name ifaces then begin
      raise (DT.Error (Private_library_cannot_implement_interface name))
    end;
    String.Map.add name (UH.crc ui, filename, DT.Not_initialized) implems

  let check_unsafe_module unsafe_allowed ui =
    if not unsafe_allowed && UH.unsafe_module ui then begin
      raise (DT.Error Unsafe_file)
    end

  let check filename (units : UH.t list) (state : State.t)
      ~unsafe_allowed ~priv =
    List.iter (fun ui -> check_unsafe_module unsafe_allowed ui) units;
    let new_units =
      String.Set.of_list (List.map (fun ui -> UH.name ui) units)
    in
    let implems =
      List.fold_left (fun implems ui ->
          check_name filename ui priv state.ifaces implems)
        state.implems units
    in
    let ifaces =
      List.fold_left (fun ifaces ui ->
          check_interface_imports filename ui ifaces)
        state.ifaces units
    in
    let allowed_units = String.Set.union state.allowed_units new_units in
    let (_ : implem String.Map.t) =
      List.fold_left
        (fun acc ui ->
           check_implementation_imports ~allowed_units filename ui acc;
           set_loaded_implem filename ui acc)
        implems units
    in
    let defined_symbols =
      List.fold_left (fun defined_symbols ui ->
          let descr =
            lazy (Printf.sprintf "between the executable file (and any \
                existing dynamically-loaded units) and the unit `%s' being \
                dynamically loaded from %s"
              (UH.name ui)
              filename)
          in
          let symbols = String.Set.of_list (UH.defined_symbols ui) in
          check_symbols_disjoint ~descr symbols defined_symbols;
          String.Set.union symbols defined_symbols)
        state.defined_symbols
        units
    in
    if priv then begin
      state
    end else begin
      let public_dynamically_loaded_units =
        String.Set.union state.public_dynamically_loaded_units new_units
      in
      let state =
        { state with
          implems;
          ifaces;
          defined_symbols;
          allowed_units;
          public_dynamically_loaded_units;
        }
      in
      state
    end

  let set_allowed_units allowed_units =
    let allowed_units = String.Set.of_list allowed_units in
    with_lock (fun global ->
        global.state <- { global.state with allowed_units }
      )

  let allow_only units =
    with_lock (fun global ->
        let allowed_units =
          String.Set.inter global.state.allowed_units
            (String.Set.of_list units)
        in
        global.state <- { global.state with allowed_units }
      )

  let prohibit units =
    with_lock (fun global ->
        let allowed_units =
          String.Set.diff global.state.allowed_units
            (String.Set.of_list units)
        in
        global.state <- { global.state with
          allowed_units;
        }
      )

  let main_program_units () =
    init ();
    let global_state = with_lock (fun {state;_} -> state) in
    String.Set.elements global_state.main_program_units

  let public_dynamically_loaded_units () =
    init ();
    let global_state = with_lock (fun {state;_} -> state) in
    String.Set.elements global_state.public_dynamically_loaded_units

  let all_units () =
    init ();
    let global_state = with_lock (fun {state;_} -> state) in
    String.Set.elements (String.Set.union
      global_state.main_program_units
      global_state.public_dynamically_loaded_units)

  let dll_filename fname =
    if Filename.is_implicit fname then Filename.concat (Sys.getcwd ()) fname
    else fname

  let load priv filename =
    init ();
    let filename = dll_filename filename in
    let handle, units = P.load ~filename ~priv in
    Fun.protect ~finally:(fun () -> P.finish handle) (fun () ->
      with_lock (fun ({unsafe_allowed; _ } as global) ->
          global.state <- check filename units global.state
              ~unsafe_allowed
              ~priv;
          P.run_shared_startup handle;
        );
      List.iter
        (fun unit_header ->
           (* Linked modules might call Dynlink themselves,
              we need to release the lock *)
           P.run Global.lock handle ~unit_header ~priv;
           if not priv then with_lock (fun global ->
               global.state <- set_loaded filename unit_header global.state
             )
        )
        units
      )

  let loadfile filename = load false filename
  let loadfile_private filename = load true filename

  let unsafe_get_global_value ~bytecode_or_asm_symbol =
    with_lock (fun _ ->
        (* The bytecode implementation reads the global symtable *)
        P.unsafe_get_global_value ~bytecode_or_asm_symbol
      )

  let is_native = P.is_native
  let adapt_filename = P.adapt_filename
end
