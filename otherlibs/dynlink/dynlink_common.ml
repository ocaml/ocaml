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

type implem_state =
  | Loaded
  | Not_initialized
  | Check_inited of int

type filename = string

type linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error =
  | Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | Cannot_open_dynamic_library of exn
  | Library's_module_initializers_failed of exn
  | Inconsistent_implementation of string
  | Module_already_loaded of string
  | Private_library_cannot_implement_interface of string

exception Error of error

let error_message = function
  | Not_a_bytecode_file name ->
    name ^ " is not an object file"
  | Inconsistent_import name ->
    "interface mismatch on " ^ name
  | Unavailable_unit name ->
    "no implementation available for " ^ name
  | Unsafe_file ->
    "this object file uses unsafe features"
  | Linking_error (name, Undefined_global s) ->
    "error while linking " ^ name ^ ".\n" ^
      "Reference to undefined global `" ^ s ^ "'"
  | Linking_error (name, Unavailable_primitive s) ->
    "error while linking " ^ name ^ ".\n" ^
      "The external function `" ^ s ^ "' is not available"
  | Linking_error (name, Uninitialized_global s) ->
    "error while linking " ^ name ^ ".\n" ^
      "The module `" ^ s ^ "' is not yet initialized"
  | Corrupted_interface name ->
    "corrupted interface file " ^ name
  | Cannot_open_dynamic_library exn ->
    "error loading shared library: " ^ (Printexc.to_string exn)
  | Inconsistent_implementation name ->
    "implementation mismatch on " ^ name
  | Library's_module_initializers_failed exn ->
    "execution of module initializers in the shared library failed: "
      ^ (Printexc.to_string exn)
  | Module_already_loaded name ->
    "The module `" ^ name ^ "' is already loaded \
      (either by the main program or a previously-dynlinked library)"
  | Private_library_cannot_implement_interface name ->
    "The interface `" ^ name ^ "' cannot be implemented by a \
      library loaded privately"

let () =
  Printexc.register_printer (function
    | Error err ->
      let msg = match err with
      | Not_a_bytecode_file s -> Printf.sprintf "Not_a_bytecode_file %S" s
      | Inconsistent_import s -> Printf.sprintf "Inconsistent_import %S" s
      | Unavailable_unit s -> Printf.sprintf "Unavailable_unit %S" s
      | Unsafe_file -> "Unsafe_file"
      | Linking_error (s, Undefined_global s') ->
        Printf.sprintf "Linking_error (%S, Dynlink.Undefined_global %S)"
          s s'
      | Linking_error (s, Unavailable_primitive s') ->
        Printf.sprintf "Linking_error (%S, Dynlink.Unavailable_primitive %S)"
          s s'
      | Linking_error (s, Uninitialized_global s') ->
        Printf.sprintf "Linking_error (%S, Dynlink.Uninitialized_global %S)"
          s s'
      | Corrupted_interface s ->
        Printf.sprintf "Corrupted_interface %S" s
      | Cannot_open_dynamic_library exn ->
        Printf.sprintf "Cannot_open_dynamic_library %S" (Printexc.to_string exn)
      | Inconsistent_implementation s ->
        Printf.sprintf "Inconsistent_implementation %S" s
      | Library's_module_initializers_failed exn ->
        Printf.sprintf "Library's_module_initializers_failed %S"
          (Printexc.to_string exn)
      | Module_already_loaded name ->
        Printf.sprintf "Module_already_loaded %S" name
      | Private_library_cannot_implement_interface name ->
        Printf.sprintf "Private_library_cannot_implement_interface %S" name
      in
      Some (Printf.sprintf "Dynlink.Error (Dynlink.%s)" msg)
    | _ -> None)

module type S = sig
  type handle

  module Unit_header : sig
    type t

    val name : t -> string
    val crc : t -> Digest.BLAKE128.t option

    val interface_imports : t -> (string * Digest.BLAKE128.t option) list
    val implementation_imports : t -> (string * Digest.BLAKE128.t option) list

    val defined_symbols : t -> string list
    val unsafe_module : t -> bool
  end

  val init : unit -> unit

  val is_native : bool

  val adapt_filename : filename -> filename

  val num_globals_inited : unit -> int

  val fold_initial_units
     : init:'a
    -> f:('a
      -> compunit:string
      -> interface:Digest.BLAKE128.t option
      -> implementation:(Digest.BLAKE128.t option * implem_state) option
      -> defined_symbols:string list
      -> 'a)
    -> 'a

  val load
     : filename:filename
    -> priv:bool
    -> handle * (Unit_header.t list)

  val run : Mutex.t -> handle -> unit_header:Unit_header.t -> priv:bool -> unit

  val unsafe_get_global_value : bytecode_or_asm_symbol:string -> Obj.t option

  val finish : handle -> unit
end

module String = struct
  include String
  module Set = Set.Make (String)
  module Map = struct
    include Map.Make (String)

    let keys t =
      fold (fun key _data keys -> Set.add key keys) t Set.empty
  end
end

module Make (P : S) = struct
  module UH = P.Unit_header

  type interface_dep =
    | Name  (* the only use of the interface can be via a module alias *)
    | Contents of Digest.BLAKE128.t

  type implem = Digest.BLAKE128.t option * filename * implem_state

  module State = struct
    type t = {
      ifaces : (interface_dep * filename) String.Map.t;
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
    String.Map.add (UH.name ui) (UH.crc ui, filename, Loaded) implems

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
            if old_crc <> crc then raise (Error (Inconsistent_import name))
            else ifaces)
      ifaces
      (UH.interface_imports ui)

  let check_implementation_imports ~allowed_units filename ui implems =
    List.iter (fun (name, crc) ->
      if not (String.Set.mem name allowed_units) then begin
        raise (Error (Unavailable_unit name))
      end;
      match String.Map.find name implems with
      | exception Not_found -> raise (Error (Unavailable_unit name))
      | ((old_crc, _old_src, unit_state) : implem) ->
        begin match old_crc, crc with
        | (None | Some _), None -> ()
        | None, Some _crc ->
          (* The [None] behaves like a CRC different from every other. *)
          raise (Error (Inconsistent_implementation name))
        | Some old_crc, Some crc ->
          if old_crc <> crc then begin
            raise (Error (Inconsistent_implementation name))
          end
        end;
        match unit_state with
        | Not_initialized ->
          raise (Error (Linking_error (
            filename, Uninitialized_global name)))
        | Check_inited i ->
          if P.num_globals_inited () < i then begin
            raise (Error (Linking_error (
              filename, Uninitialized_global name)))
          end
        | Loaded -> ())
      (UH.implementation_imports ui)

  let check_name filename ui priv ifaces implems =
    let name = UH.name ui in
    if String.Map.mem name implems then begin
      raise (Error (Module_already_loaded name))
    end;
    if priv && String.Map.mem name ifaces then begin
      raise (Error (Private_library_cannot_implement_interface name))
    end;
    String.Map.add name (UH.crc ui, filename, Not_initialized) implems

  let check_unsafe_module unsafe_allowed ui =
    if not unsafe_allowed && UH.unsafe_module ui then begin
      raise (Error Unsafe_file)
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
              ~priv
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
