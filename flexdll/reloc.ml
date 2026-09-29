(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

(* The main application: parse COFF files,
   compute relocation and export tables, rewrite some COFF files,
   call the native linker *)

open Compat
open Coff
open Cmdline

let debug ?(dry_mode = {contents=false}) min_level fmt =
  let print msg =
    if !dry_mode || !verbose >= min_level then
      Printf.printf "%s\n%!" msg
  in
    Printf.ksprintf print fmt

let search_path = ref []
let default_libs = ref []

let cc = function
  | `MSVC -> Version.msvc
  | `MSVC64 -> Version.msvc64
  | `CYGWIN64 -> Version.cygwin64
  | `MINGW -> Version.mingw
  | `MINGW64 -> Version.mingw64
  | `GNAT | `GNAT64 -> Version.gnat
  | `LIGHTLD -> failwith "No C compiler configured for this toolchain."

let objdump = ref "objdump"

let is_crt_lib = function
  | "LIBCMT"
  | "MSVCRT" -> true
  | _ -> false

let flexdir =
  try
    let s = Sys.getenv "FLEXDIR" in
    if s = "" then raise Not_found else s
  with Not_found ->
    Filename.dirname Sys.executable_name

let ext_obj () =
  if !toolchain = `MSVC || !toolchain = `MSVC64 then ".obj" else ".o"

(* Temporary files *)

let temps = ref []

let add_temp fn =
  temps := fn :: !temps; fn

let temp_file s x =
  add_temp (Filename.temp_file s x)

let open_temp_file s x =
  let (f, c) = Filename.open_temp_file s x in (add_temp f, c)

let safe_remove s =
  try Sys.remove s
  with Sys_error _ -> ()

let () =
  at_exit
    (fun () -> if not !save_temps then List.iter safe_remove !temps)

(* Calling external commands *)

let read_file fn =
  let ic = open_in fn in
  let r = ref [] in
  (try while true do r := input_line ic :: !r done with End_of_file -> ());
  close_in ic;
  List.rev !r

(* This is the longest command which can be passed to [Sys.command] *)
let max_command_length =
  let processor = try Sys.getenv "COMSPEC" with Not_found -> "cmd.exe" in
  (* The 4 is from the " /c " *)
  8191 - String.length processor - 4

let get_output ?(use_bash = false) ?(accept_error=false) cmd =
  let fn = Filename.temp_file "flexdll" "" in
  let cmd' = cmd ^ " > " ^ (Filename.quote fn) in
    if String.length cmd' <= max_command_length && not use_bash then
      begin
        if (Sys.command cmd' <> 0) && not accept_error
        then failwith ("Cannot run " ^ cmd);
      end
    else
      begin
        let (cfn, oc) = open_temp_file "longcmd" ".sh" in
          output_string oc cmd'; close_out oc;
          if Sys.command (Printf.sprintf "bash %s" cfn) <> 0
          then failwith ("Cannot run " ^ cmd)
      end;
    let r = read_file fn in
      Sys.remove fn;
      r

let get_output1 ?use_bash ?accept_error fmt =
  Printf.ksprintf (fun cmd ->
    match get_output ?use_bash ?accept_error cmd with
    | output::_ -> output
    | [] -> raise (Failure ("command " ^ cmd ^ " did not return any output"))) fmt

let get_output ?use_bash ?accept_error fmt =
  Printf.ksprintf (get_output ?use_bash ?accept_error) fmt

(* Preparing command line *)

let mk_dirs_opt pr = String.concat " " (List.map (fun s -> pr ^ (Filename.quote s)) !dirs)

exception Not_utf8

let utf8_next s i =
  let fail () = raise Not_utf8 in
  let check i =
    if i >= String.length s then fail ();
    let n = Char.code s.[i] in
    if n lsr 6 <> 0b10 then fail () else n
  in
  if !i >= String.length s then fail ();
  match s.[!i] with
  | '\000'..'\127' as c ->
      let n = Char.code c in
      i := !i + 1;
      n
  | '\192'..'\223' as c ->
      let n1 = Char.code c in
      let n2 = check (!i+1) in
      let n =
        ((n1 land 0b11111) lsl 6) lor
        ((n2 land 0b111111))
      in
      i := !i + 2;
      n
  | '\224'..'\239' as c ->
      let n1 = Char.code c in
      let n2 = check (!i+1) in
      let n3 = check (!i+2) in
      let n =
        ((n1 land 0b1111) lsl 12) lor
        ((n2 land 0b111111) lsl 6) lor
        ((n3 land 0b111111))
      in
      i := !i + 3;
      n
  | '\240'..'\247' as c ->
      let n1 = Char.code c in
      let n2 = check (!i+1) in
      let n3 = check (!i+2) in
      let n4 = check (!i+3) in
      let n =
        ((n1 land 0b111) lsl 18) lor
        ((n2 land 0b111111) lsl 12) lor
        ((n3 land 0b111111) lsl 6) lor
        ((n4 land 0b111111))
      in
      i := !i + 4;
      n
  | _ ->
      fail ()

let toutf16 s =
  let i = ref 0 in
  let b = Buffer.create (String.length s * 2) in
  while !i < String.length s do
    Buffer.add_utf_16le_uchar b (Uchar.unsafe_of_int (utf8_next s i))
  done;
  Buffer.contents b

(* Build @responsefile to work around Windows limitations on
   command-line length *)
let build_diversion lst =
  let responsefile = temp_file "camlresp" "" in
  let oc = open_out_bin responsefile in
  let lst =
    List.map (fun f ->
        let s = Bytes.of_string (Filename.quote f) in
        for i = 0 to Bytes.length s - 1 do
          if Bytes.get s i = '\\' then Bytes.set s i '/'
        done;
        Bytes.to_string s ^ "\r\n"
      ) (List.filter (fun f -> f <> "") lst)
  in
  let lst =
    match !toolchain with
    | `MINGW | `MINGW64 | `GNAT | `GNAT64 | `CYGWIN64 -> lst
    | `MSVC | `MSVC64 | `LIGHTLD ->
      (* UTF-16 response files required *)
      try
        let lst = List.map toutf16 lst in
        output_string oc "\xFF\xFE"; (* LE BOM *)
        lst
      with Not_utf8 -> lst
  in
  List.iter (fun s -> output_string oc s) lst;
  close_out oc;
  "@" ^ responsefile

let run_command cmd =
  let pipe_to_null = (!toolchain = `MSVC || !toolchain = `MSVC64) in
  let silencer = if pipe_to_null then " >NUL 2>NUL" else ""
  in
  (* note: for Cygwin, using bash allow to follow symlinks to find
     gcc... *)
  if Sys.unix || !toolchain = `CYGWIN64 ||
     String.length cmd + String.length silencer > max_command_length
  then begin
    (* Dump the command in a text file and apply bash to it. *)
    let (fn, oc) = open_temp_file "longcmd" "" in
    output_string oc cmd;
    if pipe_to_null then
      output_string oc " &>/dev/null";
    close_out oc;

    debug 1 "(call with bash: %s)\n%!" fn;
    let invoke = Printf.sprintf "bash %s" fn in
    if Sys.command invoke <> 0 then begin
      if pipe_to_null then begin
        let oc = open_out fn in
        output_string oc cmd;
        close_out oc;
        ignore (Sys.command invoke)
      end;
      failwith "Error during linking\n"
    end
  end else
    if Sys.command (cmd ^ silencer) <> 0 then begin
      if pipe_to_null then ignore (Sys.command cmd);
      failwith "Error during linking\n"
    end

let quote_files lst =
  let s =
    String.concat " "
      (List.map (fun f -> if f = "" then f else Filename.quote f) lst) in
  if not Sys.unix && String.length s >= 1024 then Filename.quote (build_diversion lst)
  else s


(* Looking for files *)

let cygpath l cont =
  let accept_error = (!use_cygpath = `Try && l <> []) in
  let l =
    let s = "-- " ^ String.concat " " (List.map Filename.quote l) in
    let args =
      if not Sys.cygwin && String.length s >= 1024 then begin
        (* cygpath loads the file in "text" mode, so CRLF endings are fine *)
        let (fn, oc) = open_temp_file "cygpathargs" "" in
        List.iter (fun x -> output_string oc x; output_char oc '\n') l;
        close_out oc;
        "--file " ^ Filename.quote fn
      end else
        s
    in
    get_output ~accept_error "cygpath -m %s" args
  in
  if accept_error && l = [] then begin
    use_cygpath := `No;
    None
  end else
    cont l

let cygpath1 fn =
  let accept_error = (!use_cygpath = `Try) in
  match get_output ~accept_error "cygpath -m %s" fn with
  | output::_ ->
      if accept_error then
        use_cygpath := `Yes;
      Some output
  | [] when accept_error ->
      use_cygpath := `No;
      None
  | [] ->
      raise (Failure "cygpath did not return any output")

let file_exists fn =
  if Sys.file_exists fn && not (Sys.is_directory fn) then Some fn
  else if !use_cygpath <> `No && Sys.file_exists (fn ^ ".lnk") then
    cygpath1 fn
  else None

let dir_exists_no_cygpath fn =
  Sys.file_exists fn && (try Sys.is_directory fn with Sys_error _ -> false)

let rec find_file_in = function
  | [] -> None
  | fn::rest ->
      match file_exists fn with
        | Some x -> Some x
        | None -> find_file_in rest

let find_file suffixes fn =
  let l =
    List.flatten
      (List.map
         (fun dir ->
            let fn = Filename.concat dir fn in
            fn :: (List.map (fun suff -> fn ^ suff) suffixes)
         ) (""::!search_path)) in
  match find_file_in l with
    | Some x -> Some x
    | None ->
        if !use_cygpath <> `No then
          cygpath l find_file_in
        else None

let rec map_until_found f = function
  | [] ->
      None
  | x::xs ->
      match f x with
      | None ->
          map_until_found f xs
      | r ->
          r

let find_file_exn =
  let memo = Hashtbl.create 16 in
  fun fn ->
    let k = String.lowercase_ascii fn in
    try Hashtbl.find memo k
    with Not_found ->
      try Hashtbl.find memo (k ^ ".lib")
      with Not_found ->
        let fns, suffixes =
          (* XXX Not sure why we do these extensions for _both_ MSVC and
                 mingw-w64 rather than .lib for MSVC and the .a ones for
                 mingw-w64? *)
          let standard_suffixes = [".lib"; ".dll.a"; ".a"] in
          if String.length fn > 2 && String.sub fn 0 2 = "-l" then
            let base = String.sub fn 2 (String.length fn - 2) in
            if String.length base > 0 && base.[0] = ':' then
              [String.sub base 1 (String.length base - 1)], []
            else if !toolchain = `MSVC || !toolchain = `MSVC64 then
              ["lib" ^ base; base], standard_suffixes
            else
              ["lib" ^ base], standard_suffixes
          else [fn], standard_suffixes in
        match map_until_found (find_file suffixes) fns with
        | Some fn ->
            Hashtbl.add memo k fn;
            Hashtbl.add memo (k ^ ".lib") fn;
            fn
        | None ->
            raise Not_found

let find_file fn =
  try find_file_exn fn
  with Not_found ->
    failwith (Printf.sprintf "Cannot find file %S" fn)

(*******************************)

let int32_to_buf b i =
  Buffer.add_char b (Char.chr (i land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 24) land 0xff))

let int_to_buf b i =
  assert(i >= 0);
  match !machine with
  | `x86 -> int32_to_buf b i
  | `x64 -> int32_to_buf b i; int32_to_buf b 0

let exportable s =
  match !machine with
  | `x86 ->
      s <> "" && (s.[0] = '_' || s.[0] = '?')
  | `x64 ->
      if String.length s > 2 && s.[0] = '?' && s.[1] = '?' then false
      else true

let drop_underscore obj s =
  match !machine with
  | `x86 ->
      assert (s <> "");
      begin
        match s.[0] with
        | '_' -> String.sub s 1 (String.length s - 1)
        | '?' -> s
        | _ -> failwith (Printf.sprintf "In %s, symbol %s doesn't start with _ or ?" obj.obj_name s)
      end
  | `x64 ->
      s

let has_prefix pr s =
  String.length s > String.length pr && String.sub s 0 (String.length pr) = pr

let check_prefix pr s =
  if has_prefix pr s then
    Some (String.sub s (String.length pr) (String.length s - String.length pr))
  else None

let parse_libpath s =
  let n = String.length s in
  let rec aux l =
    if l >= n then []
    else
      try
        let i = String.index_from s l ';' in
        String.sub s l (i - l) :: aux (succ i)
      with Not_found -> [ String.sub s l (n - l) ]
  in
  aux 0

module StrSet = Set.Make(String)

(* Put all the relocations on the symbols defined by a predicate
   into a relocation table. A relocation table describes how to
   patch some addresses with the value of some external symbols (given
   by their name). It also lists segments that are normally write-protected
   and that must be de-protected to enable the patching process. *)

let add_reloc_table obj obj_name p =
  let sname = Symbol.gen_sym () in (* symbol pointing to the reloc table *)
  let sect = Section.create ".reltbl" 0xc0300040l in
  let data = Buffer.create 1024 in
  let strings = Buffer.create 1024 in
  let nonwr = ref [] in
  let nonwrsym = Symbol.intern sect 0l in
  let strsym = Symbol.intern sect 0l in
  let str_pos = Hashtbl.create 16 in

  Reloc.abs !machine sect 0l nonwrsym;
  int_to_buf data 0;

  (* TODO: use a single symbol per section *)
  let syms = ref [] in
  let reloc secsym min max rel =
    if p rel.symbol then (
      (* kind = f(machine,rtype)
         - a RELOC_ constant in flexdll.c

         rtype
         - https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#x64-processors *)
      let kind = match !machine, rel.rtype with
        | `x86, 0x06 (* IMAGE_REL_I386_DIR32 *)
        | `x64, 0x01 (* IMAGE_REL_AMD64_ADDR64 *) ->
            0x0002 (* absolute, native size (32/64) *)

        | `x86, 0x07 (* IMAGE_REL_I386_DIR32NB *)
        | `x64, 0x03 (* IMAGE_REL_AMD64_ADDR32NB *) ->
            0x0007 (* 32nb *)

        | `x64, 0x04 (* IMAGE_REL_AMD64_REL32 *)
        | `x86, 0x14 (* IMAGE_REL_I386_REL32 *) when not !no_rel_relocs ->
            0x0001 (* rel32 *)

        | `x64, 0x05 (* IMAGE_REL_AMD64_REL32_1 *) when not !no_rel_relocs->
            0x0004 (* rel32_1 *)
        | `x64, 0x06 (* IMAGE_REL_AMD64_REL32_2 *) when not !no_rel_relocs->
            0x0005 (* rel32_2 *)
        | `x64, 0x07 (* IMAGE_REL_AMD64_REL32_3 *) when not !no_rel_relocs->
            0x0008 (* rel32_3 *)
        | `x64, 0x08 (* IMAGE_REL_AMD64_REL32_4 *) when not !no_rel_relocs->
            0x0003 (* rel32_4 *)
        | `x64, 0x09 (* IMAGE_REL_AMD64_REL32_5 *) when not !no_rel_relocs->
            0x0006 (* rel32_5 *)

        | (`x86 | `x64), (0x0a (* IMAGE_REL_{I386|AMD64}_SECTION *) |
                          0x0b (* IMAGE_REL_{I386|AMD64}_SECREL*) ) ->
            0x0100 (* debug relocs: ignore *)

        | _, k ->
            let msg =
              Printf.sprintf "Unsupported relocation kind %04x for %s in %s"
                k rel.symbol.sym_name obj_name
            in
            failwith msg
(*            Printf.eprintf "%s\n%!" msg;
            0x0001 *)
      in
      int_to_buf data kind;

      (* name *)
      let name = drop_underscore obj rel.symbol.sym_name in
      let pos =
        try Hashtbl.find str_pos name
        with Not_found ->
          let pos = Buffer.length strings in
          Hashtbl.add str_pos name pos;
          Buffer.add_string strings name;
          Buffer.add_char strings '\000';
          pos
      in
      Reloc.abs !machine sect (Int32.of_int (Buffer.length data)) strsym;
      int_to_buf data pos;

      Reloc.abs !machine sect (Int32.of_int (Buffer.length data))
        (Lazy.force secsym);
      int_to_buf data (Int32.to_int rel.addr);

      if rel.addr <= !min then min := rel.addr;
      if rel.addr >= !max then max := rel.addr;
      false
    ) else true
  in
  let section sec =
    if sec.sec_opts &&& 0x1000l <> 0l && has_prefix ".rdata$.refptr." sec.sec_name then
      begin
        (* under Cygwin64, gcc introduces mergable (link once) COMDAT sections to store
           indirection pointers to external darta symbols.  Since we don't deal with such section
           properly, we turn them into regular data section, thus loosing sharing (but we don't care). *)
        sec.sec_opts <- 0xc0500040l;
        sec.sec_name <- Printf.sprintf ".flexrefptrsection%i" (Oo.id (object end));
      end;

    let min = ref Int32.max_int and max = ref Int32.min_int in
    let sym = lazy (let s = Symbol.intern sec 0l in
                    syms := s :: !syms;
                    s) in

    sec.relocs <- filter (reloc sym min max) sec.relocs;
    if (sec.sec_opts &&& 0x80000000l = 0l) && !min <= !max then
      nonwr := (!min,!max,Lazy.force sym) :: !nonwr
  in
  List.iter section obj.sections;
  int_to_buf data 0;
  strsym.value <- Int32.of_int (Buffer.length data);
  Buffer.add_buffer data strings;
  nonwrsym.value <- Int32.of_int (Buffer.length data);
  List.iter
    (fun (min,max,secsym) ->
      Reloc.abs !machine sect (Int32.of_int (Buffer.length data)) secsym;
      int_to_buf data (Int32.to_int min);
      Reloc.abs !machine sect (Int32.of_int (Buffer.length data)) secsym;
      int_to_buf data (Int32.to_int max);
      int_to_buf data 0;
    )
    !nonwr;
  int_to_buf data 0;
  int_to_buf data 0;
  sect.data <- `String (Buffer.to_bytes data);
  obj.sections <- sect :: obj.sections;
  obj.symbols <-
    (Symbol.export sname sect 0l) ::
    strsym :: nonwrsym :: List.filter (fun x -> not (p x)) obj.symbols
    @ !syms;
  sname

(* Create a table for import symbols __imp_XXX *)

let add_import_table obj imports =
  let ptr_size = match !machine with `x86 -> 4 | `x64 -> 8 in
  let sect = Section.create ".imptbl" 0xc0300040l in
  obj.sections <- sect :: obj.sections;
  sect.data <- `String (Bytes.make (ptr_size * List.length imports) '\000');
  let i = ref 0 in
  List.iter
    (fun s ->
       let sym = Symbol.extern s in
       obj.symbols <-
         sym :: Symbol.export ("__imp_" ^ s) sect (Int32.of_int !i) ::
         obj.symbols;
       Reloc.abs !machine sect (Int32.of_int !i) sym;
       i := !i + ptr_size
    )
    imports


(* Create a table that lists exported symbols (adress,name) *)

let add_export_table obj exports symname =
  let sect = Section.create ".exptbl" 0xc0300040l in
  let data = Buffer.create 1024 in
  let strings = Buffer.create 1024 in
  let strsym = Symbol.intern sect 0l in
  obj.symbols <- strsym :: (Symbol.export symname sect 0l) :: obj.symbols;
  let exports = List.sort Stdlib.compare exports in
  (* The runtime library assumes that names are sorted! *)
  int_to_buf data (List.length exports);
  List.iter
    (fun s ->
       let sym = Symbol.extern s in
       obj.symbols <- sym :: obj.symbols;
       Reloc.abs !machine sect (Int32.of_int (Buffer.length data)) sym;
       int_to_buf data 0;

       Reloc.abs !machine sect (Int32.of_int (Buffer.length data)) strsym;
       int_to_buf data (Buffer.length strings);
       Buffer.add_string strings (drop_underscore obj s);
       Buffer.add_char strings '\000';
    )
    exports;
  strsym.value <- Int32.of_int (Buffer.length data);
  let s = Bytes.cat (Buffer.to_bytes data) (Buffer.to_bytes strings) in
  sect.data <- `String s;
  obj.sections <- sect :: obj.sections

(* A master relocation table points to all the relocation tables
   in the DLL *)

let add_master_reloc_table obj names symname =
  let sect = Section.create ".mreltbl" 0xc0300040l in
  let data = Buffer.create 1024 in
  obj.symbols <- (Symbol.export symname sect 0l) :: obj.symbols;
  List.iter
    (fun s ->
       let sym = Symbol.extern s in
       obj.symbols <- sym :: obj.symbols;
       Reloc.abs !machine sect (Int32.of_int (Buffer.length data)) sym;
       int_to_buf data 0;
    )
    names;
  int_to_buf data 0;
  sect.data <- `String (Buffer.to_bytes data);
  obj.sections <- sect :: obj.sections



let collect_dllexports obj =
  let dirs = Coff.directives obj in
  let l =
    List.map
      (function
        | (_,x::_) -> x
        | _ -> assert false
      )
      (List.find_all (fun (cmd,_args) -> String.uppercase_ascii cmd = "EXPORT") dirs)
  in
  match !toolchain with
  | `MSVC | `MSVC64 -> List.map (drop_underscore obj) l
  | _ -> l

let collect f l =
  List.fold_left
    (fun accu x -> match f x with None -> accu | Some y -> y :: accu)
    []
    l

let cmd_verbose cmd =
  debug 1 "+ %s" cmd;
  Sys.command cmd


let parse_dll_exports fn =
  let ic = open_in fn in
  let exps = ref [] in
  try
    while input_line ic <> "[Ordinal/Name Pointer] Table" do () done;
    while true do
      let s = input_line ic in
      let r = String.index s ']' in
      let sym = String.sub s (r+2) (String.length s - r - 2) in
      exps := ("_" ^ sym,0) :: !exps;
    done;
    assert false
  with Not_found | End_of_file ->
    close_in ic;
    !exps


let dll_exports fn = match !toolchain with
  | `MSVC | `MSVC64 | `LIGHTLD ->
      failwith "Creation of import library not supported for this toolchain"
  | `GNAT | `GNAT64 | `CYGWIN64 | `MINGW | `MINGW64 ->
      let dmp = temp_file "dyndll" ".dmp" in
      if cmd_verbose (Printf.sprintf "%s -p %s > %s" !objdump fn dmp) <> 0
      then failwith "Error while extracting exports from a DLL";
      parse_dll_exports dmp


let patch_output filename =
  match !stack_reserve with
  | Some x ->
      let filename =
        if not (Sys.file_exists filename) && (Sys.file_exists (filename ^ ".exe")) then filename ^ ".exe"
        else filename
      in
      begin try Stacksize.set_stack_reserve filename x
      with exn ->
        Printf.eprintf "Cannot set stack reserve: %s\n%!"
          (Printexc.to_string exn)
      end
  | None -> ()


(* Extract the set of external symbols required by an object. *)
(* If the object requires "__imp_X", and "X" is available in one of the objects/libraries
   (but not "__imp_X" itself), then we consider that "X" is required.
   Indeed, we will create "__imp_X" (with a redirection to "X").
   Collect such cases in "imported".
*)
let needed imported defined resolve_alias resolve_alternate obj =
  let rec normalize name =
    try
      let r = resolve_alias name in
      if r <> name then normalize r else r
    with Not_found ->
      (* Fall back to alternate name if and only if name was not found.
         https://devblogs.microsoft.com/oldnewthing/20200731-00/?p=104024 *)
      try
        let r = resolve_alternate name in
        if r <> name then normalize r else r
      with Not_found ->
        name
  in
  let normalize_imp name =
    match check_prefix "__imp_" name with
    | Some s when not (StrSet.mem name defined) ->
        imported := StrSet.add s !imported;
        if StrSet.mem s defined then s else name
    | None when not (StrSet.mem name defined) && StrSet.mem ("__imp_" ^ name) defined ->
        imported := StrSet.add ("__imp_" ^ name) !imported;
        "__imp_" ^ name
    | _ -> name
  in
  List.fold_left
    (fun accu sym ->
       if Symbol.is_extern sym then StrSet.add (normalize_imp (normalize sym.sym_name)) accu
       else accu
    )
    StrSet.empty
    obj.symbols

let build_dll link_exe output_file files exts extra_args =
  let main_pgm = link_exe <> `DLL in

  (* fully resolve filenames, eliminate duplicates *)
  let _, files =
    List.fold_left
      (fun (seen, accu) fn ->
         let fn = find_file fn in
         let k = String.lowercase_ascii fn in
         if StrSet.mem k seen then (seen, accu)
         else (StrSet.add k seen, fn :: accu)
      ) (StrSet.empty, []) files in
  let files = List.rev files in

  (* load given files *)
  let loaded_filenames : (string,unit) Hashtbl.t = Hashtbl.create 16 in
  let read_file fn =
    if Lib.is_dll fn then `Lib ([], dll_exports fn)
    else begin
      if !verbose >= 2 then Printf.printf "** open: %s\n" fn;
      Lib.read fn
    end
  in
  let files = List.map (fun fn -> fn, read_file fn) files in

  List.iter (fun (fn,_) -> Hashtbl.add loaded_filenames fn ()) files;

  let objs = collect (function (f, `Obj x) -> Some (f,x) | _ -> None) files in
  let libs = collect (function (f, `Lib (x,_)) -> Some (f,x) | _ -> None) files in

  let with_data_symbol symbols sym_name f =
    if !toolchain <> `MSVC && !toolchain <> `MSVC64 then
      match check_prefix "__nm_" sym_name with
      | None -> ()
      | Some s ->
        let imp_name = "__imp_" ^ s  in
        if List.exists ( fun p -> Symbol.is_defin p &&
          p.sym_name = imp_name) symbols then
          f s;
  in
  (* Collect all the available symbols, including those defined
     in default libraries *)
  let defined, from_imports, resolve_alias, resolve_alternate =
    let aliases = Hashtbl.create 16 in
    let alternates = Hashtbl.create 16 in
    let defined = ref StrSet.empty in
    let from_imports = ref StrSet.empty in (* symbols from import libraries *)
    let add_def s = defined := StrSet.add s !defined in

    let collected = Hashtbl.create 8 in
    let rec collect_defined_obj obj =
      (* see comments on Cygwin64 COMDATA sections.  Here we give a
         unique name to the internal symbol.  We use ?? to ensure the
         symbol is not exported in flexdll export table (see
         exportable function) *)
      List.iter
        (fun sym ->
           if has_prefix ".refptr." sym.sym_name then
             sym.sym_name <- Printf.sprintf "??flexrefptr%i" (Oo.id (object end))
        )
        obj.symbols;

      (* Collect aliases *)
      List.iter
        (fun (x, y) ->
           debug 2 "alias %s -> %s" x y;
           Hashtbl.add aliases x y
        )
        (Coff.aliases obj);

      (* Collect alternatenames *)
      let collect_alternatenames alternatenames =
        List.iter (fun s -> match String.split_on_char '=' s with
            | [alternate_name; true_name] ->
                debug 2 "alternatename %s -> %s" alternate_name true_name;
                Hashtbl.add alternates alternate_name true_name
            | _ ->
                debug 1 "alternatenames unrecognized: %s" s;
          ) alternatenames
      in
      List.iter (function
          | ("alternatename", alternatenames) ->
              collect_alternatenames alternatenames
          | _ -> ())
        (Coff.directives obj);

      (* Iterates through DEFAULTLIB directives *)
      let register_deflib fn =
        if not !custom_crt || not (is_crt_lib fn) then
          let fn = find_file fn in
          if not (Hashtbl.mem loaded_filenames fn)
          then (Hashtbl.add loaded_filenames fn (); collect_file fn)
      in
      if not !builtin_linker && !use_default_libs then
        List.iter
          (fun (cmd, args) ->
             if String.uppercase_ascii cmd = "DEFAULTLIB" then List.iter register_deflib args
          )
          (Coff.directives obj);

      (* Collect defined symbols *)
      List.iter
        (fun sym ->
          if Symbol.is_defin sym then (
            add_def sym.sym_name;
            with_data_symbol obj.symbols sym.sym_name add_def)
        )
        obj.symbols

    and collect_file fn =
      if not (Hashtbl.mem collected (String.lowercase_ascii fn)) then begin
        Hashtbl.replace collected (String.lowercase_ascii fn) ();
        debug 2 "** open: %s" fn;
        collect_defined fn (Lib.read fn)
      end

    and collect_defined fn = function
      | `Obj obj -> collect_defined_obj obj
      | `Lib (objs,imports) ->
          List.iter (fun (_, obj) -> collect_defined_obj obj) objs;
          List.iter
            (fun (s,_) ->
               debug 2 "lib %s import symbol %s" fn s;
               from_imports := StrSet.add s !from_imports;
               add_def s;
               add_def ("__imp_" ^ s)
            )
            imports
    in
    List.iter
      (fun (fn,x) ->
         Hashtbl.replace collected (String.lowercase_ascii fn) ();
         collect_defined fn x
      )
      files;
    if !use_default_libs then List.iter (fun fn -> collect_file (find_file fn)) !default_libs;
    List.iter (fun fn -> collect_file (find_file fn)) exts;

    if main_pgm then add_def (usym "static_symtable")
    else add_def (usym "reloctbl");

    if !machine = `x64 then add_def "__ImageBase"
    else add_def "___ImageBase";

    !defined, !from_imports, (Hashtbl.find aliases), (Hashtbl.find alternates)
  in

  (* Determine which objects from the given libraries should be linked
     in. First step: find the mapping (symbol -> object) for these
     objects. *)
  let defined_in =
    let defined_in = Hashtbl.create 16 in
    let def_in_obj fn (objname, obj) =
      List.iter
        (fun sym ->
           if Symbol.is_defin sym
           then begin
             let f s =
               if !explain then
                 Printf.printf "Symbol %s found in %s(%s)\n%!" s fn objname;
               Hashtbl.replace defined_in s (fn,objname,obj);
             in
             f sym.sym_name;
             with_data_symbol obj.symbols sym.sym_name f
           end
        )
        obj.symbols
    in
    List.iter
      (fun (fn,objs) ->
         if !explain then Printf.printf "Scanning lib %s\n%!" fn;
         List.iter (def_in_obj fn) objs
      )
      libs;
    Hashtbl.find defined_in
  in

  let imported_from_implib = ref StrSet.empty in
  let imported = ref StrSet.empty in

  let imports obj =
    let n = needed imported defined resolve_alias resolve_alternate obj in
    imported_from_implib := StrSet.union !imported_from_implib (StrSet.inter n from_imports);
    let undefs = StrSet.diff n defined in
    StrSet.filter
      (fun s ->
         match check_prefix "__imp_" s with
         | Some _ -> false
         | None -> s <> "environ"  (* special hack for Cygwin64 *)
      )
      undefs
  in

  (* Second step: transitive closure, starting from given objects *)

  let libobjects = Hashtbl.create 16 in
  let reloctbls = ref [] in
  let exported = ref StrSet.empty in

  List.iter (fun s -> exported := StrSet.add (usym s) !exported) !defexports;

  let record_obj obj =
    if !builtin_linker then ""
    else begin
      let fn = temp_file "dyndll" (ext_obj ()) in
      let oc = open_out_bin fn in
      Coff.put oc obj;
      close_out oc;
      fn
    end
  in

  let add_reloc name obj imps =
    if !show_imports && not (StrSet.is_empty imps) then (
      Printf.printf "** Imported symbols for %s:\n%!" name;
      StrSet.iter print_endline imps
    );
    let sym = add_reloc_table obj name (fun s -> StrSet.mem s.sym_name imps) in
    reloctbls := sym :: !reloctbls
  in

  let errors = ref false in
  let error_imports name imps =
    if main_pgm then begin
      Printf.eprintf "** Cannot resolve symbols for %s:\n %s\n%!"
        name
        (String.concat "\n " (StrSet.elements imps));
      errors := true
    end
  in

  let close_obj name imps obj =
    error_imports name imps;
    add_reloc name obj imps;
    record_obj obj
  in

  let dll_exports = ref StrSet.empty in
  let rec link_obj fn obj =
    List.iter
      (fun sym ->
         if Symbol.is_defin sym && exportable sym.sym_name
         then exported := StrSet.add sym.sym_name !exported
      )
      obj.symbols;

    dll_exports := List.fold_left (fun accu x -> StrSet.add x accu)
        !dll_exports (collect_dllexports obj);
    StrSet.iter
      (fun s ->
        if StrSet.mem s !exported then ()
        else
          try
            let (libname, objname, _) as o = defined_in s in
            if !explain then
              Printf.printf "%s -> %s(%s) because of %s\n%!" fn libname objname s;
            link_libobj o
          with Not_found ->
            if !explain then
              Printf.printf "%s needs %s (not found)\n%!" fn s
      )
      (needed imported defined resolve_alias resolve_alternate obj)

  and link_libobj (libname,objname,obj) =
    if Hashtbl.mem libobjects (libname,objname) then ()
    else (Hashtbl.replace libobjects (libname,objname) (obj,imports obj);
          link_obj (Printf.sprintf "%s(%s)" libname objname) obj)
  in

  let redirect = Hashtbl.create 16 in
  List.iter
    (fun (fn, obj) ->
       link_obj fn obj;
       let imps = imports obj in
       if StrSet.is_empty imps then ()
       else Hashtbl.replace redirect fn (close_obj fn imps obj);
    ) objs;

  let need_lib = Hashtbl.create 16 in
  Hashtbl.iter
    (fun (libname,objname) (obj,imps) ->
      if StrSet.is_empty imps
      then Hashtbl.replace need_lib libname ()
          (* the linker will find this object in this library *)
      else begin
        if !explain then
          Printf.printf "Library object %s(%s) needs to be rewritten\n%!"
            libname objname;
        Hashtbl.add redirect libname
          (close_obj (Printf.sprintf "%s(%s)" libname objname) imps obj)
      end
    )
    libobjects;

  if !show_exports then (
    Printf.printf "** Exported symbols:\n";
    StrSet.iter print_endline !exported;
    Printf.printf "** Symbols from import libs:\n";
    StrSet.iter print_endline !imported_from_implib;
    flush stdout
  );

  if !reexport_from_implibs then
    exported := StrSet.union !exported !imported_from_implib;

  (* Create the descriptor object *)
  let obj = Coff.create !machine in

  if not (StrSet.is_empty !imported) then begin
(*
    Printf.printf "** __imp symbols:\n%!";
    StrSet.iter print_endline !imported;
*)
    add_import_table obj (StrSet.elements !imported);
    let undef_imports = StrSet.diff !imported defined in
    if not (StrSet.is_empty undef_imports) then begin
      error_imports "descriptor object" undef_imports;
      add_reloc "descriptor object" obj undef_imports;
    end
  end;

  add_export_table obj (if !noexport then [] else StrSet.elements !exported)
    (usym (if main_pgm then "static_symtable" else "symtbl"));
  if not main_pgm then add_master_reloc_table obj !reloctbls (usym "reloctbl");

  if !errors then
    exit 2;

  if !builtin_linker then begin
    let objs = List.map
        (function
           | (_, `Obj obj) -> obj
           | (fn, _) -> failwith ("File is not an object file: " ^ fn)
        ) files
    in
    let oc = open_out_bin output_file in
    Create_dll.create_dll oc (List.rev (obj :: objs));
    close_out oc
  end else

  let descr = record_obj obj in
  let files =
    List.flatten
      (List.map
         (fun (fn,d) ->
           let all = Hashtbl.find_all redirect fn in
           if all = [] then [fn]
           else
             match d with
             | `Lib _ when Hashtbl.mem need_lib fn -> all @ [fn]
             | `Lib (_, []) | `Obj _ -> all
             | `Lib _ -> all @ [fn]
            (* Note: extracted object have higher priorities
               than objects embedded in the library, so this is ok.
               We always keep libraries with import symbols.
               For mingw, it is necessary to put the library after
               extracted objects. *)
         )
         files
      )
    @ exts in

  let files = quote_files files in
  let descr = Filename.quote descr in

  begin
    match !deffile with
    | Some x when not !dry_mode ->
        let fn =
          if x = "" then Filename.chop_extension output_file ^ ".def"
          else x
        in
        debug 1 "Generate %s" fn;
        let oc = open_out fn in
        Printf.fprintf oc "LIBRARY %s\n" output_file;
        Printf.fprintf oc "EXPORTS\n";
        StrSet.iter (Printf.fprintf oc "  %s\n") !dll_exports;
        close_out oc
    | _ -> ()
  end;

  let cmd = match !toolchain with
    | `MSVC | `MSVC64 ->
        let link = Option.value !Cmdline.use_linker ~default:"link" in

        (* Putting the file the descriptor object at the beginning
           with MSVC compilers seems to break Stack overflow recovery
           in OCaml. No idea why. *)

        let implib =
          if !implib then
            Filename.chop_extension output_file ^ ".lib"
          else
            temp_file "dyndll_implib" ".lib"
        in
        (* VS 2017.3 doesn't seem to be able to cope with /implib: existing but
           being an empty file. *)
        let c = open_out implib in output_string c "x"; close_out c;
        let _impexp = add_temp (Filename.chop_suffix implib ".lib" ^ ".exp") in
        let extra_args =
          if !custom_crt then "/nodefaultlib:LIBCMT /nodefaultlib:MSVCRT " ^ extra_args
          else "msvcrt.lib " ^ extra_args
        in

        let extra_args =
          if !machine = `x64 then (Printf.sprintf "/base:%s " !base_addr) ^ extra_args else extra_args
        in

        let extra_args =
          (* FlexDLL doesn't process .voltbl sections correctly, so don't allow the linker
             to process them. *)
          let command =
            if Sys.win32 then link ^ " /nologo /? | findstr EMITVOLATILEMETADATA > NUL"
            else link ^ " /nologo '/?' | grep -iq emitvolatilemetadata >/dev/null" in
          if Sys.command command = 0 then
            "/EMITVOLATILEMETADATA:NO " ^ extra_args
          else extra_args
        in

        (* Flexdll requires that all images (main programs and all the DLLs) are
           not too far away. This is needed because of the 32-bit relative relocations
           (e.g. function calls). It seems that passing such a /base argument to link.exe
           gives some hope that this will be the case. Problems observed otherwise
           with the Windows 7 SDK in 64-bit mode. *)

        Printf.sprintf
          "%s /nologo %s%s%s%s%s /implib:%s /out:%s /subsystem:%s %s %s %s"
          link
          (if !verbose >= 2 then "/verbose " else "")
          (if link_exe = `EXE then "" else "/dll ")
          (if main_pgm then "" else "/export:symtbl /export:reloctbl ")
          (if main_pgm then "" else if !noentry then "/noentry " else
          let s =
            match !machine with
            | `x86 -> "FlexDLLiniter@12"
            | `x64 -> "FlexDLLiniter"
          in
          Printf.sprintf "/entry:%s " s
          )
          (mk_dirs_opt "/libpath:")
          (Filename.quote implib)
          (Filename.quote output_file)
          !subsystem
          files descr
          extra_args
    | `CYGWIN64 ->
        let def_file =
          if main_pgm then ""
          else
            let def_file, oc = open_temp_file "flexlink" ".def" in
            Printf.fprintf oc "EXPORTS\n  reloctbl\n  symtbl\n";
            close_out oc;
            Filename.quote def_file
        in
        Printf.sprintf
          "%s %s%s%s -L. %s %s -o %s %s %s %s %s"
          (cc !toolchain)
          (Option.fold ~none:"" ~some:(fun ld -> "-fuse-ld=" ^ ld ^ " ") !Cmdline.use_linker)
          (if link_exe = `EXE then "" else "-shared ")
          (if main_pgm then "" else if !noentry then "-Wl,-e0 " else if !machine = `x86 then "-Wl,-e_FlexDLLiniter@12 " else "-Wl,-eFlexDLLiniter ")
          (mk_dirs_opt "-I")
          (mk_dirs_opt "-L")
          (Filename.quote output_file)
          descr
          files
          def_file
          extra_args
    | `MINGW | `MINGW64 | `GNAT | `GNAT64 ->
        let def_file =
          if main_pgm then ""
          else
            let def_file, oc = open_temp_file "flexlink" ".def" in
            Printf.fprintf oc "EXPORTS\n  reloctbl\n  symtbl\n";
            close_out oc;
            Filename.quote def_file
        in
        Printf.sprintf
          "%s -m%s %s%s%s -L. %s %s -o %s %s %s %s %s %s"
          (cc !toolchain)
          !subsystem
          (Option.fold ~none:"" ~some:(fun ld -> "-fuse-ld=" ^ ld ^ " ") !Cmdline.use_linker)
          (if link_exe = `EXE then "" else "-shared ")
          (if main_pgm then "" else if !noentry then "-Wl,-e0 " else if !machine = `x86 then "-Wl,-e_FlexDLLiniter@12 " else "-Wl,-eFlexDLLiniter ")
          (mk_dirs_opt "-I")
          (mk_dirs_opt "-L")
          (Filename.quote output_file)
          descr
          files
          def_file
          (if !implib then "-Wl,--out-implib=" ^ Filename.quote (Filename.chop_extension output_file ^ ".a") else "")
          extra_args
    | `LIGHTLD ->
        no_merge_manifest := true;
        let ld = Option.value !Cmdline.use_linker ~default:"ld" in
        Printf.sprintf
          "%s %s%s -o %s %s %s %s %s"
          ld
          (if link_exe = `EXE then "" else "--shared ")
          (if main_pgm then "" else if !noentry then "-e0 " else "-e FlexDLLiniter@12 ")
          (Filename.quote output_file)
          descr
          files
          (if !implib then "--out-implib " ^ Filename.quote (Filename.chop_extension output_file ^ ".a") else "")
          extra_args
  in
  debug ~dry_mode 1 "+ %s" cmd;
  if not !dry_mode then begin
    let manifest_file = output_file ^ ".manifest" in
    safe_remove manifest_file;
    run_command cmd;

    if (not !no_merge_manifest) && !merge_manifest && (not !real_manifest || Sys.file_exists manifest_file)
    then begin
      let fn =
        if !real_manifest then manifest_file
        else
          let default_manifest =
            match !machine with
            | `x86 -> "default.manifest"
            | `x64 -> "default_amd64.manifest"
          in
          Filename.concat flexdir default_manifest
      in
      let mcmd =
        let mt = Option.value !Cmdline.use_mt ~default:"mt" in
        Printf.sprintf "%s -nologo -outputresource:%s -manifest %s"
          mt
          (Filename.quote (if link_exe = `EXE then output_file
                           else output_file ^ ";#2"))
          (Filename.quote fn)
      in
      debug 1 "+ %s" mcmd;
      if Sys.command mcmd <> 0 then
        failwith "Error while merging the manifest";
      safe_remove manifest_file;
    end;
    patch_output output_file
 end

let ends_with s suf =
  let rec aux s suf suflen offset i =
    i >= suflen || (s.[i + offset] = suf.[i]
                   && aux s suf suflen offset (i+1)) in
  let slen = String.length s in
  let suflen = String.length suf in
  slen >= suflen && aux s suf suflen (slen - suflen) 0

let strip s =
  let rec search i s =
    if s.[i] = ' ' then search (i+1) s
    else String.sub s i (String.length s - i)
  in
  search 0 s

let read_gnatls () =
   (* This function is used by the GNAT toolchain to compute the include
      directory. gnatls actually returns with an error code different to 0, so
      we need to accept the error here. *)
   let str_l = get_output ~accept_error:true "gnatls -v" in
   let ada_include =
     List.hd (List.filter (fun s -> ends_with s "adainclude") str_l) in
   Filename.dirname (strip ada_include)

let split str sep =
  let p = String.index str sep in
  let slen = String.length str in
  String.sub str 0 p, String.sub str (p + 1) (slen - p - 1)

let nsplit str sep =
  if str = "" then []
  else
    let rec loop acc pos =
      if pos > String.length str then
        List.rev acc
      else
        let i = try String.index_from str pos sep with Not_found -> String.length str in
        loop (String.sub str pos (i - pos) :: acc) (i + 1)
    in
    loop [] 0

let normalize_path path =
  let path =
    if Sys.win32 then
      let back_to_forward c = if c = '\\' then '/' else c in
      String.init (String.length path) (fun i -> back_to_forward path.[i])
    else
      path
  in
  let path = nsplit path '/' in
  let rec loop acc path =
    match path with
    | "."  :: path -> loop acc path
    | ".." :: path -> begin
        match acc with
        | ".." :: _ | [] -> loop (".." :: acc) path
        | _ :: acc -> loop acc path
      end
    | elem :: path -> loop (elem :: acc) path
    | [] -> List.rev acc
  in
  let path = loop [] path in
  String.concat Filename.dir_sep path

let remove_duplicate_paths paths =
  let set = Hashtbl.create 16 in
  let rec loop paths =
    match paths with
    | path :: paths ->
        begin try
          Hashtbl.find set path;
          loop paths
        with Not_found ->
          Hashtbl.add set path ();
          path :: loop paths
        end
    | [] -> []
  in
  loop paths

let setup_toolchain () =
  let mingw_libs pre =
    objdump := pre ^ "objdump";
    let rec get_lib_search_dirs install libraries input =
      match input with
      | entry :: input ->
          if String.length entry > 9 && String.sub entry 0 9 = "install: " then
            let install = String.sub entry 9 (String.length entry - 9) in
            (* Ensure install does not end with a separator (or
               [Sys.is_directory] will fail) *)
            let install = Filename.concat install Filename.current_dir_name
                          |> Filename.dirname in
            get_lib_search_dirs (Some install) libraries input
          else begin try
            match split entry '=' with
            | "libraries: ", paths -> get_lib_search_dirs install paths input
            | _ -> get_lib_search_dirs install libraries input
          with Not_found ->
            get_lib_search_dirs install libraries input
          end
      | [] ->
          let separator, run_through_cygpath =
            if Sys.win32 then
              if Option.fold ~none:true ~some:dir_exists_no_cygpath install then
                ';', false
              else
                ':', (!use_cygpath <> `No)
            else
              ':', false
          in
          let libraries = nsplit libraries separator in
          if run_through_cygpath then
            Option.value ~default:libraries (cygpath libraries Option.some)
          else
            libraries
    in
    let lib_search_dirs =
      get_lib_search_dirs None "" (get_output "%s -print-search-dirs" (cc !toolchain))
      |> List.filter (( <> ) "")
      |> List.map normalize_path
      |> remove_duplicate_paths
    in
    search_path := !dirs @ lib_search_dirs;
    if !verbose >= 1 then begin
      Printf.printf "lib search dirs (%s):\n" (cc !toolchain);
      List.iter (Printf.printf "  %s\n") lib_search_dirs;
      flush stdout
    end;
    default_libs :=
      [ "-lmoldname"; "-lmingwex"; "-lmsvcrt"; "-luser32"; "-lkernel32";
        "-ladvapi32"; "-lshell32" ];
    (* -lgcc_eh isn't guaranteed to be available (e.g. if using a static
       compiler - cf. ocaml/ocaml#12996. Parsing GCC's specs is a bit too much
       work, so instead treat -lgcc_eh as optional *)
    let () =
      try
        let _ = find_file_exn "-lgcc_eh" in
        default_libs := "-lgcc_eh" :: !default_libs
      with Not_found -> ()
    in
    default_libs := "-lmingw32" :: "-lgcc" :: !default_libs;
    if !exe_mode = `EXE then default_libs := "crt2.o" :: !default_libs
    else default_libs := "dllcrt2.o" :: !default_libs
  in
  match !toolchain with
  | _ when !builtin_linker ->
      search_path := !dirs;
      add_flexdll_obj := false;
      noentry := true
  | `CYGWIN64 ->
      objdump := "objdump";
      search_path :=
        !dirs @
          [
           "/lib";
           "/lib/w32api";
           Filename.dirname (get_output1 ~use_bash:true "%s -print-libgcc-file-name" (cc !toolchain));
          ];
      default_libs := ["-lkernel32"; "-luser32"; "-ladvapi32";
                       "-lshell32"; "-lcygwin"; "-lgcc_s"; "-lgcc"]
  | `MSVC | `MSVC64 ->
      search_path := !dirs @
        parse_libpath (try Sys.getenv "LIB" with Not_found -> "");
      if not !custom_crt then
        default_libs := ["msvcrt.lib"]
  | `MINGW ->
      mingw_libs Version.mingw_prefix
  | `MINGW64 ->
      mingw_libs Version.mingw64_prefix
  | `GNAT | `GNAT64 ->
   (* This is a plain copy of the mingw version, but we do not change the
      prefix and use "gnatls" to compute the include dir. *)
    search_path :=
      !dirs @
      [
       Filename.dirname (get_output1 "%s -print-libgcc-file-name" (cc !toolchain));
       read_gnatls ();
      ];
    default_libs :=
      ["-lmingw32"; "-lgcc"; "-lmoldname"; "-lmingwex"; "-lmsvcrt";
       "-luser32"; "-lkernel32"; "-ladvapi32"; "-lshell32" ];
    if !exe_mode = `EXE then default_libs := "crt2.o" :: !default_libs
    else default_libs := "dllcrt2.o" :: !default_libs
  | `LIGHTLD ->
      search_path := !dirs

let display_msvc_output file name =
  let c = open_in file in
  try
    let first = input_line c in
    if first <> (Filename.basename name) then
      print_string first;
    while true do
      print_string (input_line c)
    done
  with _ ->
    close_in c

let compile_if_needed file =
  if Filename.check_suffix file ".c" then begin
    let tmp_obj = temp_file "dyndll" (ext_obj ()) in
    let (pipe, stdout) =
      if (!toolchain = `MSVC || !toolchain = `MSVC64) && !verbose < 2 && not !dry_mode then
        try
          let (t, c) = open_temp_file "msvc" "stdout" in
          close_out c;
          (Printf.sprintf " > %s" (Filename.quote t), t)
        with _ ->
          ("", "")
      else
        ("", "") in
    let cmd = match !toolchain with
      | `MSVC | `MSVC64 ->
          Printf.sprintf
            "%s /c /MD /nologo /Fo%s %s %s%s"
            (cc !toolchain)
            (Filename.quote tmp_obj)
            (mk_dirs_opt "/I")
            file
            pipe
      | `CYGWIN64 ->
          Printf.sprintf
            "%s -c -o %s %s %s"
            (cc !toolchain)
            (Filename.quote tmp_obj)
            (mk_dirs_opt "-I")
            file
      | `MINGW | `MINGW64 | `GNAT | `GNAT64 ->
          Printf.sprintf
            "%s -c -o %s %s %s"
            (cc !toolchain)
            (Filename.quote tmp_obj)
            (mk_dirs_opt "-I")
            (Filename.quote file)
      | `LIGHTLD ->
          failwith "Compilation of C code is not supported for this toolchain"
    in
    debug ~dry_mode 1 "+ %s" cmd;
    let exit = if !dry_mode then 0 else Sys.command cmd in
    if pipe <> "" then display_msvc_output stdout file;
    if exit <> 0 then failwith "Error while compiling";
    tmp_obj
  end else
    file

let dump fn =
  let fn = find_file fn in
  Printf.printf "*** %s:\n" fn;
  match Lib.read fn with
  | `Lib (objs,imports) ->
      List.iter
        (fun (n,o) ->
          Printf.printf "** %s(%s):\n" fn n;
          Coff.dump o
        )
        objs;
      List.iter
        (fun (s,i) -> Printf.printf "** import: %s (%i)\n" s i)
        imports;
      flush stdout
  | `Obj o ->
      Coff.dump o

let all_files () =
  let files = List.rev (List.map compile_if_needed !files) in
  let f obj =
    let fn = Filename.concat flexdir obj in
    (* Allow the obj files to be stored in a different location *)
    if file_exists fn <> None then
      fn
    else
      obj in
  let tc = match !toolchain with
  | `MSVC -> "msvc.obj"
  | `MSVC64 -> "msvc64.obj"
  | `CYGWIN64 -> "cygwin64.o"
  | `MINGW64 -> "mingw64.o"
  | `GNAT -> "gnat.o"
  | `GNAT64 -> "gnat64.o"
  | `MINGW | `LIGHTLD -> "mingw.o" in
  if !exe_mode <> `DLL then
    if !add_flexdll_obj then f ("flexdll_" ^ tc) :: files
    else files
  else
    if !noentry then files
    else f ("flexdll_initer_" ^ tc) :: files

let main () =
  parse_cmdline ();
  setup_toolchain ();

  if !verbose >= 2 then (
    let use_cygpath =
      match !use_cygpath with
      | `Yes -> "true"
      | `No -> "false"
      | `Try -> "maybe"
    in
    Printf.printf "** Use cygpath: %s\n" use_cygpath;
    Printf.printf "** Search path:\n";
    List.iter print_endline !search_path;
    if !use_default_libs then begin
      Printf.printf "** Default libraries:\n";
      List.iter print_endline !default_libs;
    end;
    flush stdout
   );
  let files = all_files () in
  match !mode with
  | `DUMP -> List.iter dump files
  | `NORMAL ->
      build_dll !exe_mode !output_file files !exts
        (String.concat " " (List.map Filename.quote (List.rev !extra_args)))
  | `PATCH ->
      let output_file =
        if !use_cygpath <> `No then
          Option.value ~default:!output_file (cygpath1 !output_file)
        else
          !output_file in
      patch_output output_file

let () =
  try main ()
  with
    | Failure s ->
        Printf.eprintf "** Fatal error: %s\n" s;
        exit 2
    | Invalid_argument s ->
        Printf.eprintf "** Fatal error: invalid argument (%s)\n" s;
        exit 2
    | Arg.Help s ->
        Printf.printf "%s\n%s\n" s footer;
        exit 0
    | Arg.Bad s ->
        Printf.eprintf "%s\n%s\n" s footer;
        exit 2
    | exn ->
        Printf.eprintf "** Error: %s\n" (Printexc.to_string exn);
        exit 2
