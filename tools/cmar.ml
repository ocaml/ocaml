open Misc

type fileType = Cma | Cmxa | Cmo | Cmx | Unknown;;
type modeType = List | Extract;;

exception InvalidLibraryFileType;; (* Library file has invalid type *)

(* Read first bytes of file - magic number and determine if
 * this number corresponds to any of Cma, Cmx, Cmo and etc. *)
let determine_file_type ichannel =
  let magicNumbers = [(Config.cma_magic_number, Cma);
                      (Config.cmxa_magic_number, Cmxa);
                      (Config.cmo_magic_number, Cmo);
                      (Config.cmx_magic_number, Cmx)] 
  in
  let length = String.length (fst (List.hd magicNumbers)) in
  let lengths_are_different = 
    List.fold_left 
        (fun are_different (str, _) -> are_different || (String.length str) <> length)
        false (List.tl magicNumbers)
  in
  if lengths_are_different then
  (
    Printf.eprintf "Internal error: magic numbers have different lengths.\n";
    exit (-1)
  );
  let buffer = Bytes.create length in
  let _ = really_input ichannel buffer 0 length in
  let filePrefix = Bytes.to_string buffer in
  List.fold_left
    (fun fileType (magicNumber, magicType) ->
      match fileType with
      | Unknown -> 
          if filePrefix = magicNumber then
            magicType
          else
            Unknown
      | _ -> fileType
    ) Unknown magicNumbers

type library_type = CMAlibrary of Cmo_format.library 
                  | CMXAlibrary of Cmx_format.library_infos;;

(* Read cma or cmxa library info from channel.
 * Assumed that channel is seeked to 0. *) 
let read_library_info channel =
  let file_type = determine_file_type channel in
  match file_type with
  | Cma ->
  (
    let pos = input_binary_int channel in
    seek_in channel pos;
    CMAlibrary (input_value channel : Cmo_format.library)
  )
  | Cmxa -> CMXAlibrary (input_value channel : Cmx_format.library_infos)
  | _ -> raise InvalidLibraryFileType
;;

(* Get list of unit names from library information. *)
let unit_names_of_library library_info =
  match library_info with
  | CMAlibrary lib ->
    List.map (fun cmo_unit -> cmo_unit.Cmo_format.cu_name) lib.Cmo_format.lib_units;
  | CMXAlibrary lib ->
    List.map (fun (cmx_unit, _) -> cmx_unit.Cmx_format.ui_name) lib.Cmx_format.lib_units;
;;

(* Remove extension from unit name, if any. *)
let strip_extension unit_name =
  let length = String.length unit_name in
  if length < 4 then
      unit_name
  else
    let extension = String.sub unit_name (length - 5) 4 in
    if extension = ".cmo" || extension = ".cmx" then
      String.sub unit_name 0 (length - 4)
    else
      unit_name
;;

type unit_type = CMOUnit of Cmo_format.compilation_unit | 
                 CMXUnit of Cmx_format.unit_infos * Digest.t;;

(* Extract single unit from library. *)
let extract_unit (library_ichannel, library_info) unit_name =
  let comp_unit, unit_file_ext =
    match library_info with
    | CMAlibrary lib ->
      let comp_unit = 
        List.find 
          (fun cmo_unit -> cmo_unit.Cmo_format.cu_name = unit_name) 
          lib.Cmo_format.lib_units
      in
      (CMOUnit comp_unit, ".cmo")
    | CMXAlibrary lib ->
      let comp_unit, digest =
        List.find 
          (fun (cmx_unit, _) -> cmx_unit.Cmx_format.ui_name = unit_name) 
          lib.Cmx_format.lib_units
      in
      (CMXUnit (comp_unit, digest), ".cmx")
  in
  let unit_ochannel = 
    open_out_bin ((String.uncapitalize_ascii unit_name) ^ unit_file_ext)
  in
  (
    match comp_unit with
    | CMOUnit comp_unit ->
      output_string unit_ochannel Config.cmo_magic_number;
      let ofs_pos_info = pos_out unit_ochannel in
      output_binary_int unit_ochannel 0;

      (* copying bytecode and debuginfo *)
      seek_in library_ichannel comp_unit.Cmo_format.cu_pos;
      comp_unit.Cmo_format.cu_pos <- pos_out unit_ochannel;
      copy_file_chunk library_ichannel unit_ochannel comp_unit.Cmo_format.cu_codesize;

      if comp_unit.Cmo_format.cu_debug > 0 then begin
        seek_in library_ichannel comp_unit.Cmo_format.cu_debug;
        comp_unit.Cmo_format.cu_debug <- pos_out unit_ochannel;
        copy_file_chunk library_ichannel unit_ochannel comp_unit.Cmo_format.cu_debugsize
      end;

      let pos_info = pos_out unit_ochannel in
      (* writing compilation unit *)
      output_value unit_ochannel comp_unit;
      (* getting back to right after magic number and writing compilation_unit position *)
      seek_out unit_ochannel ofs_pos_info;
      output_binary_int unit_ochannel pos_info;
    | CMXUnit (unit_info, crc) ->
      output_string unit_ochannel Config.cmx_magic_number;
      output_value unit_ochannel unit_info;
      Digest.output unit_ochannel crc
  );
  close_out unit_ochannel
;;

(* Extract contents of the library. Units may have .cmo or .cmx extension,
 * or have no extension at all. *)
let extract_library_contents ichannel library_info units_to_extract =
  let library_units = unit_names_of_library library_info in
  let units_to_extract = List.sort_uniq compare units_to_extract in
  let (_, missing_in_library) = 
    List.partition
      (fun unit_to_extract ->
        try
          let _ = List.find ((=) unit_to_extract) library_units in
          true
        with Not_found -> false
      ) units_to_extract
  in
  match missing_in_library with
  | [] -> 
    List.iter (extract_unit (ichannel, library_info)) units_to_extract
  | _::[] ->
  (
    Printf.eprintf "Module ";
    List.iter (Printf.eprintf "%s ") missing_in_library;
    Printf.eprintf "is not in the library.\n";
    exit (-1)
  )
  | _ ->
  (
    Printf.eprintf "Modules ";
    List.iter (Printf.eprintf "%s ") missing_in_library;
    Printf.eprintf "are not in the library.\n";
    exit (-1)
  )
;;

(* Print unit names from library_info. *)
let print_unit_names library_info =
  List.iter (Printf.printf "%s\n") (unit_names_of_library library_info);
;;

(* List or extract (see mode) units from library *)
let list_extract_library_units mode library units =
  try
    let channel = open_in_bin library in
    let library_info = read_library_info channel in
    match mode with
    | List -> print_unit_names library_info
    | Extract -> extract_library_contents channel library_info units
  with 
    InvalidLibraryFileType ->
    (
      Printf.eprintf "Library file %s has invalid type.\n" library;
      exit (-1)
    ) 
    | _ ->
    (
      Printf.eprintf "Problem opening library file %s.\n" library;
      exit (-1)
    )
;;

(* Variables for argument parsing. *)
let mode = ref List;;
let files : string list ref = ref [];;

(* Command line arguments *)
let arg_list = [
  "-list", Arg.Unit (fun _ -> mode := List), " : list the contents of .cma or .cmxa file (default)";
  "-extract", Arg.Unit (fun _ -> mode := Extract), " : extract all object files from .cma or .cmxa file";
]
let arg_usage =
  Printf.sprintf "%s [OPTIONS] LIBRARY [UNITS] : list or extract contents of the library"
                 Sys.argv.(0)

let arg_anon_fun name = files := (!files) @ [name];;

let main () =
  Arg.parse arg_list arg_anon_fun arg_usage;
  match !mode, !files with
  | List, library::[] -> list_extract_library_units !mode library []
  | Extract, library::units -> list_extract_library_units !mode library units
  | _ -> Printf.eprintf "%s\n" arg_usage

let _ = main ()
