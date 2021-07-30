(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let char_to_hex c =
  Printf.sprintf "0x%02x" (Char.code c)

let int_to_hex n =
  Printf.sprintf "0x%x" n

type error =
  | Truncated_file
  | Unrecognized of string
  | Unsupported of string * int64
  | Out_of_range of string

let error_to_string = function
  | Truncated_file ->
      "Truncated file"
  | Unrecognized magic ->
      Printf.sprintf "Unrecognized magic: %s"
        (String.concat " "
           (List.init (String.length magic)
              (fun i -> char_to_hex magic.[i])))
  | Unsupported (s, n) ->
      Printf.sprintf "Unsupported: %s: 0x%Lx" s n
  | Out_of_range s ->
      Printf.sprintf "Out of range constant: %s" s

exception Error of error

let name_at ?max_len buf start =
  if start < 0 || start > Bytes.length buf then
    raise (Error (Out_of_range (int_to_hex start)));
  let max_pos =
    match max_len with
    | None -> Bytes.length buf
    | Some n -> Int.min (Bytes.length buf) (start + n)
  in
  let rec loop pos =
    if pos >= max_pos || Bytes.get buf pos = '\000'
    then
      Bytes.sub_string buf start (pos - start)
    else
      loop (succ pos)
  in
  loop start

let array_find_map f a =
  let rec loop i =
    if i >= Array.length a then None
    else begin
      match f a.(i) with
      | None -> loop (succ i)
      | Some _ as r -> r
    end
  in
  loop 0

let array_find f a =
  array_find_map (fun x -> if f x then Some x else None) a

let really_input_bytes ic len =
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  buf

let uint64_of_uint32 n =
  Int64.(logand (of_int32 n) 0xffffffffL)

type endianness =
  | LE
  | BE

type bitness =
  | B32
  | B64

type decoder =
  {
    ic: in_channel;
    endianness: endianness;
    bitness: bitness;
  }

let word_size = function
  | {bitness = B64; _} -> 8
  | {bitness = B32; _} -> 4

let get_uint16 {endianness; _} buf idx =
  match endianness with
  | LE -> Bytes.get_uint16_le buf idx
  | BE -> Bytes.get_uint16_be buf idx

let get_uint32 {endianness; _} buf idx =
  match endianness with
  | LE -> Bytes.get_int32_le buf idx
  | BE -> Bytes.get_int32_be buf idx

let get_uint s d buf idx =
  let n = get_uint32 d buf idx in
  match Int32.unsigned_to_int n with
  | None -> raise (Error (Unsupported (s, Int64.of_int32 n)))
  | Some n -> n

let get_uint64 {endianness; _} buf idx =
  match endianness with
  | LE -> Bytes.get_int64_le buf idx
  | BE -> Bytes.get_int64_be buf idx

let get_word d buf idx =
  match d.bitness with
  | B64 -> get_uint64 d buf idx
  | B32 -> uint64_of_uint32 (get_uint32 d buf idx)

let uint64_to_int s n =
  match Int64.unsigned_to_int n with
  | None -> raise (Error (Unsupported (s, n)))
  | Some n -> n

let load_bytes d off len =
  LargeFile.seek_in d.ic off;
  really_input_bytes d.ic len

type t =
  {
    defines_symbol: string -> bool;
    symbol_offset: string -> int64 option;
  }

module ELF = struct

  (* Reference: http://man7.org/linux/man-pages/man5/elf.5.html *)

  let header_size d =
    40 + 3 * word_size d

  type header =
    {
      e_shoff: int64;
      e_shentsize: int;
      e_shnum: int;
      e_shstrndx: int;
    }

  let read_header d =
    let buf = load_bytes d 0L (header_size d) in
    let word_size = word_size d in
    let e_shnum = get_uint16 d buf (36 + 3 * word_size) in
    let e_shentsize = get_uint16 d buf (34 + 3 * word_size) in
    let e_shoff = get_word d buf (24 + 2 * word_size) in
    let e_shstrndx = get_uint16 d buf (38 + 3 * word_size) in
    {e_shnum; e_shentsize; e_shoff; e_shstrndx}

  type sh_type =
    | SHT_STRTAB
    | SHT_DYNSYM
    | SHT_OTHER

  type section =
    {
      sh_name: int;
      sh_type: sh_type;
      sh_addr: int64;
      sh_offset: int64;
      sh_size: int;
      sh_entsize: int;
      sh_name_str: string;
    }

  let load_section_body d {sh_offset; sh_size; _} =
    load_bytes d sh_offset sh_size

  let read_sections d {e_shoff; e_shnum; e_shentsize; e_shstrndx; _} =
    let buf = load_bytes d e_shoff (e_shnum * e_shentsize) in
    let word_size = word_size d in
    let mk i =
      let base = i * e_shentsize in
      let sh_name = get_uint "sh_name" d buf (base + 0) in
      let sh_type =
        match get_uint32 d buf (base + 4) with
        | 3l -> SHT_STRTAB
        | 11l -> SHT_DYNSYM
        | _ -> SHT_OTHER
      in
      let sh_addr = get_word d buf (base + 8 + word_size) in
      let sh_offset = get_word d buf (base + 8 + 2 * word_size) in
      let sh_size =
        uint64_to_int "sh_size"
          (get_word d buf (base + 8 + 3 * word_size))
      in
      let sh_entsize =
        uint64_to_int "sh_entsize"
          (get_word d buf (base + 16 + 5 * word_size))
      in
      {sh_name; sh_type; sh_addr; sh_offset;
       sh_size; sh_entsize; sh_name_str = ""}
    in
    let sections = Array.init e_shnum mk in
    if e_shstrndx = 0 then
      (* no string table *)
      sections
    else
      let shstrtbl = load_section_body d sections.(e_shstrndx) in
      let set_name sec =
        let sh_name_str = name_at shstrtbl sec.sh_name in
        {sec with sh_name_str}
      in
      Array.map set_name sections

  let read_sections d h =
    let {e_shoff; e_shentsize; e_shnum; e_shstrndx} = h in
    if e_shoff = 0L then
      [||]
    else begin
      let buf = lazy (load_bytes d e_shoff e_shentsize) in
      let word_size = word_size d in
      let e_shnum =
        if e_shnum = 0 then
          (* The real e_shnum is the sh_size of the initial section.*)
          uint64_to_int "e_shnum"
            (get_word d (Lazy.force buf) (8 + 3 * word_size))
        else
          e_shnum
      in
      let e_shstrndx =
        if e_shstrndx = 0xffff then
          (* The real e_shstrndx is the sh_link of the initial section. *)
          get_uint "e_shstrndx" d (Lazy.force buf) (8 + 4 * word_size)
        else
          e_shstrndx
      in
      read_sections d {h with e_shnum; e_shstrndx}
    end

  type symbol =
    {
      st_name: string;
      st_value: int64;
      st_shndx: int;
    }

  let find_section sections type_ sectname =
    let f {sh_type; sh_name_str; _} =
      sh_type = type_ && sh_name_str = sectname
    in
    array_find f sections

  let read_symbols d sections =
    match find_section sections SHT_DYNSYM ".dynsym" with
    | None -> [| |]
    | Some {sh_entsize = 0; _} ->
        raise (Error (Out_of_range "sh_entsize=0"))
    | Some dynsym ->
        begin match find_section sections SHT_STRTAB ".dynstr" with
        | None -> [| |]
        | Some dynstr ->
            let strtbl = load_section_body d dynstr in
            let buf = load_section_body d dynsym in
            let word_size = word_size d in
            let mk i =
              let base = i * dynsym.sh_entsize in
              let st_name = name_at strtbl (get_uint "st_name" d buf base) in
              let st_value = get_word d buf (base + word_size (* ! *)) in
              let st_shndx =
                let off = match d.bitness with B64 -> 6 | B32 -> 14 in
                get_uint16 d buf (base + off)
              in
              {st_name; st_value; st_shndx}
            in
            Array.init (dynsym.sh_size / dynsym.sh_entsize) mk
        end

  let find_symbol symbols symname =
    let f = function
      | {st_shndx = 0; _} -> false
      | {st_name; _} -> st_name = symname
    in
    array_find f symbols

  let symbol_offset sections symbols symname =
    match find_symbol symbols symname with
    | None ->
        None
    | Some {st_shndx; st_value; _} ->
        (* st_value in executables and shared objects holds a virtual (absolute)
           address. See https://refspecs.linuxfoundation.org/elf/elf.pdf, page
           1-21, "Symbol Values". *)
        Some Int64.(add sections.(st_shndx).sh_offset
                      (sub st_value sections.(st_shndx).sh_addr))

  let defines_symbol symbols symname =
    Option.is_some (find_symbol symbols symname)

  let read ic =
    seek_in ic 0;
    let identification = really_input_bytes ic 16 in
    let bitness =
      match Bytes.get identification 4 with
      | '\x01' -> B32
      | '\x02' -> B64
      | _ as c ->
          raise (Error (Unsupported ("ELFCLASS", Int64.of_int (Char.code c))))
    in
    let endianness =
      match Bytes.get identification 5 with
      | '\x01' -> LE
      | '\x02' -> BE
      | _ as c ->
          raise (Error (Unsupported ("ELFDATA", Int64.of_int (Char.code c))))
    in
    let d = {ic; bitness; endianness} in
    let header = read_header d in
    let sections = read_sections d header in
    let symbols = read_symbols d sections in
    let symbol_offset = symbol_offset sections symbols in
    let defines_symbol = defines_symbol symbols in
    {symbol_offset; defines_symbol}
end

module Mach_O = struct

  (* Reference:
     https://github.com/aidansteele/osx-abi-macho-file-format-reference *)

  let size_int = 4

  let header_size {bitness; _} =
    (match bitness with B64 -> 6 | B32 -> 5) * 4 + 2 * size_int

  type header =
    {
      ncmds: int;
      sizeofcmds: int;
    }

  let read_header d =
    let buf = load_bytes d 0L (header_size d) in
    let ncmds = get_uint "ncmds" d buf (8 + 2 * size_int) in
    let sizeofcmds = get_uint "sizeofcmds" d buf (12 + 2 * size_int) in
    {ncmds; sizeofcmds}

  type lc_symtab =
    {
      symoff: int32;
      nsyms: int;
      stroff: int32;
      strsize: int;
    }

  type load_command =
    | LC_SYMTAB of lc_symtab
    | OTHER

  let read_load_commands d {ncmds; sizeofcmds} =
    let buf = load_bytes d (Int64.of_int (header_size d)) sizeofcmds in
    let base = ref 0 in
    let mk _ =
      let cmd = get_uint32 d buf (!base + 0) in
      let cmdsize = get_uint "cmdsize" d buf (!base + 4) in
      let lc =
        match cmd with
        | 0x2l ->
            let symoff = get_uint32 d buf (!base + 8) in
            let nsyms = get_uint "nsyms" d buf (!base + 12) in
            let stroff = get_uint32 d buf (!base + 16) in
            let strsize = get_uint "strsize" d buf (!base + 20) in
            LC_SYMTAB {symoff; nsyms; stroff; strsize}
        | _ ->
            OTHER
      in
      base := !base + cmdsize;
      lc
    in
    Array.init ncmds mk

  type symbol =
    {
      n_name: string;
      n_type: int;
      n_value: int64;
    }

  let size_nlist d =
    8 + word_size d

  let read_symbols d load_commands =
    match
      (* Can it happen there be more than one LC_SYMTAB? *)
      array_find_map (function
          | LC_SYMTAB symtab -> Some symtab
          | _ -> None
        ) load_commands
    with
    | None -> [| |]
    | Some {symoff; nsyms; stroff; strsize} ->
        let strtbl = load_bytes d (uint64_of_uint32 stroff) strsize in
        let buf =
          load_bytes d (uint64_of_uint32 symoff) (nsyms * size_nlist d) in
        let size_nlist = size_nlist d in
        let mk i =
          let base = i * size_nlist in
          let n_name = name_at strtbl (get_uint "n_name" d buf (base + 0)) in
          let n_type = Bytes.get_uint8 buf (base + 4) in
          let n_value = get_word d buf (base + 8) in
          {n_name; n_type; n_value}
        in
        Array.init nsyms mk

  let fix symname =
    "_" ^ symname

  let find_symbol symbols symname =
    let f {n_name; n_type; _} =
      n_type land 0b1111 = 0b1111 (* N_EXT + N_SECT *) &&
      n_name = symname
    in
    array_find f symbols

  let symbol_offset symbols symname =
    let symname = fix symname in
    match find_symbol symbols symname with
    | None -> None
    | Some {n_value; _} -> Some n_value

  let defines_symbol symbols symname =
    let symname = fix symname in
    Option.is_some (find_symbol symbols symname)

  type magic =
    | MH_MAGIC
    | MH_CIGAM
    | MH_MAGIC_64
    | MH_CIGAM_64

  let read ic =
    seek_in ic 0;
    let magic = really_input_bytes ic 4 in
    let magic =
      match Bytes.get_int32_ne magic 0 with
      | 0xFEEDFACEl -> MH_MAGIC
      | 0xCEFAEDFEl -> MH_CIGAM
      | 0xFEEDFACFl -> MH_MAGIC_64
      | 0xCFFAEDFEl -> MH_CIGAM_64
      | _ -> (* should not happen *)
          raise (Error (Unrecognized (Bytes.to_string magic)))
    in
    let bitness =
      match magic with
      | MH_MAGIC | MH_CIGAM -> B32
      | MH_MAGIC_64 | MH_CIGAM_64 -> B64
    in
    let endianness =
      match magic, Sys.big_endian with
      | (MH_MAGIC | MH_MAGIC_64), false
      | (MH_CIGAM | MH_CIGAM_64), true -> LE
      | (MH_MAGIC | MH_MAGIC_64), true
      | (MH_CIGAM | MH_CIGAM_64), false -> BE
    in
    let d = {ic; endianness; bitness} in
    let header = read_header d in
    let load_commands = read_load_commands d header in
    let symbols = read_symbols d load_commands in
    let symbol_offset = symbol_offset symbols in
    let defines_symbol = defines_symbol symbols in
    {symbol_offset; defines_symbol}
end

module FlexDLL = struct

  (* Reference:
     https://docs.microsoft.com/en-us/windows/win32/debug/pe-format *)

  let header_size = 24

  type header =
    {
      e_lfanew: int64;
      number_of_sections: int;
      size_of_optional_header: int;
      _characteristics: int;
    }

  let read_header e_lfanew d buf =
    let number_of_sections = get_uint16 d buf 6 in
    let size_of_optional_header = get_uint16 d buf 20 in
    let _characteristics = get_uint16 d buf 22 in
    {e_lfanew; number_of_sections; size_of_optional_header; _characteristics}

  type optional_header_magic =
    | PE32
    | PE32PLUS

  type optional_header =
    {
      _magic: optional_header_magic;
      image_base: int64;
    }

  let read_optional_header d {e_lfanew; size_of_optional_header; _} =
    if size_of_optional_header = 0 then
      raise (Error (Unrecognized "SizeOfOptionalHeader=0"));
    let buf =
      load_bytes d Int64.(add e_lfanew (of_int header_size))
        size_of_optional_header
    in
    let _magic, image_base =
      match get_uint16 d buf 0 with
      | 0x10b -> PE32, uint64_of_uint32 (get_uint32 d buf 28)
      | 0x20b -> PE32PLUS, get_uint64 d buf 24
      | n ->
          raise (Error (Unsupported ("optional_header_magic", Int64.of_int n)))
    in
    {_magic; image_base}

  type section =
    {
      name: string;
      _virtual_size: int;
      virtual_address: int64;
      size_of_raw_data: int;
      pointer_to_raw_data: int64;
    }

  let section_header_size = 40

  let read_sections d
      {e_lfanew; number_of_sections; size_of_optional_header; _} =
    let buf =
      load_bytes d
        Int64.(add e_lfanew (of_int (header_size + size_of_optional_header)))
        (number_of_sections * section_header_size)
    in
    let mk i =
      let base = i * section_header_size in
      let name = name_at ~max_len:8 buf (base + 0) in
      let _virtual_size = get_uint "virtual_size" d buf (base + 8) in
      let virtual_address = uint64_of_uint32 (get_uint32 d buf (base + 12)) in
      let size_of_raw_data = get_uint "size_of_raw_data" d buf (base + 16) in
      let pointer_to_raw_data =
        uint64_of_uint32 (get_uint32 d buf (base + 20)) in
      {name; _virtual_size; virtual_address;
       size_of_raw_data; pointer_to_raw_data}
    in
    Array.init number_of_sections mk

  type symbol =
    {
      name: string;
      address: int64;
    }

  let load_section_body d {size_of_raw_data; pointer_to_raw_data; _} =
    load_bytes d pointer_to_raw_data size_of_raw_data

  let find_section sections sectname =
    array_find (function ({name; _} : section) -> name = sectname) sections

  (* We extract the list of exported symbols as encoded by flexlink, see
     https://github.com/alainfrisch/flexdll/blob/bd636def70d941674275b2f4b6c13a34ba23f9c9/reloc.ml
     #L500-L525 *)

  let read_symbols d {image_base; _} sections =
    match find_section sections ".exptbl" with
    | None -> [| |]
    | Some ({virtual_address; _} as exptbl) ->
        let buf = load_section_body d exptbl in
        let numexports =
          uint64_to_int "numexports" (get_word d buf 0)
        in
        let word_size = word_size d in
        let mk i =
          let address = get_word d buf (word_size * (2 * i + 1)) in
          let nameoff = get_word d buf (word_size * (2 * i + 2)) in
          let name =
            let off = Int64.(sub nameoff (add virtual_address image_base)) in
            name_at buf (uint64_to_int "exptbl name offset" off)
          in
          {name; address}
        in
        Array.init numexports mk

  let symbol_offset {image_base; _} sections symbols =
    match find_section sections ".data" with
    | None -> Fun.const None
    | Some {virtual_address; pointer_to_raw_data; _} ->
        fun symname ->
          begin match
            array_find (function {name; _} -> name = symname) symbols
          with
          | None -> None
          | Some {address; _} ->
              Some Int64.(add pointer_to_raw_data
                            (sub address (add virtual_address image_base)))
          end

  let defines_symbol symbols symname =
    Array.exists (fun {name; _} -> name = symname) symbols

  type machine_type =
    | IMAGE_FILE_MACHINE_ARM
    | IMAGE_FILE_MACHINE_ARM64
    | IMAGE_FILE_MACHINE_AMD64
    | IMAGE_FILE_MACHINE_I386

  let read ic =
    let e_lfanew =
      seek_in ic 0x3c;
      let buf = really_input_bytes ic 4 in
      uint64_of_uint32 (Bytes.get_int32_le buf 0)
    in
    LargeFile.seek_in ic e_lfanew;
    let buf = really_input_bytes ic header_size in
    let magic = Bytes.sub_string buf 0 4 in
    if magic <> "PE\000\000" then raise (Error (Unrecognized magic));
    let machine =
      match Bytes.get_uint16_le buf 4 with
      | 0x1c0 -> IMAGE_FILE_MACHINE_ARM
      | 0xaa64 -> IMAGE_FILE_MACHINE_ARM64
      | 0x8664 -> IMAGE_FILE_MACHINE_AMD64
      | 0x14c -> IMAGE_FILE_MACHINE_I386
      | n -> raise (Error (Unsupported ("MACHINETYPE", Int64.of_int n)))
    in
    let bitness =
      match machine with
      | IMAGE_FILE_MACHINE_AMD64
      | IMAGE_FILE_MACHINE_ARM64 -> B64
      | IMAGE_FILE_MACHINE_I386
      | IMAGE_FILE_MACHINE_ARM -> B32
    in
    let d = {ic; endianness = LE; bitness} in
    let header = read_header e_lfanew d buf in
    let opt_header = read_optional_header d header in
    let sections = read_sections d header in
    let symbols = read_symbols d opt_header sections in
    let symbol_offset = symbol_offset opt_header sections symbols in
    let defines_symbol = defines_symbol symbols in
    {symbol_offset; defines_symbol}
end

let read ic =
  seek_in ic 0;
  let magic = really_input_string ic 4 in
  match magic.[0], magic.[1], magic.[2], magic.[3] with
  | '\x7F', 'E', 'L', 'F' ->
      ELF.read ic
  | '\xFE', '\xED', '\xFA', '\xCE'
  | '\xCE', '\xFA', '\xED', '\xFE'
  | '\xFE', '\xED', '\xFA', '\xCF'
  | '\xCF', '\xFA', '\xED', '\xFE' ->
      Mach_O.read ic
  | 'M', 'Z', _, _ ->
      FlexDLL.read ic
  | _ ->
      raise (Error (Unrecognized magic))

let with_open_in fn f =
  let ic = open_in_bin fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> f ic)

let read filename =
  match with_open_in filename read with
  | t -> Ok t
  | exception End_of_file ->
      Result.Error Truncated_file
  | exception Error err ->
      Result.Error err

let defines_symbol {defines_symbol; _} symname =
  defines_symbol symname

let symbol_offset {symbol_offset; _} symname =
  symbol_offset symname
