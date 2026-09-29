(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

(* This module implements a reader/writer for COFF object files
   and libraries. *)

open Compat

module Buf : sig
  type t
  val create: unit -> t
  val length: t -> int
  val dump: out_channel -> t -> unit
  val string: t -> string -> unit
  val bytes: t -> bytes -> unit
  val int8: t -> int -> unit
  val int32: t -> int32 -> unit
  val int16: t -> int -> unit
  val lazy_int32: t -> int32 Lazy.t -> unit
  val patch_lazy_int32: t -> int -> int32 Lazy.t -> unit
  val future_int32: t -> int32 Lazy.t -> int32 ref
  val set_future: t -> int32 ref -> unit
end = struct
  type t = {
      mutable buf: bytes;
      mutable pos: int;
      mutable len: int;
      mutable patches: (unit -> unit) list;
    }

  let create () =
    { buf = Bytes.create 16;
      pos = 0;
      len = 16;
      patches = [] }

  let ensure b n =
    let len = ref b.len in
    let pos = b.pos in
    if n > Sys.max_string_length then
     failwith (Printf.sprintf "Cannot grow string buffer to len %i.\n" n);
    while n > !len do len := !len * 2 done;
    if !len > Sys.max_string_length then len := Sys.max_string_length;
    let nbuf = Bytes.create !len in
    Bytes.blit b.buf 0 nbuf 0 pos;
    b.buf <- nbuf;
    b.len <- !len

  let int8 b x =
    let pos = b.pos in
    if pos = b.len then begin
      let nlen = b.len * 2 in
      let nbuf = Bytes.create nlen in
      Bytes.blit b.buf 0 nbuf 0 pos;
      b.buf <- nbuf;
      b.len <- nlen
    end;
    Bytes.set b.buf pos (Char.chr (x land 0xff));
    b.pos <- succ pos

  let int16 b x =
    int8 b x;
    int8 b (x asr 8)

  let length b = b.pos

  let patch b =
    List.iter (fun f -> f ()) b.patches;
    b.patches <- []

  let add_patch b f =
    b.patches <- f :: b.patches

  let dump oc b =
    patch b;
    output_bytes oc (Bytes.sub b.buf 0 b.pos)

  let string b s =
    let l = String.length s in
    let r = b.pos + l in
    if r > b.len then ensure b r;
    Bytes.blit_string s 0 b.buf b.pos l;
    b.pos <- r

  let bytes b s =
    let l = Bytes.length s in
    let r = b.pos + l in
    if r > b.len then ensure b r;
    Bytes.blit s 0 b.buf b.pos l;
    b.pos <- r

  let blit_int32 s pos i =
    Bytes.set s pos (Char.chr ((Int32.to_int i) land 0xff));
    Bytes.set s (pos+1) (Char.chr ((Int32.to_int (Int32.shift_right i 8)) land 0xff));
    Bytes.set s (pos+2) (Char.chr ((Int32.to_int (Int32.shift_right i 16)) land 0xff));
    Bytes.set s (pos+3) (Char.chr ((Int32.to_int (Int32.shift_right i 24)) land 0xff))

  let int32 b i =
    let r = b.pos + 4 in
    if r > b.len then ensure b r;
    blit_int32 b.buf b.pos i;
    b.pos <- r

  let lazy_int32 b i =
    let pos = b.pos in
    let r = b.pos + 4 in
    if r > b.len then ensure b r;
    b.pos <- r;
    add_patch b (fun () -> blit_int32 b.buf pos (Lazy.force i))

  let patch_lazy_int32 b pos i =
    add_patch b (fun () -> blit_int32 b.buf pos (Lazy.force i))

  let future_int32 b ofs =
    let r = ref 0l in
    lazy_int32 b (lazy (Int32.add !r (Lazy.force ofs)));
    r

  let set_future b r =
    r := Int32.of_int (length b)
end


(* Internal representation of COFF object files *)

type symbol = {
  mutable sym_pos: int;
  mutable sym_name: string;
  mutable value: int32;
  mutable section: [ `Num of int | `Section of section ];
  stype: int;
  storage: int;
  auxn: int;
  mutable auxs: bytes;
  mutable extra_info: [ `Alias of symbol | `Section of section | `None ];
}

and reloc = {
  addr: int32;
  symbol: symbol;
  rtype: int;
}

and section = {
  mutable sec_pos: int;
  mutable sec_name: string;
  mutable vsize: int32;
  mutable vaddress: int32;
  mutable data:
      [ `String of bytes | `Uninit of int
    | `Buf of Buf.t list
    | `Sxdata of symbol array
];
  mutable relocs: reloc list;
  mutable sec_opts: int32;
}

type coff = {
  bigobj: bool;
  obj_name: string;
  machine: int;
  date: int32;
  mutable sections: section list;
  mutable symbols: symbol list;
  opts: int;
}

(* Misc *)


let (++) = Int32.add
let (&&&) = Int32.logand
let (|||) = Int32.logor
let (>>>) = Int32.shift_right_logical

let filter f l =
  let rec aux accu = function
    | [] -> accu
    | hd::tl -> if f hd then aux (hd::accu) tl else aux accu tl
  in
  aux [] l

let is_zero s =
  try
    for i = 0 to String.length s - 1 do
      if s.[i] <> '\000' then raise Exit
    done;
    true
  with Exit -> false

(* Tools to read/write binary data *)

let mk_int32 a b c d =
  let a = Int32.of_int a
  and b = Int32.shift_left (Int32.of_int b) 8
  and c = Int32.shift_left (Int32.of_int c) 16
  and d = Int32.shift_left (Int32.of_int d) 24 in
  a ++ b ++ c ++ d

let read ic pos len =
  if len = 0 then Bytes.of_string ""
  else (
    seek_in ic pos;
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    buf
  )

let read_str ic pos len =
  Bytes.to_string (read ic pos len)

let int32 buf loc =
  mk_int32
    (Char.code (Bytes.get buf loc)) (Char.code (Bytes.get buf (loc + 1)))
    (Char.code (Bytes.get buf (loc + 2))) (Char.code (Bytes.get buf (loc + 3)))

let emit_int32 oc i =
  output_byte oc (Int32.to_int (i &&& 0xffl));
  output_byte oc (Int32.to_int ((i >>> 8) &&& 0xffl));
  output_byte oc (Int32.to_int ((i >>> 16) &&& 0xffl));
  output_byte oc (Int32.to_int ((i >>> 24) &&& 0xffl))

let patch_int32 oc pos i =
  let bak = pos_out oc in
  seek_out oc pos;
  emit_int32 oc i;
  seek_out oc bak

let emit_int16 oc i =
  output_byte oc (i land 0xff);
  output_byte oc (i lsr 8)

let emit_int8 = output_byte

let int32_ buf loc = Int32.to_int (int32 buf loc)

let int16 buf loc = Char.code (Bytes.get buf loc) + (Char.code (Bytes.get buf (loc + 1))) lsl 8

let int8 buf loc = Char.code (Bytes.get buf loc)

let strz buf loc ?(max=Bytes.length buf - loc) c =
  let i =
    try Bytes.index_from buf loc c
    with Not_found -> Bytes.length buf in
  Bytes.sub_string buf loc (min max (i - loc))

let emit_zero oc n =
  for _i = 1 to n do output_char oc '\000' done

let delayed_ptr oc f =
  let bak = pos_out oc in
  emit_int32 oc 0l;
  (fun () ->
     patch_int32 oc bak (Int32.of_int (pos_out oc));
     f ()
  )

let copy_data ic pos oc len =
  (* TODO: bufferized copy when len > threshold *)
  output_bytes oc (read ic pos len)

(* Human readable pretty-printers *)

let flags x =
  let b = Buffer.create 16 in
  for i = 0 to 31 do
    let m = Int32.shift_left 1l i in
      if m &&& x <> 0l then
        Printf.bprintf b "0x%08lx " m
  done;
  Buffer.contents b

let rec dump ic pos len w =
  if len = 0 then (Printf.printf "---\n"; flush stdout)
  else let l = min len w in
  let b = read ic pos l in
  Printf.printf "%08x: " pos;
  for i = 0 to l - 1 do Printf.printf "%02x " (Char.code (Bytes.get b i)) done;
  for _i = l to w - 1 do Printf.printf "   " done;
  Printf.printf " ";
  for i = 0 to l - 1 do match Bytes.get b i with
    | '\032'..'\127' as c -> print_char c
    | _ -> print_char '.'
  done;
  Printf.printf "\n%!";
  dump ic (pos + l) (len - l) w

module Symbol = struct
  let counter = ref 0
  let gen_sym () = incr counter; Printf.sprintf "_DREL%i" !counter

  let empty () =
    { sym_pos = (-1); sym_name = gen_sym(); value = 0l;
      section = `Num 0; storage = 0; stype = 0; auxn = 0; auxs = Bytes.of_string "";
      extra_info = `None }

  let intern sec addr =
    { (empty ()) with section = `Section sec; storage = 3; value = addr }

  let named_intern name sec addr =
    { (empty ()) with sym_name = name; section = `Section sec; storage = 3; value = addr }

  let label name sec addr =
    { (empty ()) with sym_name = name; section = `Section sec; storage = 6; value = addr }

  let export name sec addr =
    { (empty ()) with sym_name = name; value = addr;
        section = `Section sec; storage = 2 }

  let extern name =
    { (empty ()) with sym_pos = (-1); sym_name = name;
        section = `Num 0; storage = 2 }


  let get ~bigobj strtbl ic pos =
    let size, sec_get = if bigobj then (20, int32_) else (18, int16) in
    let buf = read ic pos size in
    let auxn = int8 buf (size - 1) in
    { sym_pos = (-1);
      sym_name =
        (if int32_ buf 0 <> 0 then strz buf 0 ~max:8 '\000'
         else strtbl (int32_ buf 4));
      value = int32 buf 8;
      section = `Num (sec_get buf 12);
      stype = int16 buf (size - 4);
      storage = int8 buf (size - 2);
      auxn = auxn;
      auxs = read ic (pos + size) (size * auxn);
      extra_info = `None;
    }

  let is_extern = function
    | { storage = 2; section = `Num 0; value = 0l } -> true
    | _ -> false

  let is_export = function
    | { storage = 2; section = `Section _ } -> true
    | { storage = 2; section = `Num 0; value = 0l } -> false
    | { storage = 2; section = `Num 0 } -> true
    | _ -> false

  let is_defin = function
    | { storage = 2; section = `Section _ } -> true
    | { storage = 2; section = `Num 0; value = 0l } -> false
    | { storage = 2; section = `Num (0 | 0xffff) } -> true
    | _ -> false

  let dump s =
    Printf.printf " %s: " s.sym_name;
    if s.stype <> 0 then Printf.printf "(typ:%i) " s.stype;
    let sect = match s.section with
      | `Num 0xffff -> "absolute"
      | `Num 0xfffe -> "debug"
      | `Num i -> string_of_int i
      | `Section s -> Printf.sprintf "%S" s.sec_name in
    let storage = match s.storage with
      | 2 -> "extern"
      | 3 -> "static"
      | 6 -> "label"
      | 103 -> "srcfile"
      | n -> string_of_int n in
    match s with
      | { storage = 6 } ->
          Printf.printf "label %s @ 0x%08lx\n" sect s.value
      | { storage = 2; section = `Section _ } ->
          Printf.printf "export %s @ 0x%08lx\n" sect s.value
      | { storage = 2; section = `Num 0; value = 0l } ->
          Printf.printf "extern\n"
      | { storage = 2; section = `Num 0; value = n } ->
          Printf.printf "common symbol, size %ld\n" n
      | { storage = 3; value = 0l; auxn = auxn } when auxn > 0 ->
          Printf.printf "section %s, num %i, select %i\n" sect
            (int16 s.auxs 12)
            (int8 s.auxs 14)
      | { storage = 3 } ->
          Printf.printf "static %s @ 0x%08lx\n" sect s.value
      | { storage = 103 } ->
          Printf.printf "filename %s\n" (strz s.auxs 0 '\000')
      | { storage = 105 } ->
          Printf.printf "weak ext\n"
      | _ ->
          Printf.printf
            "value=0x%08lx, sect=%s, storage=%s, aux=%S\n"
            s.value sect storage (Bytes.to_string s.auxs)

  let put ~bigobj strtbl oc s =
    let sec_get, sec_put, type_ofs =
      if bigobj then
        (int32_, (fun oc i -> emit_int32 oc (Int32.of_int i)), 16)
      else
        (int16, emit_int16, 14) in
    if String.length s.sym_name <= 8
    then (output_string oc s.sym_name;
          emit_zero oc (8 - String.length s.sym_name))
    else (emit_zero oc 4; emit_int32 oc (strtbl s.sym_name));
    emit_int32 oc s.value;
    let sec = match s.section with
      | `Num i -> i
      | `Section sec when sec.sec_pos <= 0 ->
          failwith (Printf.sprintf
                      "Cannot emit section for symbol %s" s.sym_name)
      | `Section sec -> sec.sec_pos in
    sec_put oc sec;
    emit_int16 oc s.stype;
    emit_int8 oc s.storage;
    emit_int8 oc s.auxn;
    match s with
      | { storage = 105; extra_info = `Alias s' } when s'.sym_pos >= 0 ->
          (* weak ext *)
          emit_int32 oc (Int32.of_int s'.sym_pos);
          output_bytes oc (Bytes.sub s.auxs 4 (Bytes.length s.auxs - 4))
      | { storage = 3; extra_info = `Section s' } when int8 s.auxs 14 = 5 (* IMAGE_COMDAT_SELECT_ASSOCIATIVE *) ->
          (* section def *)
          output_bytes oc (Bytes.sub s.auxs 0 12);
          sec_put oc s'.sec_pos;
          output_bytes oc (Bytes.sub s.auxs type_ofs (Bytes.length s.auxs - type_ofs))
      | { storage = 3; extra_info = `Section s' } ->
          (* section def *)
          Printf.eprintf "!!! section symbol not supported (symbol: %s -> section:%s)\n%!" s.sym_name s'.sec_name;
          Printf.eprintf "length = %i\n" (int32_ s.auxs 0);
          Printf.eprintf "# reloc = %i\n" (int16 s.auxs 4);
          Printf.eprintf "# linenum = %i\n" (int16 s.auxs 6);
          Printf.eprintf "checksum = %i\n" (int32_ s.auxs 8);
          Printf.eprintf "idx = %i\n" (int16 s.auxs 12);
          Printf.eprintf "sel = %i\n" (int8 s.auxs 14);
          assert false
      | _ ->
          if s.storage = 105 then assert (sec_get s.auxs 12 = 0);
          output_bytes oc s.auxs
end

module Reloc = struct
  let abs machine sec addr sym =
    let rtype =
      match machine with
      | `x86 -> 0x06
      | `x64 -> 0x01
    in
    sec.relocs <- { addr = addr; symbol = sym; rtype = rtype } :: sec.relocs

  let rel32 machine sec addr sym =
    let rtype =
      match machine with
      | `x86 -> 0x14
      | `x64 -> 0x04
    in
    sec.relocs <- { addr = addr; symbol = sym; rtype = rtype } :: sec.relocs

  let get symtbl va ic base =
    let buf = read ic base 10 in
    { addr = Int32.sub (int32 buf 00) va;
      symbol = (try match symtbl.(int32_ buf 4) with Some s -> s
                  | None -> assert false
                with _exn -> assert false);
      rtype = int16 buf 8
    }

  let dump x =
    Printf.printf " Reloc %ld -> %s, type 0x%04x\n"
      x.addr x.symbol.sym_name x.rtype

  let put oc x =
    emit_int32 oc x.addr;
    if x.symbol.sym_pos < 0 then
      failwith (Printf.sprintf
                  "Cannot emit relocation for symbol %s\n"
                  x.symbol.sym_name);
    emit_int32 oc (Int32.of_int x.symbol.sym_pos);
    emit_int16 oc x.rtype
end

module Section = struct
  let create name flags = {
    sec_pos = (-1); sec_name = name; data = `String (Bytes.of_string ""); relocs = [];
    vaddress = 0l; vsize = 0l; sec_opts = flags;
  }

  let nreloc_ovfl = 0x01000000l

  let get filebase strtbl symtbl ic base =
    let buf = read ic base 40 in
    let size = int32_ buf 16 in
    let name =
      if Bytes.get buf 0 = '/'
      then strtbl (int_of_string (strz buf 1 ~max:7 '\000'))
      else strz buf 0 ~max:8 '\000'
    in
    let va = int32 buf 12 in

    let nrelocs = int16 buf 32 in
    let more_relocs = int32 buf 36 &&& nreloc_ovfl <> 0l in
    let base_relocs = filebase + int32_ buf 24 in
    let base_relocs, nrelocs =
      if more_relocs then begin
        let buf_first_reloc = read ic base_relocs 4 in
        let n = int32_ buf_first_reloc 0 in
        base_relocs + 10, n - 1
      end else
        base_relocs, nrelocs
    in

    let relocs =
      let r = ref [] in
      for i = 0 to nrelocs - 1 do
        r := Reloc.get symtbl va ic (base_relocs + 10 * i) :: !r
      done;
      !r
    in

    let data =
      if name = ".sxdata" then
        let s = read ic (filebase + int32_ buf 20) size in
        `Sxdata
          (Array.init (size /4)
             (fun i -> match symtbl.(int32_ s (i * 4)) with None -> assert false | Some s -> s))
      else if int32_ buf 20 = 0 then `Uninit size
      else `String (read ic (filebase + int32_ buf 20) size)
    in

    { sec_pos = (-1);
      sec_name = name;
      vsize = int32 buf 8;
      vaddress = 0l;
      data = data;
      relocs = relocs;
      sec_opts = int32 buf 36
    }

  let dump x =
    Printf.printf "Section %s (0x%08lx: %s)\n"
      x.sec_name
      x.sec_opts
      (flags x.sec_opts);
    List.iter Reloc.dump x.relocs

  let size s =
    match s.data with
    | `String s -> Bytes.length s
    | `Uninit len -> len
    | `Buf bufs -> List.fold_left (fun s b -> s + Buf.length b) 0 bufs
    | `Sxdata syms -> Array.length syms * 4

  let put strtbl oc x =
    let name =
      if String.length x.sec_name <= 8
      then x.sec_name
      else Printf.sprintf "/%ld" (strtbl x.sec_name)
    in
    output_string oc name; emit_zero oc (8 - String.length name);
    emit_int32 oc x.vsize;
(*    assert(x.vaddress = 0l); *)
    emit_int32 oc x.vaddress;
    emit_int32 oc (Int32.of_int (size x));
    let send_data = match x.data with
      | `String s ->
          delayed_ptr oc (fun () -> output_bytes oc s)
      | `Uninit _len ->
          emit_int32 oc 0l; (fun () -> ())
      | `Buf bufs ->
          delayed_ptr oc (fun () -> List.iter (Buf.dump oc) bufs)
      | `Sxdata syms ->
          delayed_ptr oc
            (fun () ->
              Array.iter (fun sym -> assert(sym.sym_pos >= 0); emit_int32 oc (Int32.of_int sym.sym_pos)) syms
            )
    in

    let nrelocs = List.length x.relocs in
    let many_relocs = nrelocs > 0xffff in

    let send_reloc =
      if x.relocs = [] then (emit_int32 oc 0l; fun () -> ())
      else delayed_ptr oc
          (fun () ->
            if many_relocs then begin
              emit_int32 oc (Int32.of_int (nrelocs + 1));
              (* +1 because this slot counts as well! *)
              emit_int32 oc 0l;
              emit_int16 oc 0
            end;
            List.iter (Reloc.put oc) x.relocs
          )
    in

    emit_int32 oc 0l;
    if many_relocs then emit_int16 oc 0xffff
    else emit_int16 oc nrelocs;
    emit_int16 oc 0;
    let sec_opts = if many_relocs then x.sec_opts ||| nreloc_ovfl else x.sec_opts &&& Int32.lognot nreloc_ovfl in
    emit_int32 oc sec_opts;

    send_data, send_reloc
end

module FileHeader = struct
  type header = {
    hdrsize : int;
    machine : int;
    seccount : int;
    date: int32;
    symtable : int;
    symcount : int;
    opthdr : int;
    opts : int;

    symsize : int;
    bigobj: bool;
  }

  let bigobj_classid =
    "\xC7\xA1\xBA\xD1\xEE\xBA\xA9\x4B\xAF\x20\xFA\xF6\x6A\xA4\xDC\xB8"

  let get_image_file_header ic ofs =
    let hdrsize = 20 in
    let buf = read ic ofs hdrsize in
    let machine = int16 buf 0 in
    let seccount = int16 buf 2 in
    let date = int32 buf 4 in
    let symtable = int32_ buf 8 in
    let symcount = int32_ buf 12 in
    let opthdr = int16 buf 16 in
    let opts = int16 buf 18 in
    let symsize = 18 in
    let bigobj = false in
    { hdrsize = hdrsize; machine = machine; seccount = seccount; date = date;
      symtable = symtable; symcount = symcount; opthdr = opthdr; opts = opts;
      symsize = symsize; bigobj = bigobj }

  let get_header_bigobj ic ofs =
    let hdrsize = 56 in
    let buf = read ic ofs hdrsize in
    let machine = int16 buf 6 in
    let date = int32 buf 8 in
    let opts = int32_ buf 32 in
    let seccount = int32_ buf 44 in
    let symtable = int32_ buf 48 in
    let symcount = int32_ buf 52 in
    let symsize = 20 in
    let opthdr = 0 in
    let bigobj = true in
    { hdrsize = hdrsize; machine = machine; seccount = seccount; date = date;
      symtable = symtable; symcount = symcount; opthdr = opthdr; opts = opts;
      symsize = symsize; bigobj = bigobj }

  let get ic ofs =
    let buf = read ic ofs 6 in
    let sig1 = int16 buf 0 in
    let sig2 = int16 buf 2 in
    if sig1 = 0 && sig2 = 0xFFFF then
      let version = int16 buf 4 in
      if version = 0 then
        `IMPORT_OBJECT_HEADER (* names from winnt.h *)
      else if version = 1 then
        `ANON_OBJECT_HEADER
      else (* version >= 2 *)
        `ANON_OBJECT_HEADER_BIGOBJ (get_header_bigobj ic ofs)
    else
      `IMAGE_FILE_HEADER (get_image_file_header ic ofs)
end

module Coff = struct
  let add_section x sect =
    x.sections <- sect :: x.sections
  let add_symbol x sym =
    x.symbols <- sym :: x.symbols

  let create machine =
    let machine =
      match machine with
      | `x64 -> 0x8664
      | `x86 -> 0x14c
    in
    { bigobj = false;
      obj_name = "generated";
      machine = machine; date = 0x4603de0el;
      sections = []; symbols = []; opts = 0 }

  let parse_directives s =
    let rec find_end i =
      if i = 0 || s.[i - 1] <> '\000' then i
      else find_end (i - 1)
    in
    let l = find_end (String.length s) in
    let rec aux0 i = if i = l then [] else match s.[i] with
      | ' ' -> aux0 (i+1)
      | '-' | '/' -> aux1 (i+1) (i+1)
      | _ -> raise Exit
    and aux1 i0 i = if i = l then (String.sub s i0 (i - i0), [])::[]
    else match s.[i] with
      | 'a'..'z' | 'A'..'Z' | '-' -> aux1 i0 (i+1)
      | ' ' -> (String.sub s i0 (i - i0), []) :: aux0 (i+1)
      | ':' -> aux2 (String.sub s i0 (i - i0)) [] (i+1)
      | _   -> raise Exit
    and aux2 cmd args i = match s.[i] with
      | '"' -> aux3 cmd args (i+1) (i+1)
      | _   -> aux4 cmd args i i
    and aux3 cmd args i0 i = match s.[i] with
      | '"' -> aux5 cmd (String.sub s i0 (i - i0) :: args) (i+1)
      | _   -> aux3 cmd args i0 (i+1)
    and aux4 cmd args i0 i =
      if i = l then (cmd, String.sub s i0 (i - i0) :: args)::[]
      else match s.[i] with
        | ' ' -> (cmd, String.sub s i0 (i - i0) :: args) :: aux0 (i+1)
        | ',' -> aux2 cmd (String.sub s i0 (i - i0) :: args) (i+1)
        | _   -> aux4 cmd args i0 (i+1)
    and aux5 cmd args i =
      if i = l then (cmd, args) :: []
      else match s.[i] with
      | ' ' -> (cmd,args) :: aux0 (i+1)
      | ',' -> aux2 cmd args (i+1)
      | _   -> raise Exit
    in
    try List.map (fun (cmd,args) -> (cmd,List.rev args)) (aux0 0)
    with _ ->
      failwith (Printf.sprintf "Cannot parse directive: %s\n" s)

  let directives obj =
    try
      let sec = List.find (fun s -> s.sec_name = ".drectve") obj.sections in
      match sec.data with
        | `String s -> parse_directives (Bytes.to_string s)
        | `Uninit _ | `Sxdata _ -> []
        | `Buf _ -> assert false
    with Not_found -> []

  let get ic ofs base h name =
    let symtable = base + h.FileHeader.symtable in

    (* the string table *)
    let strtbl =
      let pos = symtable + h.FileHeader.symsize * h.FileHeader.symcount in
      if pos = 0 then fun _ -> assert false
      else
        let len = int32_ (read ic pos 4) 0 in
        let data = read ic pos len in
        fun i -> strz data i '\000'
    in


    (* the symbol table *)
    let symbols,symtbl =
      let tbl = Array.make h.FileHeader.symcount None in
      let rec fill accu i =
        if i = h.FileHeader.symcount then List.rev accu
        else let s = Symbol.get ~bigobj:h.FileHeader.bigobj strtbl ic (symtable + h.FileHeader.symsize * i) in
        (try tbl.(i) <- Some s
         with Invalid_argument _ -> assert false);
        fill (s :: accu) (i + 1 + s.auxn) in
      fill [] 0, tbl
    in

    (* the sections *)
    let sectable = ofs + h.FileHeader.hdrsize + h.FileHeader.opthdr in
    let sections =
      Array.init h.FileHeader.seccount
        (fun i -> Section.get base strtbl symtbl ic (sectable + 40 * i))
    in

    (* remove .bf/.ef/.lf symbols *)
    let symbols =
      List.filter (function { storage = 101 } -> false | _ -> true)
        symbols in
    List.iter
      (fun s ->
         (match s with
            | { storage = 105; auxn = 1 } ->
                (* weak ext *)
                (try match symtbl.(Int32.to_int (int32 s.auxs 0)) with
                   | Some s' -> s.extra_info <- `Alias s'
                   | None -> assert false
                 with Invalid_argument _ -> assert false);
            | { storage = 3; stype = 0; auxn = auxn } when auxn > 0 ->
                (* section def *)
                let num = int16 s.auxs 12 in
                let sel = int8 s.auxs 14 in
                if sel = 5 && num > 0 then
                  (try s.extra_info <- `Section sections.(num - 1)
                   with Invalid_argument _ ->
                     Printf.eprintf "** section %i / %i (%s)\n" num
                       (Array.length sections) s.sym_name;
                     assert false);
            | { storage = 103 }
            | { auxn = 0 } -> ()
            | { storage = (2|3); stype = 0x20; auxn = 1; auxs = auxs } ->
                (* Remove extra information for function symbols *)
                s.auxs <- Bytes.make (Bytes.length auxs) '\000'
            | _ ->
                Symbol.dump s;
                Printf.printf "aux=%S\n%!" (Bytes.to_string s.auxs);
                assert false);
         (match s.section with
            | `Num i when i > 0 && i <= Array.length sections ->
                assert (i <= Array.length sections);
                (try s.section <- `Section sections.(i - 1)
                 with Invalid_argument _ -> assert false);
            | _ -> ()))
      symbols;

    { bigobj = h.FileHeader.bigobj;
      obj_name = name;
      machine = h.FileHeader.machine;
      sections = Array.to_list sections;
      date = h.FileHeader.date;
      symbols = symbols;
      opts = h.FileHeader.opts;
    }

  let aliases x =
    let a = ref [] in
    List.iter
      (fun s ->
         match s.extra_info with
           | `Alias s' -> a := (s.sym_name,s'.sym_name) :: !a
           | _ -> ()
      )
      x.symbols;
    !a



  let dump x =
    Printf.printf "machine: 0x%x\n" x.machine;
    Printf.printf "date:    0x%lx\n" x.date;
    Printf.printf "opts:    0x%x\n" x.opts;
    List.iter Symbol.dump x.symbols;
    List.iter Section.dump x.sections;
    flush stdout

  let put oc x =
    let () =
      let no = ref 0 in
      List.iter
        (fun s -> incr no; assert(s.sec_pos < 0); s.sec_pos <- !no)
        x.sections
    in

    if x.bigobj then begin
      emit_int16 oc 0; (* Sig1 *)
      emit_int16 oc 0xffff; (* Sig2 *)
      emit_int16 oc 2; (* Version *)
      emit_int16 oc x.machine;
      emit_int32 oc x.date;
      output_string oc FileHeader.bigobj_classid;
      emit_int32 oc 0l;
      emit_int32 oc (Int32.of_int x.opts);
      emit_int32 oc 0l;
      emit_int32 oc 0l;
      emit_int32 oc (Int32.of_int (List.length x.sections));
    end
    else begin
      emit_int16 oc x.machine;
      emit_int16 oc (List.length x.sections);
      emit_int32 oc x.date;
    end;

    let strbuf = Buffer.create 1024 in
    let strtbl s =
      let pos = Buffer.length strbuf in
      Buffer.add_string strbuf s;
      Buffer.add_char strbuf '\000';
      Int32.of_int (4 + pos)
    in

    let patch_sym =
      delayed_ptr oc
        (fun () -> List.iter (Symbol.put ~bigobj:x.bigobj strtbl oc) x.symbols)
    in
    let nbsym =
      let no = ref 0 in
      List.iter
        (fun s ->
           assert(s.sym_pos < 0);
           s.sym_pos <- !no;
           no := !no + 1 + s.auxn
        )
        x.symbols;
      !no
    in
    emit_int32 oc (Int32.of_int nbsym);
    if not x.bigobj then begin
      emit_int16 oc 0;
      emit_int16 oc x.opts;
    end;

    let sects =
      List.map (Section.put strtbl oc) x.sections in
    List.iter
      (fun (data,relocs) -> data (); relocs ())
      sects;

    patch_sym ();

    emit_int32 oc (Int32.of_int (Buffer.length strbuf + 4));
    Buffer.output_buffer oc strbuf;

    List.iter (fun s -> s.sym_pos <- 0) x.symbols;
    List.iter (fun s -> s.sec_pos <- 0) x.sections;
    ()
end

module Import = struct
  let read ic pos size =
    let buf = read ic pos size in
    let w = int16 buf 18 in
    let name = strz buf 20 '\000' in
(*    Printf.printf "Import header. Version = %i\n" (int16 buf 4);
    Printf.printf " machine     = 0x%x\n" (int16 buf 6);
    Printf.printf " time stamp  = 0x%lx\n" (int32 buf 8);
    Printf.printf " size data   = %ld\n" (int32 buf 12);
    Printf.printf " ord/hint    = %i\n" (int16 buf 16);
    Printf.printf " type        = %i\n" (w land 0b11);
    Printf.printf " name type   = %i\n" ((w land 0b11100) lsr 2);
    Printf.printf " symbol      = %s\n" name;
    Printf.printf " DLL         = %s\n"
      (strz buf (21 + String.length name) '\000'); *)
    name, w

end

module Lib = struct
  let magic_lib = "!<arch>\n"

  let read_lib ic libname =
    let strtbl = ref (Bytes.of_string "") in
    let imports = ref [] and objects = ref [] in
    let obj size name =
(*      Printf.printf "-> %s (size %i)\n" name size;  *)
      let pos = pos_in ic in
      match FileHeader.get ic pos with
      | `IMPORT_OBJECT_HEADER ->
        if size > 18 then
          imports := Import.read ic pos size :: !imports
      | `ANON_OBJECT_HEADER_BIGOBJ h
      | `IMAGE_FILE_HEADER h ->
        objects := (name,
                    Coff.get ic pos pos h
                      (Printf.sprintf "%s(%s)" libname name)) :: !objects
      | `ANON_OBJECT_HEADER ->
        () (* ignore *)
    in
    let rec read_member () =
      let buf = read ic (pos_in ic) 60 in
      let base = pos_in ic in
      let size = int_of_string (strz (Bytes.sub buf 48 10) 0 ' ') in
      let name = strz (Bytes.sub buf 0 16) 0 ' ' in
      begin match name with
        | "/" | "" -> ()
        | "//" ->
            let tbl = read ic (pos_in ic) size in
            (* GNU-produced archive files are supposed to be printable if the
               archive members contain text, so the entries in the list are
               newline-padded, not null-padded. Let's fix that here. *)
            for i = 0 to Bytes.length tbl - 1 do
              if Bytes.get tbl i = '\n' then
                let i = if i > 0 && Bytes.get tbl (i-1) = '/' then i-1 else i in
                Bytes.set tbl i '\000'
            done;
            strtbl := tbl
        | s when s.[0] = '/' ->
            let ofs =
              try Scanf.sscanf s "/%u%!" (fun x -> x)
              with Scanf.Scan_failure _
                 | Failure _
                 | End_of_file -> -1
            in
              (* Ignore special sections which we don't know about *)
              if ofs >= 0 then
                obj size (strz !strtbl ofs '\000')
        | s when s.[String.length s - 1] = '/' ->
            let s = String.sub s 0 (String.length s - 1) in
            obj size s
        | s ->
            Printf.ksprintf failwith "Cannot parse archive member %s" s
      end;
      seek_in ic (base + size + size mod 2);
      read_member ()
    in
    (try read_member () with End_of_file -> ());
    !objects,!imports

  let is_lib ic =
    in_channel_length ic >= String.length magic_lib
    && read_str ic 0 (String.length magic_lib) = magic_lib

  let obj_ofs ic =
    try
      let b = read ic 0x3c 4 in
      let ofs = int32_ b 0 in
      if read_str ic ofs 4 = "PE\000\000" then ofs + 4
      else 0
    with _exn ->
      0

  let is_dll filename =
    let ic = open_in_bin filename in
    let ofs = obj_ofs ic in
    close_in ic;
    ofs > 0

  let read filename =
    let ic = open_in_bin filename in
    try
(*      let t0 = Unix.gettimeofday () in
      Printf.printf "Reading %s...%!" filename; *)
      let r =
        if is_lib ic then `Lib (read_lib ic filename)
        else
          let ofs = obj_ofs ic in
          let h = match FileHeader.get ic ofs with
          | `ANON_OBJECT_HEADER_BIGOBJ h
          | `IMAGE_FILE_HEADER h -> h
          | `IMPORT_OBJECT_HEADER
          | `ANON_OBJECT_HEADER -> assert false in
          `Obj (Coff.get ic ofs 0 h filename)
      in
(*      let t1 = Unix.gettimeofday () in
      Printf.printf "  Done  (%f ms)\n%!" (t1 -. t0);  *)
      close_in ic;
      r
    with exn ->
      close_in ic;
      raise exn

  let read filename =
    try
      read filename
    with exn ->
      failwith
        (Printf.sprintf "Error while reading %s: %s"
           filename (Printexc.to_string exn))

end


module Stacksize = struct
  let set_stack_reserve filename size_of_stack_reserve =
    let filename =
      if not (Sys.file_exists filename) && (Sys.file_exists (filename ^ ".exe")) then filename ^ ".exe"
      else filename
    in
    let ic = open_in_bin filename in
    (* Read e_lfanew from the IMAGE_DOS_HEADER (see winnt.h) *)
    let pe_signature_offset = int32_ (read ic 0x3c 4) 0 in
    let pe_signature = read_str ic pe_signature_offset 4 in
    assert(pe_signature = "PE\000\000");
    let coff_file_header = read ic (pe_signature_offset + 4) 20 in
    let size_of_optional_header = int16 coff_file_header 16 in
    let optional_header = read ic (pe_signature_offset + 4 + 20) size_of_optional_header in
    let pe_format =
      match int16 optional_header 0 with
      | 0x10b -> `PE32
      | 0x20b -> `PE32plus
      | magic -> Printf.ksprintf failwith "Cannot determine PE format (magic = %x)." magic
    in
    let size_of_stack_reserve_offset = pe_signature_offset + 4 + 20 + 72 in
    close_in ic;

    let oc = open_out_gen [Open_wronly; Open_binary] 0x777 filename in
    seek_out oc size_of_stack_reserve_offset;
    emit_int32 oc size_of_stack_reserve;
    if pe_format = `PE32plus then emit_int32 oc 0l;
    close_out oc
end
