(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

(* Create a DLL from a set of "closed" COFF files (no imported symbol). *)

include (struct
open Compat
open Coff

let (&&&) = Int32.logand
let (|||) = Int32.logor
let (<<<) = Int32.shift_left

let read_int32 s i =
  Int32.of_int (Char.code (Bytes.get s i)) |||
  (Int32.of_int (Char.code (Bytes.get s (i+1))) <<< 8) |||
  (Int32.of_int (Char.code (Bytes.get s (i+2))) <<< 16) |||
  (Int32.of_int (Char.code (Bytes.get s (i+3))) <<< 24)

let align x n =
  let k = Int32.rem x n in
  if k = 0l then x
  else Int32.add x (Int32.sub n k)

let discard_section s =
  let opts = s.sec_opts in
  opts &&&
  (0x00000200l (* Info section (.drectve) *)
 ||| 0x00000800l (* Remove *)
 ||| 0x02000000l) <> 0l (* Discardable *)

let sect_data s =
  match s.data with
  | `String data -> data
  | `Buf _ -> assert false
  | `Sxdata _ -> assert false
  | `Uninit 0 -> Bytes.of_string ""
  | `Uninit size ->
      failwith
        (Printf.sprintf
           "uninitialized section %s (size %d): not supported"
           s.sec_name size
        )

let split_relocs page_size relocs =
  let relocs = Array.of_list relocs in
  Array.sort (fun (i, _) (j, _) -> compare i j) relocs;
  let blocks = ref [] and current_block = ref (ref []) and current_base = ref (-1l) in
  Array.iter
    (fun (rva, k) ->
      let base = Int32.mul (Int32.div rva page_size) page_size in
      let ofs = Int32.to_int (Int32.sub rva base) in
      if base = !current_base then (!current_block) := (ofs, k) :: !(!current_block)
      else begin
        current_base := base;
        current_block := ref [(ofs, k)];
        blocks := (base, !current_block) :: !blocks
      end
    )
    relocs;
  List.rev_map (fun (base, relocs) -> (base, List.rev !relocs)) !blocks


type sec_info = {
    sec_info_obj: coff; (* original object *)
    mutable sec_info_sec: section; (* target section in the image *)
    mutable sec_info_ofs: int32; (* offset within the target image *)
    sec_info_vaddress: int32 Lazy.t;
  }

let create_dll oc objs =
  let image_base = 0x10000l in
  let page_size = 0x1000l in
  let dllname = "foo.dll" in

  (* msdos stub *)
  output_string oc "MZ";
  for _i = 3 to 0x3c do output_byte oc 0 done;
  emit_int32 oc 0xe8l; (* file offset of COFF file header, just here *)
  for _i = 0x40 to 0xe7 do output_byte oc 0 done;

  let sections = Hashtbl.create 8 in
  let sec_id = ref 0 in
  let sec_info = Hashtbl.create 8 in
  let sym_id = ref 0 in
  let globals = Hashtbl.create 8 in
  let locals = Hashtbl.create 8 in
  let commons = Hashtbl.create 8 in
  let relocs = ref [] in

  List.iter
    (fun obj ->
      List.iter
        (fun s ->
          (* todo: cut the section name at 8 chars, remove the part of $ *)
          if discard_section s then ()
          else let (l, sect) =
            try Hashtbl.find sections s.sec_name
            with Not_found ->
              let r =
                ref [],
                Section.create s.sec_name
                  (s.sec_opts
                     &&& (0x00000020l
                        ||| 0x00000040l
                        ||| 0x00000080l
                        ||| 0x10000000l
                        ||| 0x20000000l
                        ||| 0x40000000l
                        ||| 0x80000000l))
              in
              Hashtbl.replace sections s.sec_name r;
              r
          in
          l := s :: !l;
          s.sec_pos <- !sec_id;
          incr sec_id;
          let rec info =
            {
             sec_info_obj = obj;
             sec_info_sec = sect;
             sec_info_ofs = 0l;
             sec_info_vaddress = lazy (Int32.add info.sec_info_ofs sect.vaddress)
           }
          in
          Hashtbl.replace sec_info s.sec_pos info
        )
        obj.sections;

      List.iter
        (function
          | {sym_name=name; value=ofs; storage=storage; section = `Section s} as sym when s.sec_pos >= 0 ->
              let info = Hashtbl.find sec_info s.sec_pos in
              let rva = lazy (Int32.add ofs (Lazy.force info.sec_info_vaddress)) in
              if storage = 2
              then
                if Hashtbl.mem globals name
                then failwith ("Multiply defined symbol: " ^ name)
                else Hashtbl.replace globals name rva
              else begin
                sym.sym_pos <- !sym_id;
                incr sym_id;
                Hashtbl.replace locals sym.sym_pos rva
              end
          | {sym_name=name; storage=2; section=`Num 0; value=size } ->
              let oldsize =
                try Hashtbl.find commons name
                with Not_found -> 0l
              in
              Hashtbl.replace commons name (max size oldsize)
          | _ -> ()
        )
        obj.symbols;
    )
    objs;

  let rva_of_global s =
    try Hashtbl.find globals s
    with Not_found -> failwith ("Cannot find global symbol " ^ s)
  in
  let rva_of_local s =
    try Hashtbl.find locals s
    with Not_found -> assert false
  in

  let sects = ref [] in
  (* Put image sections at their target rva *)
  let va = ref 0x1000l in
  let put_sect s =
    s.vsize <- Int32.of_int (Section.size s);
    s.vaddress <- !va;
    va := align (Int32.add !va s.vsize) 0x1000l;
    sects := s :: !sects
  in

  (* create the uninitialized section data *)
  let () =
    let bss = Section.create ".bss" 0xC0000080l in
    let total = ref 0l in
    Hashtbl.iter
      (fun name size ->
        if not (Hashtbl.mem globals name) && size > 0l
        then
          let pos = !total in
          total := Int32.add !total size;
          let rva = lazy (Int32.add pos bss.vaddress) in
          Hashtbl.replace globals name rva
      )
      commons;
    bss.data <- `Uninit (Int32.to_int !total);
    if !total <> 0l then put_sect bss
  in

  let treat_sect (l, sect) =
    let sect_len = ref 0 in
    let mk_sect s =
      let buf = Buf.create () in
      let info = Hashtbl.find sec_info s.sec_pos in
      let sec_ofs = !sect_len in
      info.sec_info_ofs <- Int32.of_int sec_ofs;
      let sdata = sect_data s in
      let slen = Bytes.length sdata in

      let pad =
        if slen mod 16 = 0 then 0
        else 16 - slen mod 16
      in
      sect_len := !sect_len + Bytes.length sdata + pad;
      Buf.bytes buf sdata;
      if pad > 0 then
        Buf.bytes buf (Bytes.make pad '\000');

      let mk_reloc r =
        (* rva of the target symbol *)
        let sym = r.symbol in
        let rva =
          if Symbol.is_extern sym || Symbol.is_export sym then rva_of_global sym.sym_name
          else if sym.sym_pos >= 0 then rva_of_local sym.sym_pos
          else begin
            Symbol.dump sym;
            failwith (Printf.sprintf "Cannot resolve symbol %s\n" sym.sym_name)
          end
        in

        (* rva of the relocation *)
        let rel_rva = lazy (Int32.add r.addr (Lazy.force info.sec_info_vaddress)) in

        let pos = Int32.to_int r.addr in
        let initial = read_int32 sdata pos in
        match !Cmdline.machine, r.rtype with
        | `x86, 0x06 (* IMAGE_REL_I386_DIR32 *)
        | `x64, 0x02 (* IMAGE_REL_AMD64_ADDR32 *) ->
            (* 32-bit VA *)
            relocs := (rel_rva, `R32) :: !relocs;
            Buf.patch_lazy_int32 buf pos (lazy (Int32.add (Int32.add initial (Lazy.force rva)) image_base))

        | `x64, 0x01 (* IMAGE_REL_AMD64_ADDR64 *) ->
            (* 64-bit VA *)
            assert(read_int32 sdata (pos + 4) = 0l);
            relocs := (rel_rva, `R64) :: !relocs;
            Buf.patch_lazy_int32 buf pos (lazy (Int32.add (Int32.add initial (Lazy.force rva)) image_base))

        | `x86, 0x14 (* IMAGE_REL_I386_REL32 *)
        | `x64, 0x04 (* IMAGE_REL_AMD64_REL32 *) ->
            Buf.patch_lazy_int32 buf pos (lazy (Int32.sub (Int32.add initial (Lazy.force rva)) (Int32.add (Lazy.force rel_rva) 4l)))
        | _, k ->
            Printf.ksprintf failwith "Unsupport relocation kind %04x for %s"
              k r.symbol.sym_name
      in
      List.iter mk_reloc s.relocs;
      buf
    in
    let bufs = List.map mk_sect !l in
    sect.data <- `Buf bufs;
    put_sect sect
  in

  Hashtbl.iter
    (fun _name x -> treat_sect x)
    sections;

  (* create the export table *)
  let edata =
    let edata = Section.create ".dllexp" 0x40000040l in
    let b = Buf.create () in
    edata.data <- `Buf [b];
    let vaddress = lazy edata.vaddress in

    let export_symbols = ["symtbl";"reloctbl"] in
    let export_symbols = List.sort compare export_symbols in
    Buf.int32 b 0l; (* flags *)
    Buf.int32 b 0l; (* timestamp *)
    Buf.int32 b 0l; (* version *)
    let dllname_offset = Buf.future_int32 b vaddress in (* name rva *)
    Buf.int32 b 1l; (* ordinal base *)
    Buf.int32 b (Int32.of_int (List.length export_symbols)); (* addr table entries *)
    Buf.int32 b (Int32.of_int (List.length export_symbols)); (* number of name pointers *)
    let exp_tbl = Buf.future_int32 b vaddress in (* export address table rva *)
    let name_ptr_tbl = Buf.future_int32 b vaddress in (* name pointer rva *)
    let ord_ptr_tbl = Buf.future_int32 b vaddress in (* ordinal table pointer rva *)
    Buf.set_future b dllname_offset;
    Buf.string b dllname;
    Buf.int8 b 0;
    Buf.set_future b exp_tbl;
    List.iter (fun s -> Buf.lazy_int32 b (rva_of_global (Cmdline.usym s)))
      export_symbols;
    Buf.set_future b name_ptr_tbl;
    let export_symbols_ofs =
      List.map (fun _ -> Buf.future_int32 b vaddress) export_symbols
    in
    Buf.set_future b ord_ptr_tbl;
    for i = 0 to List.length export_symbols - 1 do
      Buf.int16 b i
    done;
    List.iter2
      (fun s f ->
        Buf.set_future b f;
        Buf.string b s;
        Buf.int8 b 0
      )
      export_symbols
      export_symbols_ofs;
    put_sect edata;
    edata
  in

  (* create the reloc table *)
  let rdata =
    let rdata = Section.create ".reloc" 0x40000040l in
    let b = Buf.create () in
    rdata.data <- `Buf [b];

    (* careful with list functions: the list of relocs can be very long *)
    let relocs = List.rev_map (fun (rva, k) -> (Lazy.force rva, k)) !relocs in
    let relocs = split_relocs page_size relocs in
    List.iter
      (fun (base, relocs) ->
        let n = List.length relocs in
        let size = 8 + 2 * n in
        let size = if n mod 2 = 1 then size + 2 else size in
        Buf.int32 b base;
        Buf.int32 b (Int32.of_int size);
        List.iter
          (fun (ofs, k) ->
             let k =
               match k with
               | `R32 -> 0x3000 (* IMAGE_REL_BASED_HIGHLOW *)
               | `R64 -> 0xA000 (* IMAGE_REL_BASED_DIR64 *)
             in
             Buf.int16 b (ofs lor k)
          )
          relocs;
        if n mod 2 = 1 then Buf.int16 b 0
      )
      relocs;
    put_sect rdata;
    rdata
  in

  output_string oc "PE\000\000";
  (* coff header *)
  let machine = !Cmdline.machine in
  let disp_mach ~x86 ~x64 =
    match machine with `x86 -> x86 | `x64 -> x64
  in
  let emit_int32_64 x =
    emit_int32 oc x;
    if machine = `x64 then emit_int32 oc 0l
  in
  emit_int16 oc (disp_mach ~x86:0x14c ~x64:0x8664);
  emit_int16 oc (List.length !sects); (* number of sections *)
  emit_int32 oc 0l; (* date *)
  emit_int32 oc 0l; (* ptr to symbol table *)
  emit_int32 oc 0l; (* number of symbols *)
  emit_int16 oc ((disp_mach ~x86:28 ~x64:24) + (disp_mach ~x86:68 ~x64:88) + 8 * 16); (* size of optional headers *)
  emit_int16 oc
    (disp_mach
       ~x86:0x2102 (* flags: exec, 32-bit, dll *)
       ~x64:0x2022 (* flags: exec, large address aware(?), dll *)
    );

  (* optional header *)
  (*   standard fields *)
  emit_int16 oc (disp_mach ~x86:0x10b ~x64:0x20b); (* magic: pe32/pe32+ *)
  emit_int16 oc 8; (* linker version *)
  emit_int32 oc 0l; (* size of code *)
  emit_int32 oc 0l; (* size of initialized data *)
  emit_int32 oc 0l; (* size of uninitialized data *)
  emit_int32 oc 0l; (* entry point *)
  emit_int32 oc 0x1000l; (* base of code *)
  if machine = `x86 then emit_int32 oc 0x1000l; (* base of data *)
  (*   windows-specific fields *)
  emit_int32_64 image_base; (* image base *)
  emit_int32 oc 0x1000l; (* section alignment *)
  emit_int32 oc 0x200l; (* file alignment *)
  emit_int32 oc 0x04l; (* OS version *)
  emit_int32 oc 0l; (* image version *)
  emit_int32 oc 0x04l; (* subsystem version *)
  emit_int32 oc 0l; (* win32 version *)
  emit_int32 oc !va; (* size of image *)
  let size_of_headers = pos_out oc in
  emit_int32 oc 0l; (* size of headers *)
  emit_int32 oc 0l; (* checksum *)
  emit_int16 oc 3; (* subsystem: windows CUI *)
  emit_int16 oc 0x400; (* characteristics: no EH *)
  emit_int32_64 0x100000l; (* size of stack reserve *)
  emit_int32_64 0x1000l; (* size of stack commit *)
  emit_int32_64 0x100000l; (* size of heap reserve *)
  emit_int32_64 0x1000l; (* size of heap commit *)
  emit_int32 oc 0l; (* loader flags *)
  emit_int32 oc 16l; (* number of directories *)

  (* directories *)
  for i = 0 to 15 do
    match i with
    | 0 -> emit_int32 oc edata.vaddress; emit_int32 oc edata.vsize
    | 5 -> emit_int32 oc rdata.vaddress; emit_int32 oc rdata.vsize
    | _ -> emit_int32 oc 0l; emit_int32 oc 0l;
  done;

  let sects =
    List.map (Section.put (fun _ -> assert false) oc) (List.rev !sects)
  in
  let align_file () =
    let i = pos_out oc mod 0x200 in
    if i <> 0 then for _k = i + 1 to 0x200 do output_char oc '\000' done;
  in
  align_file ();
  patch_int32 oc size_of_headers (Int32.of_int (pos_out oc));
  List.iter (fun (data,_) -> align_file (); data ()) sects

end : sig
           val create_dll: out_channel -> Coff.coff list -> unit
end)
