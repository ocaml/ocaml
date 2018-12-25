(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  section : Asm_section.t;
  compilation_unit : Compilation_unit.t;
  name : string;
  (* Just like for [Backend_sym], the [name] uniquely determines the
     symbol. *)
}

let escape name =
  let spec = ref false in
  for i = 0 to String.length name - 1 do
    match String.unsafe_get name i with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ()
    | _ -> spec := true;
  done;
  if not !spec then begin
    name
  end else begin
    let b = Buffer.create (String.length name + 10) in
    String.iter
      (function
        | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c -> Buffer.add_char b c
        | c -> Printf.bprintf b "$%02x" (Char.code c)
      )
      name;
    Buffer.contents b
  end

let symbol_prefix () = (* CR mshinwell: needs checking *)
  match Target_system.architecture () with
  | IA32 | X86_64 ->
    begin match Target_system.system () with
    | Linux
    | Windows Cygwin
    | Windows MinGW
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Generic_BSD
    | Solaris
    | BeOS
    | GNU
    | Dragonfly
    | Windows Native
    | Unknown -> "" (* checked ok. *)
    | MacOS_like -> "_" (* checked ok. *)
    end
  | ARM
  | AArch64
  | POWER
  | Z -> ""

let encode backend_sym =
  Backend_sym.to_escaped_string
    ~symbol_prefix:(symbol_prefix ())
    ~escape backend_sym

let create backend_sym =
  let section : Asm_section.t =
    match Backend_sym.kind backend_sym with
    | Text -> Text
    | Data -> Data
  in
  { section;
    compilation_unit = Backend_sym.compilation_unit backend_sym;
    name = encode backend_sym;
  }

let of_external_name section compilation_unit name =
  { section;
    compilation_unit;
    name =  (* The choice of [Data] is arbitrary. *)
      encode (Backend_sym.of_external_name compilation_unit name Data);
  }

let of_external_name_no_prefix section compilation_unit name =
  let name =
    (* The choice of [Data] is arbitrary. *)
    Backend_sym.to_escaped_string ~symbol_prefix:"" ~escape
      (Backend_sym.of_external_name compilation_unit name Data)
  in
  { section;
    compilation_unit;
    name;
  }

let section t = t.section

let encode ?reloc t =
  match reloc with
  | None -> t.name
  | Some reloc -> t.name ^ reloc

let prefix_with t prefix =
  { section = t.section;
    compilation_unit = t.compilation_unit;
    name = (escape prefix) ^ t.name;
  }

(* Detection of functions that can be duplicated between a DLL and
   the main program (PR#4690) *)

let isprefix s1 s2 =
  String.length s1 <= String.length s2
    && String.sub s2 0 (String.length s1) = s1

let is_generic_function t =
  List.exists
    (fun p -> isprefix p t.name)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"]

let compilation_unit t = t.compilation_unit

include Identifiable.Make (struct
  type nonrec t = t
  let compare t1 t2 = String.compare t1.name t2.name
  let equal t1 t2 = String.equal t1.name t2.name
  let hash t = Hashtbl.hash t.name
  let output _ _ = Misc.fatal_error "Not yet implemented"
  let print ppf t = Format.pp_print_string ppf t.name
end)

module Names = struct
  (* See corresponding CR-someday in backend_sym.ml. *)
  let of_external_name section name =
    of_external_name section Compilation_unit.extern name

  let mcount = of_external_name Text "mcount"
  let _mcount = of_external_name Text "_mcount"
  let __gnu_mcount_nc = of_external_name Text "__gnu_mcount_nc"
  let sqrt = of_external_name Text "sqrt"

  let caml_young_ptr = of_external_name Data "caml_young_ptr"
  let caml_young_limit = of_external_name Data "caml_young_limit"
  let caml_exception_pointer = of_external_name Data "caml_exception_pointer"
  let caml_negf_mask = of_external_name Data "caml_negf_mask"
  let caml_absf_mask = of_external_name Data "caml_absf_mask"

  let caml_call_gc = of_external_name Text "caml_call_gc"
  let caml_c_call = of_external_name Text "caml_c_call"
  let caml_alloc1 = of_external_name Text "caml_alloc1"
  let caml_alloc2 = of_external_name Text "caml_alloc2"
  let caml_alloc3 = of_external_name Text "caml_alloc3"
  let caml_allocN = of_external_name Text "caml_allocN"
  let caml_ml_array_bound_error =
    of_external_name Text "caml_ml_array_bound_error"
  let caml_raise_exn = of_external_name Text "caml_raise_exn"

  let caml_frametable = of_external_name Data "caml_frametable"
  let caml_spacetime_shapes = of_external_name Data "caml_spacetime_shapes"
end
