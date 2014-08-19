(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*        Mehdi Dogguy, PPS laboratory, University Paris Diderot       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.   Modifications Copyright 2010 Mehdi Dogguy,       *)
(*  used and distributed as part of OCaml by permission from           *)
(*  the author.   This file is distributed under the terms of the      *)
(*  Q Public License version 1.0.                                      *)
(*                                                                     *)
(***********************************************************************)


open Printf
open Misc
open Config

open Cmi_format
open Cmx_format

(** copied from objinfo *)
let read_dyn_header filename ic =
  let tempfile = Filename.temp_file "objinfo" ".out" in
  let helper = Filename.concat Config.standard_library "objinfo_helper" in
  try
    try_finally
      (fun () ->
        let rc = Sys.command (sprintf "%s %s > %s"
                                (Filename.quote helper)
                                (Filename.quote filename)
                                tempfile) in
        if rc <> 0 then failwith "cannot read";
        let tc = open_in tempfile in
        try_finally
          (fun () ->
            let ofs = Scanf.fscanf tc "%Ld" (fun x -> x) in
            LargeFile.seek_in ic ofs;
            Some(input_value ic : dynheader))
          (fun () -> close_in tc))
      (fun () -> remove_file tempfile)
  with Failure _ | Sys_error _ -> None

let md5sum_aux filename =
  let ic = open_in_bin filename in
  let len_magic_number = String.length cmo_magic_number in
  let magic_number = really_input_string ic len_magic_number in
  let default () = Digest.file filename in
  let crc =
    if magic_number = cmo_magic_number then begin
      (** cmo doesn't have digest stored, default *)
      default ()
    end else if magic_number = cma_magic_number then begin
      (** cma doesn't have digest stored, default *)
      default ()
    end else if magic_number = cmi_magic_number then begin
      (** cmi digest use the digest of the signature, of
          all the dependencies and the flags *)
      let crcs,flags = Cmi_format.input_crcs_flags ic in
      let todigest =
        Buffer.create (List.length crcs * 16 + List.length flags) in
      List.iter (function
          | (_,Some crc) -> Buffer.add_string todigest crc
          | (name,_)     -> Buffer.add_string todigest name
        ) crcs;
      List.iter (function Rectypes -> Buffer.add_char todigest 'R') flags;
      let todigest, len = Buffer.recreate todigest 1 in
      Digest.subbytes todigest 0 len
    end else if magic_number = cmx_magic_number then begin
      (** the crc of the unit is at the end of the file *)
      let pos = in_channel_length ic - 16 in
      seek_in ic pos;
      Digest.input ic
    end else if magic_number = cmxa_magic_number then begin
      (** There is no crc easily available in a cmxa without reading the
          whole library_infos, ie file. So we just compute directly
          the digest of the file. *)
      default ()
    end else begin
      let pos_trailer = in_channel_length ic - len_magic_number in
      let _ = seek_in ic pos_trailer in
      let magic_number = really_input_string ic len_magic_number in
      if magic_number = Config.exec_magic_number then begin
        (** bytecode *)
        default ()
      end else if Filename.check_suffix filename ".cmxs" then begin
        flush stdout;
        match read_dyn_header filename ic with
        | None ->
            (** can't read the header, digest of the whole file *)
            default ()
        | Some header ->
            if header.dynu_magic = Config.cmxs_magic_number then
              let units = header.dynu_units in
              let todigest = Buffer.create (List.length units * 16) in
              List.iter
                (fun unit -> Buffer.add_string todigest unit.dynu_crc) units;
              let todigest, len = Buffer.recreate todigest 1 in
              Digest.subbytes todigest 0 len
            else begin
              (** wrong magic number *)
              default ()
            end;
      end else begin
        (** not an ocaml object file *)
        default ()
      end
    end
  in
  close_in ic;
  crc

let md5sum filename =
  let digest = md5sum_aux filename in
  Printf.printf "%s\n%!" (Digest.to_hex digest)

let arg_list = []
let arg_usage =
   Printf.sprintf "%s [OPTIONS] FILES : give information on files" Sys.argv.(0)

let main() =
  Arg.parse arg_list md5sum arg_usage;
  exit 0

let _ = main ()
