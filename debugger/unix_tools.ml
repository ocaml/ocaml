(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(****************** Tools for Unix *************************************)

module Real_stdlib = Stdlib
open Misc
open Unix

(*** Convert a socket name into a socket address. ***)
let convert_address address =
  if address = "" then
    failwith "Can't convert address: empty address";
  let unix_addr_info =
    { ai_family = PF_UNIX; ai_socktype = SOCK_STREAM; ai_protocol = 0;
      ai_addr = ADDR_UNIX address; ai_canonname = ""; } in
  match String.rindex address ':' with
  | exception Not_found -> unix_addr_info
  (* "./foo" is explicitly a path and not a network address *)
  | _ when not (Filename.is_implicit address) -> unix_addr_info
  | n ->
     let is_likely_ipv6 =
       n >= 4 && address.[0] = '[' && address.[n - 1] = ']' in
     let host = if is_likely_ipv6 then String.sub address 1 (n - 2)
                else String.sub address 0 n
     and port = String.(sub address (n + 1) (length address - n - 1)) in
     if host = "" || port = "" then
       Printf.ksprintf failwith "Can't convert address %S: \
                                 empty host or empty port" address;
     port |> String.iter (fun c -> if c < '0' || '9' < c then
       Printf.ksprintf failwith "Can't convert address %S: \
                                 the port number should be an integer" address);
     match getaddrinfo host port [AI_SOCKTYPE SOCK_STREAM] with
     | addr_info :: _ -> addr_info
     | [] -> Printf.ksprintf failwith
               "Can't convert address: unknown host %S port %S" host port

(*** Report a unix error. ***)
let report_error = function
  | Unix_error (err, fun_name, arg) ->
     prerr_string "Unix error: '";
     prerr_string fun_name;
     prerr_string "' failed";
     if String.length arg > 0 then
       (prerr_string " on '";
        prerr_string arg;
        prerr_string "'");
     prerr_string ": ";
     prerr_endline (error_message err)
  | _ -> fatal_error "report_error: not a Unix error"

(* Find program `name' in `PATH'. *)
(* Return the full path if found. *)
(* Raise `Not_found' otherwise. *)
let search_in_path name =
  Printf.fprintf Real_stdlib.stderr "search_in_path [%s]\n%!" name;
  let check name =
    try access name [X_OK]; name with Unix_error _ -> raise Not_found
  in
    if not (Filename.is_implicit name) then
      check name
    else
      let path = Sys.getenv "PATH" in
        let length = String.length path in
          let rec traverse pointer =
            if (pointer >= length) || (path.[pointer] = ':') then
              pointer
            else
              traverse (pointer + 1)
          in
            let rec find pos =
              let pos2 = traverse pos in
                let directory = (String.sub path pos (pos2 - pos)) in
                  let fullname =
                    if directory = "" then name else directory ^ "/" ^ name
                  in
                    try check fullname with
                    | Not_found ->
                        if pos2 < length then find (pos2 + 1)
                        else raise Not_found
          in
            find 0

(* Expand a path. *)
(* ### path -> path' *)
let rec expand_path ch =
  let rec subst_variable ch =
    try
      let pos = String.index ch '$' in
        if (pos + 1 < String.length ch) && (ch.[pos + 1] = '$') then
          (String.sub ch 0 (pos + 1))
            ^ (subst_variable
                 (String.sub ch (pos + 2) (String.length ch - pos - 2)))
        else
          (String.sub ch 0 pos)
            ^ (subst2 (String.sub ch (pos + 1) (String.length ch - pos - 1)))
    with Not_found ->
      ch
  and subst2 ch =
    let suiv =
      let i = ref 0 in
        while !i < String.length ch &&
              (let c = ch.[!i] in (c >= 'a' && c <= 'z')
                               || (c >= 'A' && c <= 'Z')
                               || (c >= '0' && c <= '9')
                               || c = '_')
        do incr i done;
        !i
    in (Sys.getenv (String.sub ch 0 suiv))
       ^ (subst_variable (String.sub ch suiv (String.length ch - suiv)))
  in
    let ch = subst_variable ch in
      let concat_root nom ch2 =
        try Filename.concat (getpwnam nom).pw_dir ch2
        with Not_found ->
          "~" ^ nom
      in
        if ch.[0] = '~' then
          try
            match String.index ch '/' with
              1 ->
                (let tail = String.sub ch 2 (String.length ch - 2)
                 in
                   try Filename.concat (Sys.getenv "HOME") tail
                   with Not_found ->
                     concat_root (Sys.getenv "LOGNAME") tail)
            |  n -> concat_root
                      (String.sub ch 1 (n - 1))
                      (String.sub ch (n + 1) (String.length ch - n - 1))
          with
            Not_found ->
              expand_path (ch ^ "/")
        else ch

let make_absolute name =
  if Filename.is_relative name
  then Filename.concat (getcwd ()) name
  else name
