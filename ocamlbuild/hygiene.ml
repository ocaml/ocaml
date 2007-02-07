(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
(* Original author: Berke Durak *)
(* Hygiene *)
open My_std
open Slurp

exception Exit_hygiene_violations

type rule =
| Implies_not of pattern * pattern
| Not of pattern
and pattern = suffix
and suffix = string

type penalty = Warn | Fail

type law = {
  law_name : string;
  law_rules : rule list;
  law_penalty : penalty
}

let list_collect f l =
  let rec loop result = function
    | [] -> List.rev result
    | x :: rest ->
        match f x with
        | None -> loop result rest
        | Some y -> loop (y :: result) rest
  in
  loop [] l

let list_none_for_all f l =
  let rec loop = function
    | [] -> None
    | x :: rest ->
        match f x with
        | None -> loop rest
        | y -> y
  in
  loop l

let sf = Printf.sprintf

module SS = Set.Make(String);;

let check ?(sterilize=false) laws entry =
  let penalties = ref [] in
  let microbes = ref SS.empty in
  let remove path name =
    if sterilize then
      microbes := SS.add (filename_concat path name) !microbes
  in
  let check_rule = fun entries -> function
    | Not suffix ->
        list_collect
          begin function
            | File(path, name, _, true) ->
                if Filename.check_suffix name suffix then
                  begin
                    remove path name;
                    Some(sf "File %s in %s has suffix %s" name path suffix)
                  end
                else
                  None
            | File _ | Dir _| Error _ | Nothing -> None
          end
          entries
    | Implies_not(suffix1, suffix2) ->
        list_collect
          begin function
            | File(path, name, _, true) ->
                if Filename.check_suffix name suffix1 then
                  begin
                    let base = Filename.chop_suffix name suffix1 in
                    let name' = base ^ suffix2 in
                    if List.exists
                       begin function
                         | File(_, name'', _, true) -> name' = name''
                         | File _ | Dir _ | Error _ | Nothing -> false
                       end
                       entries
                    then
                      begin
                        remove path name';
                        Some(sf "Files %s and %s should not be together in %s" name name' path)
                      end
                    else
                      None
                  end
                else
                  None
            | File _ | Dir _ | Error _ | Nothing -> None
          end
          entries
  in
  let rec check_entry = function
    | Dir(_,_,_,true,entries) ->
        List.iter
          begin fun law ->
            match List.concat (List.map (check_rule !*entries) law.law_rules) with
            | [] -> ()
            | explanations ->
              penalties := (law, explanations) :: !penalties
          end
          laws;
        List.iter check_entry !*entries
    | Dir _ | File _ | Error _ | Nothing -> ()
  in
  check_entry entry;
  begin
    let microbes = !microbes in
    if SS.is_empty microbes then
      (entry, !penalties)
    else
      begin
        Printf.printf "STERILIZE: do you want me to remove the following files?\n ";
        SS.iter
          begin fun fn ->
            Printf.printf " %s" fn
          end
          microbes;
        Printf.printf "\n";
        let rec answer () =
          Printf.printf "Type YES (in uppercase) for removal, something else for no action: %!";
          let u = input_line stdin in
          if u <> "" then
            u
          else
            answer ()
        in
        if answer () = "YES" then
          begin
            let success = ref true in
            SS.iter
              begin fun fn ->
                try
                  Sys.remove fn
                with
                | x ->
                    success := false;
                    Printf.printf "ERROR removing %s: %s\n%!" fn (Printexc.to_string x)
              end
              microbes;
            if !success then
              let entry' =
                Slurp.filter (fun path name _ -> not (SS.mem (My_std.filename_concat path name) microbes)) entry
              in
              (entry', []) (* XXX: No penalties, is this correct ? *)
            else
              raise Exit_hygiene_violations
          end
        else
          begin
            Printf.printf "I am taking your answer for a NO.\n";
            (entry, !penalties)
          end
      end
  end
;;
