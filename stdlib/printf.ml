(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"

let fprintf outchan format =
  let format = (Obj.magic format : string) in
  let rec doprn i =
    if i >= String.length format then
      Obj.magic ()
    else begin
      let c = String.unsafe_get format i in
      if c <> '%' then begin
        output_char outchan c;
        doprn (succ i)
      end else begin
        let j = skip_args (succ i) in
        match String.unsafe_get format j with
          '%' ->
            output_char outchan '%';
            doprn (succ j)
        | 's' ->
            Obj.magic(fun s ->
              if j <= i+1 then
                output_string outchan s
              else begin
                let p =
                  try
                    int_of_string (String.sub format (i+1) (j-i-1))
                  with _ ->
                    invalid_arg "fprintf: bad %s format" in
                if p > 0 && String.length s < p then begin
                  output_string outchan
                                (String.make (p - String.length s) ' ');
                  output_string outchan s
                end else if p < 0 && String.length s < -p then begin
                  output_string outchan s;
                  output_string outchan
                                (String.make (-p - String.length s) ' ')
                end else
                  output_string outchan s
              end;
              doprn (succ j))
        | 'c' ->
            Obj.magic(fun c ->
              output_char outchan c;
              doprn (succ j))
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun n ->
              output_string outchan
                            (format_int (String.sub format i (j-i+1)) n);
              doprn (succ j))
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            Obj.magic(fun f ->
              output_string outchan
                            (format_float (String.sub format i (j-i+1)) f);
              doprn (succ j))
        | 'b' ->
            Obj.magic(fun b ->
              output_string outchan (string_of_bool b);
              doprn (succ j))
        | 'a' ->
            Obj.magic(fun printer arg ->
              printer outchan arg;
              doprn(succ j))
        | 't' ->
            Obj.magic(fun printer ->
              printer outchan;
              doprn(succ j))
        | c ->
            invalid_arg ("fprintf: unknown format")
      end
    end

  and skip_args j =
    match String.unsafe_get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j

  in doprn 0

let printf fmt = fprintf stdout fmt
and eprintf fmt = fprintf stderr fmt

let bprintf_internal tostring buf format =
  let format = (Obj.magic format : string) in
  let rec doprn i =
    if i >= String.length format then
      if tostring then Obj.magic (Buffer.contents buf) else Obj.magic ()
    else begin
      let c = String.unsafe_get format i in
      if c <> '%' then begin
        Buffer.add_char buf c;
        doprn (succ i)
      end else begin
        let j = skip_args (succ i) in
        match String.unsafe_get format j with
          '%' ->
            Buffer.add_char buf '%';
            doprn (succ j)
        | 's' ->
            Obj.magic(fun s ->
              if j <= i+1 then
                Buffer.add_string buf s
              else begin
                let p =
                  try
                    int_of_string (String.sub format (i+1) (j-i-1))
                  with _ ->
                    invalid_arg "fprintf: bad %s format" in
                if p > 0 && String.length s < p then begin
                  Buffer.add_string buf
                                (String.make (p - String.length s) ' ');
                  Buffer.add_string buf s
                end else if p < 0 && String.length s < -p then begin
                  Buffer.add_string buf s;
                  Buffer.add_string buf
                                (String.make (-p - String.length s) ' ')
                end else
                  Buffer.add_string buf s
              end;
              doprn (succ j))
        | 'c' ->
            Obj.magic(fun c ->
              Buffer.add_char buf c;
              doprn (succ j))
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun n ->
              Buffer.add_string buf
                            (format_int (String.sub format i (j-i+1)) n);
              doprn (succ j))
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            Obj.magic(fun f ->
              Buffer.add_string buf
                            (format_float (String.sub format i (j-i+1)) f);
              doprn (succ j))
        | 'b' ->
            Obj.magic(fun b ->
              Buffer.add_string buf (string_of_bool b);
              doprn (succ j))
        | 'a' ->
            if tostring then
              Obj.magic(fun printer arg ->
                Buffer.add_string buf (printer () arg);
                doprn(succ j))
            else
              Obj.magic(fun printer arg ->
                printer buf arg;
                doprn(succ j))
        | 't' ->
            if tostring then
              Obj.magic(fun printer ->
                Buffer.add_string buf (printer ());
                doprn(succ j))
            else
              Obj.magic(fun printer ->
                printer buf;
                doprn(succ j))
        | c ->
            invalid_arg ("sprintf: unknown format")
      end
    end

  and skip_args j =
    match String.unsafe_get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j

  in doprn 0

let bprintf buf fmt = bprintf_internal false buf fmt

let sprintf fmt = bprintf_internal true (Buffer.create 16) fmt
