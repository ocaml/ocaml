(* Specific operations for the Intel 386 processor *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

type specific_operation =
    Ineg                                (* Integer negate *)
  | Ilea of addressing_mode             (* Lea gives scaled adds *)

(* Sizes, endianness *)

let big_endian = false

let size_addr = 4
let size_int = 4
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 n -> Iindexed2(n + delta)
  | Iindexed2scaled(scale, n) -> Iindexed2scaled(scale, n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1
  | Iindexed2 n -> 2
  | Iindexed2scaled(scale, n) -> 2

(* Printing operations and addressing modes *)

open Format

let print_addressing printreg addr arg =
  match addr with
    Ibased(s, 0) ->
      print_string "\""; print_string s; print_string "\""
  | Ibased(s, n) ->
      print_string "\""; print_string s; print_string "\" + "; print_int n
  | Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed2 n ->
      printreg arg.(0); print_string " + "; printreg arg.(1);
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed2scaled(scale, n) ->
      printreg arg.(0); print_string " + "; printreg arg.(1);
      print_string " * "; print_int scale;
      if n <> 0 then begin print_string " + "; print_int n end

let print_specific_operation printreg op arg =
  match op with
    Ineg -> print_string "- "; printreg arg.(0)
  | Ilea addr -> print_addressing printreg addr arg

