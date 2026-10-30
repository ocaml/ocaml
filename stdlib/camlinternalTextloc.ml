module type S = sig
  val file_none : string

  type byte_pos = int

  type line_num = int

  type line_pos = line_num * byte_pos

  type t

  val none : t

  val make :
    file:string -> first_byte:byte_pos -> last_byte:byte_pos ->
    first_line:line_pos -> last_line:line_pos -> t

  val file : t -> string

  val first_byte : t -> byte_pos
  val last_byte : t -> byte_pos

  val first_line : t -> line_pos
  val last_line : t -> line_pos
end

let file_none = ""

type byte_pos = int

type line_num = int

type line_pos = line_num * byte_pos

type repr =
  { file : string;
    first_byte : byte_pos; last_byte : byte_pos;
    first_line : line_pos; last_line : line_pos }

type t = textloc

external of_repr : repr -> t = "%identity"
external to_repr : t -> repr = "%identity"

let make ~file ~first_byte ~last_byte ~first_line ~last_line =
  of_repr { file; first_byte; last_byte; first_line; last_line }

let none =
  let line_pos_none = -1, -1 in
  make
    ~file:"-" ~first_byte:~-1 ~last_byte:~-1
    ~first_line:line_pos_none ~last_line:line_pos_none

let file l = (to_repr l).file
let first_byte l = (to_repr l).first_byte
let last_byte l = (to_repr l).last_byte
let first_line l = (to_repr l).first_line
let last_line l = (to_repr l).last_line

type view =
  { file : string;
    first_byte : byte_pos; last_byte : byte_pos;
    first_line : line_pos; last_line : line_pos }

let view (l : t) : view =
  let { file; first_byte; last_byte; first_line; last_line } : repr =
    to_repr l
  in
  { file; first_byte; last_byte; first_line; last_line }

let of_lexing_range (start : Lexing.position) (end_ : Lexing.position) : t =
  let open Lexing in
  assert (start.pos_fname = end_.pos_fname);
  make
    ~file:start.pos_fname
    ~first_byte:start.pos_cnum
    ~last_byte:(end_.pos_cnum - 1) (* "end" means past the end *)
    ~first_line:(start.pos_lnum, start.pos_bol)
    ~last_line:(end_.pos_lnum, end_.pos_bol)

module type Low_level = sig
  type repr
  val int : int -> repr
  val string : string -> repr
  val block : repr list -> repr
end

module Lift_repr (Low_level : Low_level) = struct
  let to_repr l =
    let { file; first_byte; last_byte;
          first_line = first_line_num, first_line_pos;
          last_line = last_line_num, last_line_pos } : repr = to_repr l in
    let open Low_level in
    block
      [ string file;
        int first_byte; int last_byte;
        block [ int first_line_num; int first_line_pos ];
        block [ int last_line_num; int last_line_pos ];
      ]

  let make ~file ~first_byte ~last_byte ~first_line ~last_line =
    to_repr (make ~file ~first_byte ~last_byte ~first_line ~last_line)
end
