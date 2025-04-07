(* If you change this internal representation, make sure to also change the
   code inserting values of type [t] in [Translcore]. *)
type repr =
  { filename : string; line : int; start_offset : int; end_offset : int }

type t = lexing_location

let of_repr (r : repr) : t = Obj.magic r

let to_repr (t : t) : repr = Obj.magic t

let dummy =
  of_repr { filename = ""; line = 0; start_offset = -1; end_offset = -1 }

let filename { filename; _ } = filename

let filename t = filename (to_repr t)

let line_num { line; _ } = line

let line_num t = line_num (to_repr t)

let line_pos { start_offset; _ } = start_offset

let line_pos t = line_pos (to_repr t)

let end_pos { end_offset; _ } = end_offset

let end_pos t = end_pos (to_repr t)
