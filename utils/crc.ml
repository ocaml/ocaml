(* CRC computation *)

external unsafe_for_string: string -> int -> int -> int = "crc_string"

let for_string str ofs len =
  if ofs < 0 or ofs + len > String.length str
  then invalid_arg "Crc.for_string"
  else unsafe_for_string str ofs len

external for_channel: in_channel -> int -> int = "crc_chan"


