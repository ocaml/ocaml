(* CRC computation *)

val for_string: string -> int -> int -> int
external for_channel: in_channel -> int -> int = "crc_chan"


