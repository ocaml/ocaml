(* $Id$ *)

let sub s ?:pos{=0} ?:len{=String.length s - pos} () =
  String.sub s pos len
