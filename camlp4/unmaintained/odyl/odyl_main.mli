(* camlp4r *)
(* $Id$ *)

exception Error of string and string;

value nolib : ref bool;
value initialized : ref bool;
value path : ref (list string);
value loadfile : string -> unit;
value directory : string -> unit;

value go : ref (unit -> unit);
value name : ref string;
