(* TEST
include unix
flags += " -nolabels "
* hasunix
** bytecode
** native
*)

module U : module type of Unix = UnixLabels

let ()  =
  ()
