(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module type INCLUDING = sig
  include module type of List
  include module type of ListLabels
end

module Including_typed: INCLUDING = struct
  include List
  include ListLabels
end
