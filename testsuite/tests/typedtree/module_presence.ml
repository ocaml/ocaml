(* TEST
 flags = "-dtypedtree -dno-locations";
 expect;
*)

module X = struct end
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X/11
      module_expr
        Tmod_structure
        []
]

module X : sig end
|}]

module X = struct end [@foo]
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X/12
      module_expr
        attribute "foo"
          []
        Tmod_structure
        []
]

module X : sig end
|}]

module Y = X
[%%expect{|
[
  structure_item
    Tstr_module (Absent)
    Y/13
      module_expr
        Tmod_ident "X/12"
]

module Y = X
|}]

module type T = sig module Y = X end
[%%expect{|
[
  structure_item
    Tstr_modtype "T/15"
      module_type
        Tmty_signature
        [
          signature_item
            Tsig_module (Absent)
            Y/14
              module_type
                Tmty_alias "X/12"
        ]
]

module type T = sig module Y = X end
|}]
