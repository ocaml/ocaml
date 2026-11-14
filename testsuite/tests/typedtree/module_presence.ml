(* TEST
 flags = "-dtypedtree -dno-locations";
 expect;
*)

module X = struct end
[%%expect{|
[
  structure_item
    Tstr_module (Present)
    X/282
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
    X/283
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
    Y/284
      module_expr
        Tmod_ident "X/283"
]

module Y = X
|}]

module type T = sig module Y = X end
[%%expect{|
[
  structure_item
    Tstr_modtype "T/286"
      module_type
        Tmty_signature
        [
          signature_item
            Tsig_module (Absent)
            Y/285
              module_type
                Tmty_alias "X/283"
        ]
]

module type T = sig module Y = X end
|}]
