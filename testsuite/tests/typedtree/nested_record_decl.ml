(* TEST
 flags = "-dtypedtree -dno-locations -dno-unique-ids";
 expect;
*)

(* One nested record beside an ordinary field. *)
type person = {
  name : string;
  address : { street : string; city : string };
}
[%%expect{|
[
  structure_item
    Tstr_type Rec
    [
      type_declaration person
        ptype_params =
          []
        ptype_constraints =
          []
        ptype_kind =
          Ttype_record
            [

                Immutable
                Nonatomic
                name                core_type
                  Ttyp_poly
                  core_type
                    Ttyp_constr "string!"
                    []

                Immutable
                Nonatomic
                address                [

                    Immutable
                    Nonatomic
                    street                    core_type
                      Ttyp_poly
                      core_type
                        Ttyp_constr "string!"
                        []

                    Immutable
                    Nonatomic
                    city                    core_type
                      Ttyp_poly
                      core_type
                        Ttyp_constr "string!"
                        []
                ]
            ]
        ptype_private = Public
        ptype_manifest =
          None
    ]
]

type person = {
  name : string;
  address : { street : string; city : string; };
}
|}]

(* Two levels of nesting, with mutable and atomic nested fields. *)
type company = {
  id : int;
  head_office : {
    mutable label : string;
    location : { lat : float; mutable lon : float [@atomic] };
  };
}
[%%expect{|
[
  structure_item
    Tstr_type Rec
    [
      type_declaration company
        ptype_params =
          []
        ptype_constraints =
          []
        ptype_kind =
          Ttype_record
            [

                Immutable
                Nonatomic
                id                core_type
                  Ttyp_poly
                  core_type
                    Ttyp_constr "int!"
                    []

                Immutable
                Nonatomic
                head_office                [

                    Mutable
                    Nonatomic
                    label                    core_type
                      Ttyp_poly
                      core_type
                        Ttyp_constr "string!"
                        []

                    Immutable
                    Nonatomic
                    location                    [

                        Immutable
                        Nonatomic
                        lat                        core_type
                          Ttyp_poly
                          core_type
                            Ttyp_constr "float!"
                            []

                        attribute "atomic"
                          []
                        Mutable
                        Atomic
                        lon                        core_type
                          Ttyp_poly
                          core_type
                            Ttyp_constr "float!"
                            []
                    ]
                ]
            ]
        ptype_private = Public
        ptype_manifest =
          None
    ]
]

type company = {
  id : int;
  head_office : { mutable label : string;
    location : { lat : float; mutable lon : float [@atomic]; };
  };
}
|}]
