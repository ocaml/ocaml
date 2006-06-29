open Camlp4

module Id = struct
  let name = "Camlp4Parsers.LoadCamlp4Ast"
  let version = "$Id$"
end

module Make (Ast : Camlp4.Sig.Camlp4Ast.S) = struct
  module Ast = Ast
  open Ast

  let _loc = Loc.ghost

  let parse_implem ?directive_handler:(_) _ _ =
    let e =
    Ast.ExApp (_loc,
      Ast.ExApp (_loc,
        Ast.ExId (_loc,
          Ast.IdAcc (_loc, Ast.IdUid (_loc, "G"), Ast.IdLid (_loc, "extend"))),
        Ast.ExTyc (_loc, Ast.ExId (_loc, Ast.IdLid (_loc, "expr")),
          Ast.TyApp (_loc,
            Ast.TyId (_loc,
              Ast.IdAcc (_loc, Ast.IdUid (_loc, "G"),
                Ast.IdAcc (_loc, Ast.IdUid (_loc, "Entry"),
                  Ast.IdLid (_loc, "t")))),
            Ast.TyQuo (_loc, "expr")))),
      Ast.ExTup (_loc,
        Ast.ExCom (_loc, Ast.ExId (_loc, Ast.IdUid (_loc, "None")),
          Ast.ExApp (_loc,
            Ast.ExApp (_loc, Ast.ExId (_loc, Ast.IdUid (_loc, "::")),
              Ast.ExTup (_loc,
                Ast.ExCom (_loc, Ast.ExId (_loc, Ast.IdUid (_loc, "None")),
                  Ast.ExCom (_loc, Ast.ExId (_loc, Ast.IdUid (_loc, "None")),
                    Ast.ExApp (_loc,
                      Ast.ExApp (_loc, Ast.ExId (_loc, Ast.IdUid (_loc, "::")),
                        Ast.ExTup (_loc,
                          Ast.ExCom (_loc,
                            Ast.ExApp (_loc,
                              Ast.ExApp (_loc,
                                Ast.ExId (_loc, Ast.IdUid (_loc, "::")),
                                Ast.ExApp (_loc,
                                  Ast.ExId (_loc,
                                    Ast.IdAcc (_loc, Ast.IdUid (_loc, "G"),
                                      Ast.IdUid (_loc, "Skeyword"))),
                                  Ast.ExStr (_loc, "foo"))),
                              Ast.ExId (_loc, Ast.IdUid (_loc, "[]"))),
                            Ast.ExApp (_loc,
                              Ast.ExId (_loc,
                                Ast.IdAcc (_loc, Ast.IdUid (_loc, "G"),
                                  Ast.IdAcc (_loc, Ast.IdUid (_loc, "Action"),
                                    Ast.IdLid (_loc, "mk")))),
                              Ast.ExFun (_loc,
                                Ast.AsArr (_loc, Ast.PaAny _loc, Ast.ONone,
                                  Ast.ExFun (_loc,
                                    Ast.AsArr (_loc,
                                      Ast.PaTyc (_loc,
                                        Ast.PaId (_loc,
                                          Ast.IdLid (_loc, "_loc")),
                                        Ast.TyId (_loc,
                                          Ast.IdAcc (_loc,
                                            Ast.IdUid (_loc, "Loc"),
                                            Ast.IdLid (_loc, "t")))),
                                      Ast.ONone,
                                      Ast.ExTyc (_loc,
                                        Ast.ExApp (_loc,
                                          Ast.ExApp (_loc,
                                            Ast.ExId (_loc,
                                              Ast.IdAcc (_loc,
                                                Ast.IdUid (_loc, "Ast"),
                                                Ast.IdUid (_loc, "ExId"))),
                                            Ast.ExId (_loc,
                                              Ast.IdLid (_loc, "_loc"))),
                                          Ast.ExApp (_loc,
                                            Ast.ExApp (_loc,
                                              Ast.ExId (_loc,
                                                Ast.IdAcc (_loc,
                                                  Ast.IdUid (_loc, "Ast"),
                                                  Ast.IdUid (_loc, "IdLid"))),
                                              Ast.ExId (_loc,
                                                Ast.IdLid (_loc, "_loc"))),
                                            Ast.ExStr (_loc, "foo"))),
                                        Ast.TyQuo (_loc, "expr")))))))))),
                      Ast.ExId (_loc, Ast.IdUid (_loc, "[]"))))))),
            Ast.ExId (_loc, Ast.IdUid (_loc, "[]"))))))
    in Ast.StExp (_loc, e)
  let parse_interf ?directive_handler:(_) _ _ = assert false;;

end;;

let module M = Camlp4.Register.OCamlParser(Id)(Make) in ()
