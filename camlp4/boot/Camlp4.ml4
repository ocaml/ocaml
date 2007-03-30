module Debug : sig INCLUDE "camlp4/Camlp4/Debug.mli"; end = struct INCLUDE "camlp4/Camlp4/Debug.ml"; end;
module Options : sig INCLUDE "camlp4/Camlp4/Options.mli"; end = struct INCLUDE "camlp4/Camlp4/Options.ml"; end;
module Sig = struct INCLUDE "camlp4/Camlp4/Sig.ml"; end;
module ErrorHandler : sig INCLUDE "camlp4/Camlp4/ErrorHandler.mli"; end = struct INCLUDE "camlp4/Camlp4/ErrorHandler.ml"; end;

module Struct = struct
  module Loc :
    sig INCLUDE "camlp4/Camlp4/Struct/Loc.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Struct/Loc.ml"; end;
  module Token :
    sig INCLUDE "camlp4/Camlp4/Struct/Token.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Struct/Token.ml"; end;
  module Lexer = struct INCLUDE "camlp4/boot/Lexer.ml"; end;
  module Camlp4Ast = struct INCLUDE "camlp4/Camlp4/Struct/Camlp4Ast.ml"; end;
  module DynAst = struct INCLUDE "camlp4/Camlp4/Struct/DynAst.ml"; end;
  module Quotation = struct INCLUDE "camlp4/Camlp4/Struct/Quotation.ml"; end;
  module AstFilters = struct INCLUDE "camlp4/Camlp4/Struct/AstFilters.ml"; end;
  module Camlp4Ast2OCamlAst :
    sig INCLUDE "camlp4/Camlp4/Struct/Camlp4Ast2OCamlAst.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Struct/Camlp4Ast2OCamlAst.ml"; end;
  module CleanAst = struct INCLUDE "camlp4/Camlp4/Struct/CleanAst.ml"; end;
  module CommentFilter :
    sig INCLUDE "camlp4/Camlp4/Struct/CommentFilter.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Struct/CommentFilter.ml"; end;
  module DynLoader :
    sig INCLUDE "camlp4/Camlp4/Struct/DynLoader.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Struct/DynLoader.ml"; end;
  module EmptyError :
    sig INCLUDE "camlp4/Camlp4/Struct/EmptyError.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Struct/EmptyError.ml"; end;
  module EmptyPrinter :
    sig INCLUDE "camlp4/Camlp4/Struct/EmptyPrinter.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Struct/EmptyPrinter.ml"; end;
  module FreeVars :
    sig INCLUDE "camlp4/Camlp4/Struct/FreeVars.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Struct/FreeVars.ml"; end;
  module Grammar = struct
    module Context = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Context.ml"; end;
    module Structure = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Structure.ml"; end;
    module Search = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Search.ml"; end;
    (* module Find = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Find.ml"; end; *)
    module Tools = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Tools.ml"; end;
    module Print :
      sig INCLUDE "camlp4/Camlp4/Struct/Grammar/Print.mli"; end =
      struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Print.ml"; end;
    module Failed = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Failed.ml"; end;
    module Parser = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Parser.ml"; end;
    module Insert = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Insert.ml"; end;
    module Delete = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Delete.ml"; end;
    module Fold :
      sig INCLUDE "camlp4/Camlp4/Struct/Grammar/Fold.mli"; end =
      struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Fold.ml"; end;
    module Entry = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Entry.ml"; end;
    module Static = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Static.ml"; end;
    module Dynamic = struct INCLUDE "camlp4/Camlp4/Struct/Grammar/Dynamic.ml"; end;
  end;
end;

module Printers = struct
  module DumpCamlp4Ast :
    sig INCLUDE "camlp4/Camlp4/Printers/DumpCamlp4Ast.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Printers/DumpCamlp4Ast.ml"; end;
  module DumpOCamlAst :
    sig INCLUDE "camlp4/Camlp4/Printers/DumpOCamlAst.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Printers/DumpOCamlAst.ml"; end;
  module Null :
    sig INCLUDE "camlp4/Camlp4/Printers/Null.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Printers/Null.ml"; end;
  module OCaml :
    sig INCLUDE "camlp4/Camlp4/Printers/OCaml.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Printers/OCaml.ml"; end;
  module OCamlr :
    sig INCLUDE "camlp4/Camlp4/Printers/OCamlr.mli"; end =
    struct INCLUDE "camlp4/Camlp4/Printers/OCamlr.ml"; end;
end;

module OCamlInitSyntax = struct INCLUDE "camlp4/Camlp4/OCamlInitSyntax.ml"; end;
module PreCast : sig INCLUDE "camlp4/Camlp4/PreCast.mli"; end = struct INCLUDE "camlp4/Camlp4/PreCast.ml"; end;
module Register : sig INCLUDE "camlp4/Camlp4/Register.mli"; end = struct INCLUDE "camlp4/Camlp4/Register.ml"; end;
