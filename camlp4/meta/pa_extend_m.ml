(* camlp4r pa_extend.cmo q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Pa_extend;

EXTEND
  symbol: LEVEL "top"
    [ NONA
      [ min = [ UIDENT "SLIST0" -> False | UIDENT "SLIST1" -> True ];
        s = SELF; sep = OPT [ UIDENT "SEP"; t = symbol -> t ] ->
          let used =
            match sep with
            [ Some symb -> [mk_name loc <:expr< anti >> :: symb.used @ s.used]
            | None -> s.used ]
          in
          {used = used; text = sslist loc min sep s; styp = STlid loc "ast"}
      | "SOPT"; s = SELF ->
          {used = s.used; text = ssopt loc s; styp = STlid loc "ast"} ] ]
  ;
END;
