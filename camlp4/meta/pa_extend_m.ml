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

value psymbol p s t =
  let symb = {used = []; text = s; styp = fun _ -> t} in
  {pattern = Some p; symbol = symb}
;

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
          let text n =
            let rl =
              let r1 =
                let prod =
                  let n = mk_name loc <:expr< anti_list >> in
                  [psymbol <:patt< a >> (snterm loc n None)
                     <:ctyp< 'anti_list >>]
                in
                let act = <:expr< a >> in {prod = prod; action = Some act}
              in
              let r2 =
                let psymb =
                  let symb =
                    {used = []; text = slist loc min sep s;
                     styp = fun n -> <:ctyp< list $s.styp n$ >>}
                  in
                  let patt = <:patt< l >> in
                  {pattern = Some patt; symbol = symb}
                in
                let act = <:expr< list l >> in
                {prod = [psymb]; action = Some act}
              in
              [r1; r2]
            in
            srules loc "anti" rl n
          in
          {used = used; text = text; styp = fun _ -> <:ctyp< ast >>} ] ]
  ;
END;
