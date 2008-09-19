open Fancy_lambda_quot.LambdaSyntax;;
let _loc = Camlp4.PreCast.Loc.ghost;;
let rec propagate = function
  | << $f$ $x$ $y$ >> ->
      begin match propagate f, propagate x, propagate y with
      | f, << $int:i$ >>, << $int:j$ >> -> 
          begin match f with
          | << plus >>  -> << $int:i + j$ >>
          | << minus >> -> << $int:i - j$ >>
          | << times >> -> << $int:i * j$ >>
          | << div >>   -> << $int:i / j$ >>
          | _           -> << $f$ $int:i$ $int:j$ >>
          end
      | f, x, y -> << $f$ $x$ $y$ >>
      end
  | << $f$ $x$ >> -> << $propagate f$ $propagate x$ >>
  | << fun $x$ -> $e$ >> -> << fun $x$ -> $propagate e$ >> (* here x should not be a primitive like plus *)
  | << $var:_$ >> | << $int:_$ >> as e -> e
;;

let ex1 = propagate << f (fun x -> g (plus 3 (times 4 42)) (minus 1 (x 3))) >>
;;
