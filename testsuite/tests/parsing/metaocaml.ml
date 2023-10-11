(* TEST
 flags = "-stop-after parsing -dsource -dparsetree -dno-locations";
 ocamlparam=",_,metaocaml-mode=1";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(** In the MetaOCaml mode, the lexer always emits the DOTTILDE token for .~*)
let escape = .~ x

(** Contrarily, >. is only translated to the MetaOCaml token after the first
    .> *)
let (>.): float -> float -> bool = Stdlib.(>)
let _ = 1. >. 4.;;

let 45 =
  let (>.) = fun x y -> x - y and (>.>) = fun x y -> x * y
  and (>.>.) = fun x y -> 2*x + y in
  ((3 >. 1) >.>. 1) >.> (13 >. 4);;


(** The tests below should reprint MetaOCaml constructs, but they
    currently output the extension syntax as MetaOCaml pretty-printing
    support was not upstreamed yet. *)
let test = .< x >. ;;

let test2 = .~ x

(** Test that the extension syntax is normalized to the constructor version.
    (this test will not produce the expected output before pretty-printing
     support is upstreamed)*)
let _normalize = [%metaocaml.escape y]
let _normalize = [%metaocaml.bracket z]
