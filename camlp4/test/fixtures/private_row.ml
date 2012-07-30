

module type Ops = sig
     type expr
  val eval : expr -> int
  end
  ;;


module Plus = struct
     type 'a expr0 = [`Num of int | `Plus of 'a * 'a ]
     module F(X : Ops with type expr = private ([> 'a expr0] as 'a)) =
        struct
           type expr = X.expr expr0
           let eval : expr -> int = function
                 `Num n -> n
                |`Plus(e1,e2) -> X.eval e1 + X.eval e2
        end
    module rec L : (Ops with type expr = L.expr expr0) = F(L)
  end
  ;;


open Printf
;;

let _ = Printf.printf "%d\n%!" (Plus.L.eval (`Plus ((`Num 5),(`Num 2))));;
