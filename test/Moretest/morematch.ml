(*
  More tests for pattern matching
*)

let test msg f arg r =
  if f arg <> r then begin
    prerr_endline msg ;
    failwith "Malaise"
  end
;;

type t = A | B | C | D | E | F
  ;;

let f x = match x with
| A | B | C -> 1
| D | E -> 2
| F -> 3;;

test "un" f C 1 ;
test "un" f D 2 ;
test "un" f F 3 ; ()
;;

let g x = match x with
  1 -> 1
| 2 -> 2
| 3 -> 3
| 4 | 5 -> 4
| 6 -> 5
| 7 | 8 -> 6
| 9 -> 7
;;

test "deux" g 5 4 ;
test "deux" g 6 5 ;
test "deux" g 9 7 ; ()
;;

  
let g x = match x with
  1 -> 1
| 2 -> 2
| 3 -> 3
| 4 | 5 -> 4
| 6 -> 5   
| 7 | 8 -> 6
| 9 -> 7
| _ -> 8;;

test "trois" g 10 8
;;

let g x= match  x with
  1 -> 1
| 2 -> 2
| 3 -> 3
| 4 | 5 -> 4
| 6 -> 5   
| 4|5|7 -> 100
| 7 | 8 -> 6
| 9 -> 7
| _ -> 8;;
test "quatre" g 4 4 ;
test "quatre" g 7 100 ; ()
;;


let h x =
 match x with
   (1,1) -> 1
| (2|3), 1 -> 2
| 2,(2|3) -> 3
| (4,4) -> 5
| _ -> 100
;;

test "cinq" h (2,2) 3 ;
test "cinq" h (2,1) 2 ;
test "cinq" h (2,4) 100 ; ()
;;

(* idem hh (2,5) *)

let hh x = match x with
| 1,1 -> 1
| 2,1 -> 2
| (2|3),(1|2|3|4) -> 3
| 2,5 -> 4
| (4,4) -> 5
| _ -> 100
;;

let hhh x = match x with
| 1,1 -> 1
| (2|3),1 -> 2
| 2,2 -> 3
| _ -> 100
;;

let h x =
 match x with
   (1,1) -> 1
| 3,1 -> 2
| 2,(2|3) -> 3
| (4,4) -> 5
| _ -> 100
;;

let h x = match x with
  1 -> 1
| 2|3 -> 2
| 4 -> 4
| 5 -> 5
| 6|7 -> 6
| 8 -> 8
| _ -> 100
;;
let f x = match x with
| ((1|2),(3|4))|((3|4),(1|2)) -> 1
| (3,(5|6)) -> 2
| _ -> 3
;;

test "six" f (1,3) 1 ;
test "six" f (3,2) 1 ;
test "six" f (3,5) 2 ;
test "six" f (3,7) 3 ; ()
;;

type tt = {a : bool list ; b : bool}

let f = function
  | {a=([]|[true])} -> 1
  | {a=false::_}|{b=(true|false)}    -> 2
;;

test "sept" f {a=[] ; b = true} 1 ;
test "sept" f {a=[true] ; b = false} 1 ;
test "sept" f {a=[false ; true] ; b = true} 2 ;
test "sept" f {a=[false] ; b = false} 2 ; ()
;;

let f = function
  | (([]|[true]),_) -> 1
  | (false::_,_)|(_,(true|false)) -> 2
;;

test "huit" f ([],true) 1 ;
test "huit" f ([true],false) 1 ;
test "huit" f ([false ; true], true) 2 ;
test "huit" f ([false], false) 2 ; ()
;;


let split_cases = function
   | `Nil | `Cons _ as x -> `A x
   | `Snoc _ as x -> `B x
;;

test "oubli" split_cases `Nil (`A `Nil);
test "oubli" split_cases (`Cons 1) (`A (`Cons 1));
test "oubli" split_cases (`Snoc 1) (`B (`Snoc 1)) ; ()
;;

type t1 = A of int | B of int
let f1 = function
  | (A x | B x) -> x
;;

test "neuf" f1 (A 1) 1 ;
test "neuf" f1 (B 1) 1 ;
;;

type coucou = A of int | B of int * int | C
;;


let g = function
  | (A x | B (_,x)) -> x
  | C -> 0
;;


test "dix" g (A 1) 1 ;
test "dix" g (B (1,2)) 2 ;
;;



let h = function
  | ([x]|[1 ; x ]|[1 ; 2 ; x]) -> x
  | _ -> 0
;;

test "encore" h [1] 1 ;
test "encore" h [1;2] 2 ;
test "encore" h [1;2;3] 3 ;
test "encore" h [0 ; 0] 0 ; ()
;;

let f = function
| (x,(0 as y)) | (y,x) -> y-x
;;

test "foo" f (1,0) (-1);
test "foo" f (1,2) (-1)
;;


let f = function (([]|[_]) as x)|(_::([] as x))|(_::_::x)  -> x
;;

test "zob" f [] [] ;
test "zob" f [1] [1] ;
test "zob" f [1;2;3] [3]
;;


type zob = A | B | C | D of zob * int | E of zob * zob

let rec f = function
  | (A | B | C) -> A
  | D (x,i) -> D (f x,i)
  | E (x,_) -> D (f x,0)
;;


test "fin" f B A ;
test "fin" f (D (C,1)) (D (A,1)) ;
test "fin" f (E (C,A)) (D (A,0)) ; ()
;;

type length = 
    Char of int | Pixel of int | Percent of int | No of string | Default

let length = function
  | Char n -> n | Pixel n -> n
  | _       -> 0
;;

test "length" length (Char 10) 10 ;
test "length" length (Pixel 20) 20 ;
test "length" length Default 0 ;
test "length" length (Percent 100) 0 ; ()
;;

let length2 = function
  | Char n -> n | Percent n -> n
  | _       -> 0
;;

test "length2" length2 (Char 10) 10 ;
test "length2" length2 (Pixel 20) 0 ;
test "length2" length2 Default 0 ;
test "length2" length2(Percent 100) 100 ; ()
;;

let length3 = function
  | Char _ | No _ -> true
  | _ -> false
;;

test "length3" length3 (Char 10) true ;
test "length3" length3 (No "") true ;
test "length3" length3 (Pixel 20) false ;
test "length3" length3 Default false ;
test "length3" length3(Percent 100) false ; ()
;;

type hevea = A | B | C

let h x = match x with
| A -> 1
| B|C -> 2
;;

test "hevea" h A 1 ;
test "hevea" h B 2 ;
test "hevea" h B 2 ; ()
;;
type lambda =
    Lvar of int
  | Lconst of int
  | Lapply of lambda * lambda list
  | Lfunction of bool  * int list * lambda
  | Llet of  bool * int * lambda * lambda
  | Lletrec of (int * lambda) list * lambda
  | Lprim of string * lambda list
  | Lswitch of lambda * lambda_switch
  | Lstaticfail
  | Lcatch of lambda * lambda
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * int list) * lambda
  | Ltrywith of lambda * int * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of int * lambda * lambda * bool * lambda
  | Lassign of int * lambda
  | Lsend of lambda * lambda * lambda list
  | Levent of lambda * lambda_event
  | Lifused of int * lambda
and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * lambda) list;     (* Tag block cases *)
    sw_checked: bool ;                  (* True if bound checks needed *)
    sw_nofail: bool}                    (* True if should not fail *)
and lambda_event =
  { lev_loc: int;
    lev_kind: bool ;
    lev_repr: int ref option;
    lev_env: int list }

let rec approx_present v l = true

let rec lower_bind v arg lam = match lam with
| Lifthenelse (cond, ifso, ifnot) -> 1
| Lswitch (ls,({sw_consts=[i,act] ; sw_blocks = []} as sw))
    when not (approx_present v ls) -> 2
| Lswitch (ls,({sw_consts=[] ; sw_blocks = [i,act]} as sw))
    when not (approx_present v ls) -> 3
| Llet (true , vv, lv, l) -> 4
| _ -> 5
;;

test "lower_bind" (lower_bind 0 0) (Llet (true,0, Lvar 1, Lvar 2)) 4 ;
test "lower_bind" (lower_bind 0 0) (Lvar 0) 5 ;
test "lower_bind" (lower_bind 0 0) (Lifthenelse (Lvar 0, Lvar 1, Lvar 2)) 1
;;


type field_kind =
    Fvar of field_kind option ref
  | Fpresent
  | Fabsent

let unify_kind (k1, k2) =  match k1, k2 with
    (Fvar r, (Fvar _ | Fpresent))             -> 1
  | (Fpresent, Fvar r)                        -> 2
  | (Fpresent, Fpresent)                      -> 3
  | _                                         -> 4


let r = ref (Some Fpresent)
;;

test "unify"  unify_kind (Fvar r, Fpresent) 1 ;
test "unify"  unify_kind (Fvar r, Fvar r) 1 ;
test "unify"  unify_kind (Fvar r, Fabsent) 4 ;
test "unify"  unify_kind (Fpresent, Fvar r) 2 ;
test "unify"  unify_kind (Fpresent, Fpresent) 3 ;
test "unify"  unify_kind (Fabsent, Fpresent) 4 ; ()
;;


type youyou = A | B | C | D of youyou

let foo (k1, k2) = match k1,k2 with
| D _, (A|D _) -> 1
| (A|B),D _ -> 2
| C,_       -> 3
| _, (A|B|C) -> 4
;;

test "foo" foo (D A,A) 1 ;
test "foo" foo (D A,B) 4 ;
test "foo" foo (A,A) 4 ; ()
;;

type yaya = A | B
;;

let yaya = function
| A,_,_ -> 1
| _,A,_ -> 2
| B,B,_ -> 3
| A,_,(100|103) -> 5
;;

test "yaya" yaya (A,A,0) 1 ;
test "yaya" yaya (B,A,0) 2 ;
test "yaya" yaya (B,B,100) 3 ; ()
;;


let yoyo =  function
| [],_,_ -> 1
| _,[],_ -> 2
| _::_,_::_,_ -> 3
| [],_,(100|103|104) -> 5
| [],_,(100|103) -> 6
| [],_,(1000|1001|1002|20000) -> 7
;;

test "yoyo" yoyo ([],[],0) 1 ;
test "yoyo" yoyo ([1],[],0) 2 ;
test "yoyo" yoyo ([1],[1],100) 3 ; ()
;;

let youyou = function
  | (100|103|104) -> 1
  | (100|103|101) -> 2
  | (1000|1001|1002|20000) -> 3
  | _ -> -1
;;

test "youyou" youyou 100 1 ;
test "youyou" youyou 101 2 ;
test "youyou" youyou 1000 3
;;

type autre =
  |  C | D | E of autre | F of autre * autre | H of autre | I | J | K of string

let rec autre = function
| C,_,_ -> 1
| _,C,_ -> 2
| D,D,_ -> 3
| (D|F (_,_)|H _|K _),_,_ -> 4
| (_, (D|I|E _|F (_, _)|H _|K _), _) -> 8
| (J,J,((C|D) as x |E x|F (_,x))) | (J,_,((C|J) as x)) -> autre (x,x,x)
| (J, J, (I|H _|K _)) -> 9
| I,_,_ -> 6
| E _,_,_ -> 7
;;

test "autre" autre (J,J,F (D,D)) 3 ;
test "autre" autre (J,J,D) 3 ;
test "autre" autre (J,J,I) 9 ;
test "autre" autre (H I,I,I) 4 ;
test "autre" autre (J,J,H I) 9 ; ()
;;


type youpi = YA | YB | YC
and hola = X | Y | Z | T of hola | U of hola | V of hola

let xyz = function
| YA,_,_ -> 1
| _,YA,_ -> 2
| YB,YB,_ -> 3
| ((YB|YC), (YB|YC), (X|Y|Z|V _|T _)) -> 6
| _,_,(X|U _) -> 8
| _,_,Y -> 5
;;

test "xyz" xyz (YC,YC,X) 6 ;
test "xyz" xyz (YC,YB,U X) 8 ;
test "xyz" xyz (YB,YC,X) 6 ; ()
;;


(* Ce test est pour le compilo lui-meme *)
let eq (x,y) = x=y
;;

test "eq" eq ("coucou", "coucou") true ; ()
;;

(* Test des gardes, non trivial *)

let is_none = function
  | None -> true
  | _ -> false

let garde x = match x with
| (Some _, _) when is_none (snd x) -> 1
| (Some (pc, _), Some pc') when pc = pc' -> 2
| _ -> 3
;;

test "garde" garde (Some (1,1),None) 1 ;
test "garde" garde (Some (1,1),Some 1) 2 ;
test "garde" garde (Some (2,1),Some 1) 3 ; ()
;;

let orstring = function
  | ("A"|"B"|"C") -> 2
  | "D" -> 3
  | _ -> 4
;;

test "orstring" orstring "A" 2 ;
test "orstring" orstring "B" 2 ;
test "orstring" orstring "C" 2 ;
test "orstring" orstring "D" 3 ;
test "orstring" orstring "E" 4 ; ()
;;

type var_t = [`Variant of [ `Some of string | `None | `Foo] ]

let crash (pat:var_t) =
      match pat with
      | `Variant (`Some tag) -> tag
      | `Variant (`None) -> "none"
      | _ -> "foo"

;;

test "crash" crash (`Variant `None) "none" ;
test "crash" crash (`Variant (`Some "coucou")) "coucou" ;
test "crash" crash (`Variant (`Foo)) "foo" ; ()
;;

let flatgarde c =
let x,y = c in
match x,y with
| (1,2)|(2,3) when y=2 -> 1
| (1,_)|(_,3) -> 2
| _ -> 3
;;

test "flatgarde" flatgarde (1,2) 1 ;
test "flatgarde" flatgarde (1,3) 2 ;
test "flatgarde" flatgarde (2,3) 2 ;
test "flatgarde" flatgarde (2,4) 3 ; ()
;;


