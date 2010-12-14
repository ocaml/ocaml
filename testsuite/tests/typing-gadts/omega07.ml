(*
   An attempt at encoding omega examples from the 2nd Central European
   Functional Programming School:
     Generic Programming in Omega, by Tim Sheard and Nathan Linger
          http://web.cecs.pdx.edu/~sheard/
*) 

type ('a,'b) sum = Inl of 'a | Inr of 'b

type zero
type _ succ
type _ nat =
  | NZ : zero nat
  | NS : 'a nat -> 'a succ nat
;;

type _ rep =
  | Rint : int rep
  | Rchar : char rep
  | Runit : unit rep
  | Rpair : 'a rep * 'b rep -> ('a * 'b) rep
  | Rsum : 'a rep * 'b rep -> ('a,'b) sum rep

let t1 = Rsum (Rpair (Rint, Rchar), Rpair (Runit, Rint))
;;
let rec sumR : type a. a rep -> a -> int = fun r x ->
  match r, x with
  | Rint, n -> n
  | Rpair(r,s), (x,y) -> sumR r x + sumR s y
  | Rsum(r,s), Inl x -> sumR r x
  | Rsum(r,s), Inr x -> sumR s x
  | _ -> 0
;;
let rec sum2 : type a. a rep -> a -> int = fun r ->
  match r with
  | Rint -> (fun n -> n)
  | Rpair(r,s) ->
    let sumr = sum2 r and sums = sum2 s in (fun (x,y) -> sumr x + sums y)
  | Rsum(r,s) ->
    let sumr = sum2 r and sums = sum2 s in
    (function Inl x -> sumr x | Inr x -> sums x)
  | _ -> (fun _ -> 0)
;;
type (_,_) seq =
  | Snil  : ('a,zero) seq
  | Scons : 'a * ('a,'n) seq -> ('a, 'n succ) seq
;;
type tp
type nd
type (_,_) fk
type _ shape =
  | Tp : tp shape
  | Nd : nd shape
  | Fk : 'a shape * 'b shape -> ('a,'b) fk shape
;;
type tt
type ff
type _ boolean =
  | BT : tt boolean
  | BF : ff boolean
;;
type (_,_) path =
  | Pnone : 'a -> (tp,'a) path
  | Phere : (nd,'a) path
  | Pleft : ('x,'a) path -> (('x,'y) fk, 'a) path
  | Pright : ('y,'a) path -> (('x,'y) fk, 'a) path
;;
type (_,_) tree =
  | Ttip  : (tp,'a) tree
  | Tnode : 'a -> (nd,'a) tree
  | Tfork : ('x,'a) tree * ('y,'a) tree -> (('x,'y)fk, 'a) tree
;;
let tree1 = Tfork (Tfork (Ttip, Tnode 4), Tfork (Tnode 4, Tnode 3))
;;
let rec find : type sh.
    ('a -> 'a -> bool) -> 'a -> (sh,'a) tree -> (sh,'a) path list
  = fun eq n t ->
    match t with
    | Ttip -> []
    | Tnode m ->
        if eq n m then [Phere] else []
    | Tfork (x, y) ->
        List.map (fun x -> Pleft x) (find eq n x) @
        List.map (fun x -> Pright x) (find eq n y)
;;
let rec extract : type sh. (sh,'a) path -> (sh,'a) tree -> 'a = fun p t ->
  match (p, t) with
  | Pnone x, Ttip -> x
  | Phere, Tnode y -> y
  | Pleft p, Tfork(l,_) -> extract p l
  | Pright p, Tfork(_,r) -> extract p r
;;
type (_,_,_) plus =
  | PlusZ : 'a nat -> (zero, 'a, 'a) plus
  | PlusS : ('a,'b,'c) plus -> ('a succ, 'b, 'c succ) plus
;;
type (_,_) le =
  | LeZ : 'a nat -> (zero, 'a) le
  | LeS : ('n, 'm) le -> ('n succ, 'm succ) le
;;
type _ even =
  | EvenZ : zero even
  | EvenSS : 'n even -> 'n succ succ even
;;
type one = zero succ
type two = one succ
type three = two succ
type four = three succ
;;
let even0 : zero even = EvenZ
let even2 : two even = EvenSS EvenZ
let even4 : four even = EvenSS (EvenSS EvenZ)
;;
let p1 : (two, one, three) plus = PlusS (PlusS (PlusZ (NS NZ)))
;;
let rec summandLessThanSum : type a b c. (a,b,c) plus -> (a,c) le = fun p ->
  match p with
  | PlusZ n -> LeZ n
  | PlusS p' -> LeS (summandLessThanSum p')
;;


type (_,_) equal = Eq : ('a,'a) equal

let rec sameNat : type a b. a nat -> b nat -> (a,b) equal option = fun a b ->
  match a, b with
  | NZ, NZ -> Some Eq
  | NS a', NS b' ->
      begin match sameNat a' b' with
      | Some Eq -> Some (Eq : (a, b) equal)
      | None -> None
      end
  | _ -> None
;;

(* AVL trees *)

type (_,_,_) balance =
  | Less : ('h, 'h succ, 'h succ) balance
  | Same : ('h, 'h, 'h) balance
  | More : ('h succ, 'h, 'h succ) balance

type _ avl =
  | Leaf : zero avl
  | Node :
      ('hL, 'hR, 'hMax) balance * 'hL avl * int * 'hR avl -> 'hMax succ avl

type avl' = Avl : 'h avl -> avl'
;;

let empty = Avl Leaf

let rec elem : type h. int -> h avl -> bool = fun x t ->
  match t with
  | Leaf -> false
  | Node (_, l, y, r) ->
      x = y || if x < y then elem x l else elem x r
;;

let rec rotr : type n. (n succ succ) avl -> int -> n avl ->
  ((n succ succ) avl, (n succ succ succ) avl) sum =
  fun tL y tR ->
  match tL with
  | Node (Same, a, x, b) -> Inr (Node (Less, a, x, Node (More, b, y, tR)))
  | Node (More, a, x, b) -> Inl (Node (Same, a, x, Node (Same, b, y, tR)))
  | Node (Less, a, x, Node (Same, b, z, c)) ->
      Inl (Node (Same, Node (Same, a, x, b), z, Node (Same, c, y, tR)))
  | Node (Less, a, x, Node (Less, b, z, c)) ->
      Inl (Node (Same, Node (More, a, x, b), z, Node (Same, c, y, tR)))
  | Node (Less, a, x, Node (More, b, z, c)) ->
      Inl (Node (Same, Node (Same, a, x, b), z, Node (Less, c, y, tR)))
;;
let rec rotl : type n. n avl -> int -> (n succ succ) avl ->
  ((n succ succ) avl, (n succ succ succ) avl) sum =
  fun tL u tR ->
  match tR with
  | Node (Same, a, x, b) -> Inr (Node (More, Node (Less, tL, u, a), x, b))
  | Node (Less, a, x, b) -> Inl (Node (Same, Node (Same, tL, u, a), x, b))
  | Node (More, Node (Same, a, x, b), y, c) ->
      Inl (Node (Same, Node (Same, tL, u, a), x, Node (Same, b, y, c)))
  | Node (More, Node (Less, a, x, b), y, c) ->
      Inl (Node (Same, Node (More, tL, u, a), x, Node (Same, b, y, c)))
  | Node (More, Node (More, a, x, b), y, c) ->
      Inl (Node (Same, Node (Same, tL, u, a), x, Node (Less, b, y, c)))
;;
let rec ins : type n. int -> n avl -> (n avl, (n succ) avl) sum =
  fun x t ->
  match t with
  | Leaf -> Inr (Node (Same, Leaf, x, Leaf))
  | Node (bal, a, y, b) ->
      if x = y then Inl t else
      if x < y then begin
        match ins x a with
        | Inl a -> Inl (Node (bal, a, y, b))
        | Inr a ->
            match bal with
            | Less -> Inl (Node (Same, a, y, b))
            | Same -> Inr (Node (More, a, y, b))
            | More -> rotr a y b
      end else begin
        match ins x b with
        | Inl b -> Inl (Node (bal, a, y, b) : n avl)
        | Inr b ->
            match bal with
            | More -> Inl (Node (Same, a, y, b) : n avl)
            | Same -> Inr (Node (Less, a, y, b) : n succ avl)
            | Less -> rotl a y b
      end
;;

let insert x (Avl t) =
  match ins x t with
  | Inl t -> Avl t
  | Inr t -> Avl t
;;

let rec del_min : type n. (n succ) avl -> int * (n avl, (n succ) avl) sum =
  function
  | Node (Less, Leaf, x, r) -> (x, Inl r)
  | Node (Same, Leaf, x, r) -> (x, Inl r)
  | Node (bal, Node (v, a, y, b) , x, r) ->
      (* Cannot write (Node _ as l) *)
      match del_min (Node (v, a, y, b)) with
      | y, Inr l -> (y, Inr (Node (bal, l, x, r)))
      | y, Inl l ->
          (y, match bal with
          | Same -> Inr (Node (Less, l, x, r))
          | More -> Inl (Node (Same, l, x, r))
          | Less -> rotl l x r)

type _ avl_del =
  | Dsame : 'n avl -> 'n avl_del
  | Ddecr : ('m succ, 'n) equal * 'm avl -> 'n avl_del

let rec del : type n. int -> n avl -> n avl_del = fun y t ->
  match t with
  | Leaf -> Dsame Leaf
  | Node (bal, l, x, r) ->
      if x = y then begin
        match r with
        | Leaf ->
            begin match bal with
            | Same -> Ddecr (Eq, l)
            | More -> Ddecr (Eq, l)
            end
        | Node _ ->
            begin match bal, del_min r with
            | _, (z, Inr r) -> Dsame (Node (bal, l, z, r))
            | Same, (z, Inl r) -> Dsame (Node (More, l, z, r))
            | Less, (z, Inl r) -> Ddecr (Eq, Node (Same, l, z, r))
            | More, (z, Inl r) ->
                match rotr l z r with
                | Inl t -> Ddecr (Eq, t)
                | Inr t -> Dsame t
            end
      end else if y < x then begin
        match del y l with
        | Dsame l -> Dsame (Node (bal, l, x, r))
        | Ddecr(Eq,l) ->
            begin match bal with
            | Same -> Dsame (Node (Less, l, x, r))
            | More -> Ddecr (Eq, Node (Same, l, x, r))
            | Less ->
                match rotl l x r with
                | Inl t -> Ddecr (Eq, t)
                | Inr t -> Dsame t
            end
      end else begin
        match del y r with
        | Dsame r -> Dsame (Node (bal, l, x, r))
        | Ddecr(Eq,r) ->
            begin match bal with
            | Same -> Dsame (Node (More, l, x, r))
            | Less -> Ddecr (Eq, Node (Same, l, x, r))
            | More ->
                match rotr l x r with
                | Inl t -> Ddecr (Eq, t)
                | Inr t -> Dsame t
            end
      end
;;

let delete x (Avl t) =
  match del x t with
  | Dsame t -> Avl t
  | Ddecr (_, t) -> Avl t
;;


(* Red-black trees *)

type red
type black
type (_,_) sub_tree =
  | Bleaf : (black, zero) sub_tree
  | Rnode :
      (black, 'n) sub_tree * int * (black, 'n) sub_tree -> (red, 'n) sub_tree
  | Bnode :
      ('cL, 'n) sub_tree * int * ('cR, 'n) sub_tree -> (black, 'n succ) sub_tree

type rb_tree = Root : (black, 'n) sub_tree -> rb_tree
;;

type dir = LeftD | RightD

type (_,_) ctxt =
  | CNil : (black,'n) ctxt
  | CRed : int * dir * (black,'n) sub_tree * (red,'n) ctxt -> (black,'n) ctxt
  | CBlk : int * dir * ('c1,'n) sub_tree * (black, 'n succ) ctxt -> ('c,'n) ctxt
;;

let blacken = function
    Rnode (l, e, r) -> Bnode (l, e, r)

type _ crep =
  | Red : red crep
  | Black : black crep

let color : type c n. (c,n) sub_tree -> c crep = function
  | Bleaf -> Black
  | Rnode _ -> Red
  | Bnode _ -> Black
;;

let rec fill : type c n. (c,n) ctxt -> (c,n) sub_tree -> rb_tree =
  fun ct t ->
  match ct with
  | CNil -> Root t
  | CRed (e, LeftD, uncle, c) -> fill c (Rnode (uncle, e, t))
  | CRed (e, RightD, uncle, c) -> fill c (Rnode (t, e, uncle))
  | CBlk (e, LeftD, uncle, c) -> fill c (Bnode (uncle, e, t))
  | CBlk (e, RightD, uncle, c) -> fill c (Bnode (t, e, uncle))
;;
let recolor d1 pE sib d2 gE uncle t =
  match d1, d2 with
  | LeftD, RightD -> Rnode (Bnode (sib, pE, t), gE, uncle)
  | RightD, RightD -> Rnode (Bnode (t, pE, sib), gE, uncle)
  | LeftD, LeftD -> Rnode (uncle, gE, Bnode (sib, pE, t))
  | RightD, LeftD -> Rnode (uncle, gE, Bnode (t, pE, sib))
;;
let rotate d1 pE sib d2 gE uncle (Rnode (x, e, y)) =
  match d1, d2 with
  | RightD, RightD -> Bnode (Rnode (x,e,y), pE, Rnode (sib, gE, uncle))
  | LeftD,  RightD -> Bnode (Rnode (sib, pE, x), e, Rnode (y, gE, uncle))
  | LeftD,  LeftD  -> Bnode (Rnode (uncle, gE, sib), pE, Rnode (x,e,y))
  | RightD, LeftD  -> Bnode (Rnode (uncle, gE, x), e, Rnode (y, pE, sib))
;;
let rec repair : type c n. (red,n) sub_tree -> (c,n) ctxt -> rb_tree =
  fun t ct ->
  match ct with
  | CNil -> Root (blacken t)
  | CBlk (e, LeftD, sib, c) -> fill c (Bnode (sib, e, t))
  | CBlk (e, RightD, sib, c) -> fill c (Bnode (t, e, sib))
  | CRed (e, dir, sib, CBlk (e', dir', uncle, ct)) ->
      match color uncle with
      | Red -> repair (recolor dir e sib dir' e' (blacken uncle) t) ct
      | Black -> fill ct (rotate dir e sib dir' e' uncle t)
;;
let rec ins : type c n. int -> (c,n) sub_tree -> (c,n) ctxt -> rb_tree =
  fun e t ct ->
  match t with
  | Rnode (l, e', r) ->
      if e < e' then ins e l (CRed (e', RightD, r, ct))
                else ins e r (CRed (e', LeftD, l, ct))
  | Bnode (l, e', r) ->
      if e < e' then ins e l (CBlk (e', RightD, r, ct))
                else ins e r (CBlk (e', LeftD, l, ct))
  | Bleaf -> repair (Rnode (Bleaf, e, Bleaf)) ct
;;
let insert e (Root t) = ins e t CNil
;;
