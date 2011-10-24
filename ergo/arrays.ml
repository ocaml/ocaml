(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2010                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*     Alain Mebsout                                                      *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

open Options
open Format
  
module Sy = Symbols
module T  = Term
module A  = Literal
module L  = List
  
type 'a abstract = unit

module type ALIEN = sig
  include Sig.X
  val embed : r abstract -> r
  val extract : r -> (r abstract) option
end

module Make(X : ALIEN) = struct

  type t = X.r abstract
  type r = X.r

  let name           = "Farrays"
  let unsolvable _   = assert false 
  let is_mine_a _    = false
  let is_mine_symb _ = false
  let is_mine_type _ = false
  let type_info _    = assert false
  let color _        = assert false
  let print _ _      = assert false
  let embed _        = assert false
  let compare _ _    = assert false
  let leaves _       = assert false
  let subst _ _ _    = assert false 
  let make _         = assert false
  let solve _ _ _    = assert false


  (* == La partie Relation de Arrays =====================================*)
  module Rel = struct
    type r = X.r
    
    (* map get |-> { set } des associations (get,set) deja splites *)
    module TM = struct
      include T.Map
      let update t a mp = 
        try add t (T.Set.add a (find t mp)) mp
        with Not_found -> add t (T.Set.singleton a) mp
      let splited t a mp = try T.Set.mem a (find t mp) with Not_found -> false
    end

    (* ce module fournit une fonction de comparaison pour A.Eq et A.Neq 
       encodant la symetrie *)
    module Argu = struct
      type t = r A.view

      let compare a1 a2 = 
        match a1, a2 with
          | A.Eq(r1,r2) , A.Eq(s1,s2)  -> 
              let r1,r2 = if X.compare r1 r2 > 0 then r1,r2 else r2,r1 in
              let s1,s2 = if X.compare s1 s2 > 0 then s1,s2 else s2,s1 in
              let c = X.compare r1 s1 in
              if c = 0 then X.compare r2 s2 else c
          | A.Neq(r1,r2), A.Neq(s1,s2) -> 
              let r1,r2 = if X.compare r1 r2 > 0 then r1,r2 else r2,r1 in
              let s1,s2 = if X.compare s1 s2 > 0 then s1,s2 else s2,s1 in
              let c = X.compare r1 s1 in
              if c = 0 then X.compare r2 s2 else c
          | A.Eq _      , A.Neq _      -> 1
          | A.Neq _     , A.Eq _       -> -1
          | A.Builtin _ , _            -> assert false
          | _           , A.Builtin _  -> assert false
    end
      
    (* ensemble d'egalites/disegalites sur des atomes semantiques *)
    module Aset= Set.Make(Argu)


    (* map k |-> {sem Atom} d'egalites/disegalites sur des atomes semantiques*)
    module Amap = struct
      include Map.Make(Argu)
      let find k mp = try find k mp with Not_found -> A.Set.empty
      let add k v mp = add k (A.Set.add v (find k mp)) mp
    end

    (* ensemble de termes "get" avec leurs arguments et leurs types*)
    type gtype = {g:T.t; gt:T.t; gi:T.t; gty:Ty.t}
    module G :Set.S with type elt = gtype = Set.Make
      (struct type t = gtype let compare t1 t2 = T.compare t1.g t2.g end)

    (* ensemble de termes "set" avec leurs arguments et leurs types *)
    type stype = {s:T.t; st:T.t; si:T.t; sv:T.t; sty:Ty.t}
    module S :Set.S with type elt = stype = Set.Make
      (struct type t = stype let compare t1 t2 = T.compare t1.s t2.s end)

    (* map t |-> {set(t,-,-)} qui associe a chaque tableau l'ensemble 
       de ses affectations *)
    module TBS = struct
      include Map.Make(T)
      let find k mp = try find k mp with Not_found -> S.empty
        
      (* add reutilise find ci-dessus *)
      let add k v mp = add k (S.add v (find k mp)) mp
    end

    type t = 
        {gets  : G.t;            (* l'ensemble des "get" croises*)
         tbset : S.t TBS.t ;     (* map t |-> set(t,-,-) *)
         split : Aset.t;         (* l'ensemble des case-split possibles *)
         sps   : Aset.t;         (* atomes (=, <>) supposes par cc(X)*)
         csq   : A.Set.t Amap.t; (* consequences des splits *)
         seen  : T.Set.t TM.t    (* combinaisons (get,set) deja splitees *) }
          

    module Debug = struct

      let assume fmt la = 
        if debug_arrays && la <> [] then begin
          fprintf fmt "[Arrays.Rel] We assume@.";
          L.iter (fprintf fmt "  > %a@." (A.print_view X.print)) la;
        end

      let print_gets fmt = G.iter (fun t -> fprintf fmt "%a@." T.print t.g)
      let print_sets fmt = S.iter (fun t -> fprintf fmt "%a@." T.print t.s)
      let print_splits fmt = 
        Aset.iter (fun a -> fprintf fmt "%a@." (A.print_view X.print) a)
      let print_tbs fmt = 
        TBS.iter (fun k v -> fprintf fmt "%a --> %a@." T.print k print_sets v)

      let env fmt env = 
        if debug_arrays then begin
          fprintf fmt "-- gets ----------------------------------------@.";
          print_gets fmt env.gets;
          fprintf fmt "-- tabs of sets --------------------------------@.";
          print_tbs fmt env.tbset;
          fprintf fmt "-- splits --------------------------------------@.";
          print_splits fmt env.split;
          fprintf fmt "------------------------------------------------@."
        end

      let new_equalities fmt st = 
        if debug_arrays then 
          begin
            fprintf fmt "[Arrays] %d implied equalities@." (A.Set.cardinal st);
            A.Set.iter (fprintf fmt "  %a@." A.print) st
          end
    end

    (* retourne l'ensemble des feuilles d'un atome *)
    let leaves_of_atom = function
      | A.Eq (r1,r2) | A.Neq (r1,r2) -> 
          (X.leaves r2) @ (X.leaves r1)

      | A.Builtin (_,_,l)            ->
          L.fold_left (fun acc r -> (X.leaves r) @ acc) [] l
            
    (* met a jour gets et tbset en utilisant l'ensemble des termes donne*)
    let update_gets_sets st acc =
      T.Set.fold
        (fun t (gets,tbset) ->
           let {T.f=f;xs=xs;ty=ty} = T.view t in 
           match Sy.is_get f, Sy.is_set f, xs with
             | true , false, [a;i]   -> 
                 G.add {g=t; gt=a; gi=i; gty=ty} gets, tbset

             | false, true , [a;i;v] -> 
                 gets, TBS.add a {s=t; st=a; si=i; sv=v; sty=ty} tbset

             | false, false, _ -> 
                 (gets,tbset)

             | _  -> assert false
        ) st acc
        
    (* met a jour les composantes gets et tbset de env avec les termes 
       contenus dans les atomes de la *)
    let new_terms env la =
      let gets, tbset = 
        L.fold_left
          (fun acc a ->
             L.fold_left
               (fun acc r ->
                  match X.term_extract r with
                  | Some t -> update_gets_sets (T.subterms T.Set.empty t) acc
                  | None   -> acc
               ) acc (leaves_of_atom a)
          ) (env.gets,env.tbset) la
      in 
      {env with gets=gets; tbset=tbset}

        
    (* mise a jour de env avec les instances 
       1) i_j   => i_j_ded 
       2) i_n_j => i_n_j_ded *)
    let update_env env acc xi xj i_j i_j_ded i_n_j i_n_j_ded =
      match X.equal xi xj, Aset.mem i_j env.sps, Aset.mem i_n_j env.sps with
        | true , _, _ -> env, A.Set.add i_j_ded acc
            
        | false, true, false -> 
            let csq = Amap.add i_n_j i_n_j_ded env.csq in
            {env with csq = csq}, A.Set.add i_j_ded acc
              
        | false, false, true -> 
            let csq = Amap.add i_j i_j_ded env.csq in
            {env with csq = csq}, A.Set.add i_n_j_ded acc
              
        | false, false, false -> 
            let sp = Aset.add i_j env.split in
            let csq = Amap.add i_j i_j_ded env.csq in
            let csq = Amap.add i_n_j i_n_j_ded csq in
            { env with split = sp; csq = csq }, acc
              
        | false, true,  true  -> assert false

    (*----------------------------------------------------------------------
      get(set(-,-,-),-) modulo egalite
      ---------------------------------------------------------------------*)
    let get_of_set {g=get; gt=gtab; gi=gi; gty=gty} (env,acc) class_of = 
      L.fold_left
        (fun (env,acc) set -> 
           if TM.splited get set env.seen then (env,acc)
           else 
             let env = {env with seen = TM.update get set env.seen} in
             let {T.f=f;xs=xs;ty=sty} = T.view set in 
             match Sy.is_set f, xs with
               | true , [stab;si;sv] -> 
                   let xi, _ = X.make gi in
                   let xj, _ = X.make si in
                   let get_stab  = T.make (Sy.Op Sy.Get) [stab;gi] gty in
                   let i_j       = A.Eq(xi,xj) in
                   let i_j_ded   = A.make (A.Eq(get,sv)) in
                   let i_n_j     = A.Neq(xi,xj) in
                   let i_n_j_ded = A.make (A.Eq(get,get_stab)) in
                   update_env env acc xi xj i_j i_j_ded i_n_j i_n_j_ded
               | _ -> (env,acc)
        ) (env,acc) (class_of gtab)

    (*----------------------------------------------------------------------
      get(t,-) and set(t,-,-) modulo egalite
      ---------------------------------------------------------------------*)
    let get_and_set  {g=get; gt=gtab; gi=gi; gty=gty} (env,acc) class_of =
      let suff_sets = 
        L.fold_left
          (fun acc t -> S.union acc (TBS.find t env.tbset))
          S.empty (class_of gtab)
      in
      S.fold
        (fun  {s=set; st=stab; si=si; sv=sv; sty=sty} (env,acc) -> 
           if TM.splited get set env.seen then (env,acc)
           else 
             begin
               let env = {env with seen = TM.update get set env.seen} in
               let xi, _ = X.make gi in
               let xj, _ = X.make si in
               let get_stab  = T.make (Sy.Op Sy.Get) [stab;gi] gty in
               let gt_of_st  = T.make (Sy.Op Sy.Get) [set;gi] gty in
               let i_j       = A.Eq(xi,xj) in
               let i_j_ded   = A.make (A.Eq(gt_of_st,sv)) in
               let i_n_j     = A.Neq(xi,xj) in
               let i_n_j_ded = A.make (A.Eq(gt_of_st,get_stab)) in
               update_env env acc xi xj i_j i_j_ded i_n_j i_n_j_ded
             end
        ) suff_sets (env,acc)
        
    (* Generer de nouvelles instantiations de lemmes *)
    let new_splits env acc class_of = 
      G.fold
        (fun gt_info accu ->
           let accu = get_of_set gt_info accu class_of in
           get_and_set gt_info accu class_of
        ) env.gets (env,acc)
        
    let fresh_index = 
      let cpt = ref (-1) in
      fun ty -> 
        incr cpt; 
        let sy = Sy.name (Format.sprintf "!i_%d" !cpt) in
        Term.make sy [] ty

    (* XXX on peut faire mieux si on a directement le type *)
    let ext_2 acc t1 t2 = 
      let {T.f=f; xs=xs; ty=ty} = T.view t1 in
        match Sy.is_set f, xs with
          | true, [_;i;v] -> 
              let {T.ty=ty_i} = T.view i in
              let {T.ty=ty_v} = T.view v in
              let index = fresh_index ty_i in
              let g1 = T.make (Sy.Op Sy.Get) [t1;index] ty_v in
              let g2 = T.make (Sy.Op Sy.Get) [t2;index] ty_v in
              A.Set.add (A.make (A.Neq(g1,g2))) acc
          | _ ->
              let {T.f=f; xs=xs; ty=ty} = T.view t2 in
              match xs with
                | [_;i;v] -> 
                    let {T.ty=ty_i} = T.view i in
                    let {T.ty=ty_v} = T.view v in
                    let index = fresh_index ty_i in
                    let g1 = T.make (Sy.Op Sy.Get) [t1;index] ty_v in
                    let g2 = T.make (Sy.Op Sy.Get) [t2;index] ty_v in
                    A.Set.add (A.make (A.Neq(g1,g2))) acc
                | _ -> acc

    (* XXX on peut faire mieux si on a directement le type *)
    let ext_1 acc r1 r2 class_of =
      match X.type_info r1 with
        | Ty.Tint | Ty.Treal | Ty.Tbool | Ty.Tunit | Ty.Tbitv _ -> acc
        | Ty.Tvar _ | Ty.Tfarray _ | Ty.Text _ -> 
            match X.term_extract r1, X.term_extract r2 with
              | Some t1, Some t2 ->
                  L.fold_left
                    (fun acc t1 ->
                       L.fold_left 
                         (fun acc t2 -> ext_2 acc t1 t2) acc (class_of t2)
                    ) acc (class_of t1)
                    
              | _ -> acc

    (* nouvelles disegalites par instantiation du premier
       axiome d'exentionnalite *)
    let extension acc la class_of = 
      List.fold_left
        (fun acc -> function 
           | A.Neq(r1,r2) -> ext_1 acc r1 r2 class_of
           | _ -> acc
        ) acc la

    (* deduction de nouvelles dis/egalites *)
    let new_equalities env eqs la class_of = 
      let la = L.filter (function A.Builtin _  -> false | _ -> true) la in
      let assu, sp, eqs = 
        L.fold_left
          (fun (assu,sp,eqs) a ->
             Aset.add a assu, 
             Aset.remove (A.neg_view a)(Aset.remove a sp),
             A.Set.union (Amap.find a env.csq) eqs
          ) (env.sps, env.split, eqs) la
      in
      {env with sps=assu; split=sp}, extension eqs la class_of

    (* instantiation des axiomes des tableaux *)
    let instantiate env la class_of =
      let la = List.map fst la in
      Debug.assume fmt la; 
      let env = new_terms env la in
      let env, atoms = new_splits env A.Set.empty class_of in
      let env, atoms = new_equalities env atoms la class_of in
      Debug.env fmt env;
      Debug.new_equalities fmt atoms;
      env, A.Set.elements atoms 
        
    (* choisir une egalite sur laquelle on fait un case-split *)
    let case_split env = 
      try
        (* XXX enlever l'assertion avant la prochaine release *)
        assert (Aset.is_empty (Aset.inter env.split env.sps));
        let a = Aset.choose env.split in
        if debug_arrays then 
          fprintf fmt "[Arrays.case-split] %a@." (A.print_view X.print) a;
        [a, None]
      with Not_found ->
        if debug_arrays then fprintf fmt "[Arrays.case-split] Nothing@.";
        []
          
    let empty _ = 
      {gets  = G.empty;
       tbset = TBS.empty;
       split = Aset.empty;
       sps   = Aset.empty;
       csq   = Amap.empty;
       seen  = TM.empty
      }

    let assume env _ = env, []
    let query a _  = false
    let add env r = env
  end
end
