(** Decorate a pair of outcometree to highlight difference *)

open Outcometree

module H = Highlightable
type emphase = H.emphase = On | Off
type mode = Inclusion | Unification

module D = Decorated

(** {2 Fuel interface} *)

(* Default fuel available for printing an error *)
let fuel = Clflags.error_size

let get_fuel () = !fuel

(** {2 Two dimensional size } *)
module Size = struct
  type t = {
    (* size of elements that should be printed in priority *)
    primary: int;
    (* size of elements that may be printed if there is some spare space *)
    secondary: int
  }
  let one = { primary=1; secondary = 0 }
  let empty = { primary=0; secondary=0 }

  let secondary secondary = { empty with secondary }
  let primary primary = { empty with primary }

  let map2 (%) x y =
    { primary = x.primary % y.primary; secondary = x.secondary % y.secondary }

  let (++) = map2 (+)
  let max = max
  let sum = List.fold_left (++) empty

  let card s = s.primary + s.secondary
end

(** {2 Diffed type } *)

(** Generator type *)
type ('a,'b) gen = {
  gen: int -> 'a; (** [gen fuel x] generate a representation of the underlying
                      element with size [fuel] *)
  size:'b;
}

(** Main type *)
type 'a diff =
  | Eq of ('a, int) gen
  | D of ('a * 'a , Size.t) gen

let is_eq = function
  | Eq _ -> true
  | _ -> false

let size = function
  | D r -> r.size
  | Eq r -> Size.secondary r.size

let flatten = function
  | D r -> r
  | Eq r -> { size = Size.secondary r.size;
              gen = (fun fuel -> let r = r.gen fuel in r, r) }

let pick l fuel = match l with
  | Eq r -> r.gen fuel
  | D r -> fst (r.gen fuel)

(** {3 Combinators } *)
let const x _fuel = x
let pure ?(size=0) f = Eq { size; gen = const f }
let split size left right=
  D { gen = const (left, right); size}

let with_emph emphase x = H.Item(emphase,x)
let plain x = with_emph Off x
let emph x = with_emph On x

let hmax x y = let open H in
  match x, y with
  | On, _ | _, On -> On
  | Off, Off -> Off

let at_least st = function
  | H.Ellipsis _ as i -> i
  | H.Item(st',x) -> H.Item(hmax st' st,x)


let fmap f = function
  | Eq r -> Eq { r with gen = (fun fuel -> f (r.gen fuel) ) }
  | D r -> D { r with gen = (fun fuel -> let x, y = r.gen fuel in f x, f y) }

let set_emphase emphase = fmap (with_emph emphase)
let raise_emphase emphase = fmap (at_least emphase)

let stitch x y = match x, y with
  | Eq x, Eq y ->
      D { size =  Size.primary (max x.size y.size);
          gen =(fun fuel -> x.gen fuel,y.gen fuel)
        }
  | r, r' ->
      (* Note that this branch is used for type aliases,
         cf. Otyp_alias, Not Otyp_alias branch *)
      let r = flatten r and r' = flatten r' in
      D {
        size = Size.max r.size r'.size;
        gen = (fun fuel -> let x, _ =  r.gen fuel and _, y = r'.gen fuel in
               x, y
              )
        }

let self_diff_then_stitch diff x y =
  stitch (raise_emphase On @@ diff x x) (raise_emphase On @@ diff y y)

let similar left right =
  D { gen = const (left, right); size = Size.secondary 1 }

let fork size f g = function
  | Eq r ->
      D {
        gen = (fun fuel ->
            let common = r.gen fuel in
            f common, g common
          );
        size = Size.(size ++ secondary r.size)
      }
  | D r ->
       D {
        gen = (fun fuel ->
            let x, y = r.gen fuel in
            f x, g y
          );
        size = Size.( size ++ r.size )
      }

(** {3 Hlist } *)

module Std = struct
  type nonrec 'a list = 'a list = [] | (::) of 'a * 'a list
end

module AppList = struct

  module T = struct
    type ('a,'res) t =
      | [] : ('res,'res) t
      | (::) : 'a diff * ('any,'res) t -> ('a -> 'any, 'res) t

  end

  type 'a concrete = Single of 'a | Pair of 'a * 'a

  let concrete_pure f = Single f

  let map_2_2 f x = match f, x with
    | Single f, Single x -> Single(f x)
    | Pair (f,g), Single x -> Pair(f x,g x)
    | Single f, Pair(x,y) -> Pair(f x, f y)
    | Pair (f,g), Pair(x,y) -> Pair(f x, g y)

  let rec sizes: type a b. (a,b) T.t -> Size.t list =
    function
    | T.[] -> []
    | T.( a :: q ) -> size a :: sizes q

  module Level_map =
    Map.Make(struct type t = int let compare (x:int) (y:int) = compare x y end)


  (* Fill first the subelements with the smallest full size,
     up to the point where it is impossible to fill all elements
     with a given full size, then fill elements from the left to the right
  *)
  let rising_tide proj fuel l =
    let levels =
      List.fold_left (fun lvls s ->
          let lvl = proj s in
          if lvl <> 0 then
            let c = try Level_map.find lvl lvls with Not_found -> 0 in
            Level_map.add lvl (1 + c) lvls
          else
            lvls)
        Level_map.empty l in
    let _, max_level, fuel_left =
      Level_map.fold(fun k x (keep_going, old , fuel) ->
        if keep_going && k * x <= fuel then (true, k, fuel - k * x)
        else (false, old, fuel)
      ) levels (true, 0, fuel) in
     let rec finish fuel = function
      | [] -> []
      | x :: q when proj x > max_level ->
          let consumed = min (proj x) fuel in
          consumed :: finish (fuel-consumed) q
      | x :: q -> (proj x) :: finish fuel q in
    finish fuel_left l


  let distribute gmax sizes fuel =
    let open Size in
    if fuel >= card gmax then
      List.map (fun size -> card size) sizes
    else if fuel <= gmax.primary then
      rising_tide (fun y -> y.primary) fuel sizes
    else
      let fuel' = fuel - gmax.primary in
      let d = rising_tide (fun y -> y.secondary) fuel' sizes in
      List.map2 (fun x y -> x.primary + y) sizes d

  let rec apply : type a res. a concrete -> (a,res) T.t -> int list
    -> res concrete =
    fun f l fs -> match l, fs with
      | T.[], [] -> f
      | T.( a :: q ), fuel :: fs ->
          let f' = map_2_2 f
          (match a with
            | D r -> let x, y = r.gen fuel in Pair(x,y)
            | Eq r -> Single (r.gen fuel)
          ) in
          apply f' q fs
      | _ -> assert false

  let rec is_all_eq: type a b. (a,b) T.t -> bool  = function
    | T.(a :: q) -> if is_eq a then is_all_eq q else false
    | T.[] -> true

  let eq = function
    | Single x -> x
    | Pair _ -> raise (Invalid_argument "Unexpected pair output")

  let pair = function
    | Single x -> x, x
    | Pair (x, y) -> x, y

  let mkdiff f l =
    let sizes = sizes l in
    let global = Size.sum sizes in

    let gen proj fuel =
      let distribution = distribute global sizes fuel in
      proj (apply f l distribution) in
    if is_all_eq l then
      Eq { size = Size.card global; gen = gen eq }
    else
      D { size= global; gen = gen pair }

  let (<*>) f = mkdiff (concrete_pure f)

end

open AppList

(** {3 Simple combinators }*)
let diff size left right =
  if left = right then
    Eq { size = Size.card size; gen = const (plain right) }
  else
    D { gen = const (emph left,emph right); size }

let fmap2 ?side:(left,right=On,On) f x y = match x, y with
  | H.Item(hl,x), H.Item(hr,y) -> set_emphase (hmax hl hr) (f x y)
  | H.Ellipsis _ as e, H.Item(_,x) ->
      stitch (pure e) (set_emphase right @@ f x x)
  | H.Item(_,x), (H.Ellipsis _ as e) ->
      stitch (set_emphase left @@ f x x) (pure e)
  | H.Ellipsis n, H.Ellipsis n' -> pure @@ H.Ellipsis(max n n')

let bind2 ?side:(left,right=On,On) f x y = match x, y with
  | H.Item(hx,x), H.Item(hy,y) ->  raise_emphase (hmax hx hy) (f x y)
  | H.Ellipsis _ as e, H.Item(_,x) ->
      stitch (pure e) (raise_emphase right @@ f x x)
  | H.Item(_,x), (H.Ellipsis _ as e) ->
      stitch (raise_emphase left @@ f x x) (pure e)
  | H.Ellipsis n, H.Ellipsis n' -> pure @@ H.Ellipsis(max n n')


(** {3 List combinators } *)
let dup x = x, x

module List_combinators = struct
  let ellipses n = H.Ellipsis n
  let fueled x f = f > 0 || Size.card (size x) = 0

  let rec elide = function
    | H.Ellipsis 0 :: q -> elide q
    | H.Ellipsis k :: H.Ellipsis n :: q -> elide (H.Ellipsis (k+n) :: q)
    | a :: q -> a :: elide q
    | [] -> []

  let map2 f (x,y) = f x, f y

  let list_diff l =
    let sizes = List.map size l in
    let global = Size.sum sizes in
    let distribute fuel = AppList.distribute global sizes fuel in
    if List.for_all is_eq l then
      let gen fuel =
        elide
        @@ List.map2
          (fun x fuel -> if fueled x fuel then pick x fuel else ellipses 1) l
        @@ distribute fuel in
      Eq { size = Size.card global; gen }
    else
      let gen fuel =
        map2 elide
        @@ List.split
        @@ List.map2 (fun x fuel -> if fueled x fuel  then
                         (flatten x).gen fuel
                       else dup (ellipses 1)
                     ) l
        @@ distribute fuel in
      D {  size = global; gen }


  let hole = ellipses 0

  (* Simple list where the k-th elements should be compared to the k-th element  *)
  let list diff x y =
    let rec list xs ys = match xs, ys with
      | [], [] -> []
      | x :: xs , y :: ys -> diff (plain x) (plain y) :: list xs ys
      | x :: xs, ([] as ys) -> diff (plain x) hole  :: list xs ys
      | ([] as xs), y :: ys -> diff hole (plain y) :: list xs ys
    in
    list_diff @@ list x y


  let lift_cmp cmp x y = match x,y with
    | H.Ellipsis _, H.Ellipsis _ -> 0
    | H.Ellipsis _, H.Item _ -> -1
    | H.Item _, H.Ellipsis _  -> 1
    | H.Item(_,x), H.Item(_,y) -> cmp x y

  (* Keyed list where the element with key k should be compared to the element
     associated with the same key *)
  let rec pair_list cmp diff xs ys = match xs, ys with
    | [], [] -> []
    | x :: xs, ([] as ys) ->
        diff (plain x) hole :: pair_list cmp diff xs ys
    | ([] as xs), y :: ys ->
        diff hole (plain y) :: pair_list cmp diff xs ys
    | x :: xs' , y :: ys' ->
        let x', y' = plain x, plain y in
        let cmp_xy = cmp x' y' in
        if cmp_xy < 0 then
          diff x' hole :: pair_list cmp diff xs' ys
        else if cmp_xy > 0 then
          diff hole y' :: pair_list cmp diff xs ys'
        else
          diff x' y' :: pair_list cmp diff xs' ys'

  let keyed_list cmp diff x y =
    list_diff @@ pair_list (lift_cmp cmp) diff x y

  (** Compare free-form structure like module *)

  let dispatch arrays ln rn xs =
    let dispatch_side a x = function
      | Some n -> a.(n)<-x
      | None -> () in
    dispatch_side (fst arrays) (fst xs) ln;
    dispatch_side (snd arrays) (snd xs) rn

  let rec reorder out l fuel  =  match l, fuel with
    | [], _  |  _, [] -> ()
    | (x, ln, rn):: xs , f :: fs when not (fueled x f) ->
        dispatch out ln rn (dup @@ H.Ellipsis 1);
        reorder out xs fs
    | (x,ln,rn) :: xs, f :: fs  ->
        let x = (flatten x).gen f in
        dispatch out ln rn x;
        reorder out xs fs

    let rec merge cmp diff xs ys = match xs, ys with
    | [], [] -> []
    | (k, x) :: xs, ([] as ys) ->
        (diff x hole, Some k, None) :: merge cmp diff xs ys
    | ([] as xs), (k, y) :: ys ->
        (diff hole y, None, Some k):: merge cmp diff xs ys
    | (nx, x') :: xs' , (ny, y') :: ys' ->
        let cmp_xy = cmp x' y' in
        if cmp_xy < 0 then
          (diff x' hole, Some nx, None) :: merge cmp diff xs' ys
        else if cmp_xy > 0 then
         (diff hole y', None, Some ny) :: merge cmp diff xs ys'
        else
         (diff x' y', Some nx, Some ny) :: merge cmp diff xs' ys'

  let module_like cmp diff x y =
    let decorate l =
      List.rev @@ snd @@
      List.fold_left (fun (i,l) x -> i+1,(i, plain x)::l) (0,[]) l in
    let sort = List.sort (fun (_,y) (_,y') -> lift_cmp cmp y y') in
    let x, y = sort(decorate x), sort(decorate y) in
    let companion x = Array.make (List.length x) (H.Ellipsis 1) in
    let l = merge (lift_cmp cmp) diff x y in
    let sizes = List.map (fun (x,_,_) -> size x) l in
    let global = Size.sum sizes in
    let gen fuel =
      let x,y as out = companion x, companion y in
      let fuels = AppList.distribute global sizes fuel in
      reorder out l fuels;
      elide (Array.to_list x), elide (Array.to_list y) in
    if List.for_all (fun (x,_,_) -> is_eq x) l then
      Eq { size = Size.card global; gen = (fun fuel -> fst (gen fuel)) }
    else
      D { size = global; gen }
end
let list, keyed_list, module_like = List_combinators.( list, keyed_list, module_like)


  (** {2 Utility functions } *)
let pair x y = x, y
let some x = Some x
let opt_ext diff x y =
  let fsome st x = with_emph st (some x) in
  let ff st = fmap @@ fsome st in
  match x, y with
  | None, None -> pure (plain None)
  | Some x, Some y -> ff Off (diff x y)
  | Some x, None ->  stitch (ff On @@ diff x x) (pure (emph None))
  | None, Some x ->  stitch (pure (emph None)) (ff On @@ diff x x)

let bool: bool -> _ = diff Size.empty

let positive_rec x y = match x, y with
  | Orec_first, _ | _, Orec_first -> diff Size.one x y
  | _ -> diff Size.empty x y

let negative_rec x y = match x, y with
  | Orec_not, _ | _, Orec_not -> diff Size.one x y
  | _ -> diff Size.empty x y

let priv x y = match x, y with
  | Asttypes.Private, _ | _, Asttypes.Private -> diff Size.one x y
  | _ -> diff Size.empty x y

let ext: out_ext_status -> _ = diff Size.empty
let attr_diff: out_attribute -> _ = diff Size.empty
let string: string -> _ = diff Size.one
open AppList.T


(** {2 Outcome tree difference computation functions} *)
module Ident = struct
  let apply x y = plain @@ D.Oide_apply (x,y)
  let dot x y = plain @@ D.Oide_dot (x,y)
  let base_ident x = plain @@ D.Oide_ident x

  let rec main x y = match x, y with
    | Oide_apply(x,y), Oide_apply(x',y') -> apply <*> [ main x x'; main y y' ]
    | Oide_dot(x,y), Oide_dot(x',y') -> dot <*> [ main x x'; string y y' ]
    | Oide_ident s, Oide_ident s' -> base_ident <*> [ string s s' ]
    | x, y -> self_diff_then_stitch main x y
end
let id_diff: out_ident -> _ = Ident.main

let ocmp (n,_) (n',_) = compare n n'

module Type = struct

  let constr x y = plain @@ D.Otyp_constr(x,y)
  let manifest x y = plain @@ D.Otyp_manifest(x,y)
  let object' x y  = plain @@ D.Otyp_object(x,y)
  let sum x = plain @@ D.Otyp_sum x
  let class' x y z =plain @@ D.Otyp_class(x,y,z)
  let record x = plain @@ D.Otyp_record x
  let var x y = plain @@ D.Otyp_var(x,y)
  let variant x y z w = plain @@ D.Otyp_variant(x,y,z,w)
  let poly x y = plain @@ D.Otyp_poly(x,y)
  let module' x y z = plain @@ D.Otyp_module(x,y,z)
  let attribute x y = plain @@ D.Otyp_attribute(x,y)
  let tuple l =  plain @@ D.Otyp_tuple l
  let alias x y = plain @@ D.Otyp_alias(x,y)

  let rec fn_to_list =
    let open Std in
    function
    | Otyp_arrow (arg,rest) ->
        let rest, ret = fn_to_list rest in
        arg :: rest, ret
    | rest -> [], rest

  let rec list_to_fn =
    let open Std in
    function
    | [], ret -> ret
    | arg :: q, ret -> plain @@ D.Otyp_arrow (arg, list_to_fn (q,ret))

  let arrow l = list_to_fn l

  module M = Misc.StringSet
  let unfree_vars = ref M.empty
  let reset_free () = unfree_vars := M.empty

  let is_free var =
    if M.mem var !unfree_vars then
      false
    else
      (unfree_vars := M.add var !unfree_vars; true)

  let std x = pure (plain x)
  let slist x y = list (bind2 string) x y

  let rec type' mode t1 t2 =
    match t1, t2 with
    | Otyp_abstract, Otyp_abstract
    | Otyp_open, Otyp_open -> pure (Decorate.typ t1)

    | Otyp_alias (ty,as'), Otyp_alias(ty2,as2) ->
        alias <*> [typ mode ty ty2; string as' as2]
    | Otyp_alias (ty,as'), y when is_free as' ->
        let d = typ mode ty y in
        stitch (alias <*> [d; std as']) d
    | x, Otyp_alias (ty,as') when is_free as' ->
        let d = typ mode x ty in
        stitch d (alias <*> [ d; std as' ])

    | Otyp_arrow _ , Otyp_arrow _ ->
        let fn =  fn_to_list t1 and fn' = fn_to_list t2 in
        arrow <*> [ fn_args mode fn fn' ]
    | Otyp_class (b, id, args), Otyp_class (b',id',args') ->
        class' <*> [ bool b b'; id_diff id id'; tylist mode args args' ]
    | Otyp_constr(t1, ([_] as args)), Otyp_constr(t2, ([_] as args2))->
        constr <*> [ id_diff t1 t2; tylist mode args args2 ]
    | Otyp_constr(t1, [arg]), t2 ->
        fork (Size.primary 1)
          (fun x -> constr (Decorate.ident ~highlight:H.On t1) [x])
          (fun x -> x)
          (type' mode arg t2)
    | t1, Otyp_constr(t2, [arg]) ->
        fork (Size.primary 1)
          (fun x -> x)
          (fun x -> constr (Decorate.ident ~highlight:H.On t2) [x])
          (type' mode t1 arg)
    | Otyp_constr (t1, args), Otyp_constr(t2,args2) ->
        constr <*> [ id_diff t1 t2; tylist mode args args2 ]
    | Otyp_manifest(x,y), Otyp_manifest(x',y') ->
        manifest <*> [ typ mode x x'; typ mode y y' ]
    | Otyp_object (l,closed), Otyp_object (l2,closed2)  ->
        let st = function Some _ -> Off | None -> On in
        object' <*> [ olist mode (st closed2, st closed) l l2;
                      opt_ext bool closed closed2 ]
    | Otyp_record r, Otyp_record r' ->
        record <*> [ rlist mode r r' ]
    | Otyp_sum s, Otyp_sum s' ->
        sum <*> [ list (bind2 @@ dconstr mode) s s' ]
    | Otyp_tuple l, Otyp_tuple l' -> tuple <*> [ tylist mode l l' ]

    | Otyp_var (b,name), Otyp_var(b',name')
      when is_free name || (is_free name' && mode = Unification) ->
        if name = name' then
          var <*> [bool b b'; string name name']
        else
          similar (Decorate.typ t1) (Decorate.typ t2)
    | Otyp_var (b,name), Otyp_var(b',name') ->
        var <*> [bool b b'; string name name']
    | Otyp_var(_, name), _ when is_free name ->
        similar (Decorate.typ t1) (Decorate.typ t2)
    | _, Otyp_var(_,name) when is_free name && mode = Unification ->
        similar (Decorate.typ t1) (Decorate.typ t2)
    | Otyp_variant (b,fields,b2,tags), Otyp_variant(b',fields',b2',tags') ->
        variant <*> [
          bool b b';
          dvariant mode fields fields';
          bool b2 b2';
          opt_ext slist tags tags'
        ]

    | Otyp_poly (forall,ty), Otyp_poly (forall',ty') ->
        poly <*> [ slist forall forall'; typ mode ty ty' ]
    | Otyp_module (name, args, tyl), Otyp_module (name',args',tyl') ->
        module' <*> [ string name name'; slist args args'; tylist mode tyl tyl' ]
    | Otyp_attribute (t,attr), Otyp_attribute (t',attr') ->
        attribute <*> [ typ mode t t'; attr_diff attr attr' ]
    | Otyp_stuff _, Otyp_stuff _ ->
        split Size.one (Decorate.typ t1) (Decorate.typ t2)

    | (Otyp_var _ | Otyp_constr _ ), (Otyp_var _ | Otyp_constr _ ) ->
        self_diff_then_stitch (typ mode) t1 t2

    | _ -> self_diff_then_stitch (type' mode) t1 t2
  and typ mode x = type' mode x
  and tylist mode x y = list (bind2 @@ typ mode) x y
  and fn_args mode (x,ret) (y,ret') =
    pair <*> [ dfn_args mode x y; typ mode ret ret' ]
  and dofield mode (n,ty) (n',ty') =
        pair <*> [ string n n'; typ mode ty ty']
  and olist mode side x = keyed_list ocmp (fmap2 ~side @@ dofield mode) x

  and dconstr mode c c' =
    (fun cname args ret -> plain {D.cname;args;ret} )
          <*> [ string c.cname c'.cname;
                tylist mode c.args c'.args;
                opt_ext (typ mode) c.ret c'.ret
              ]

  and dfield mode f f' =
    (fun label mut typ -> plain {D.label; mut; typ} )
    <*> [ string f.label f'.label
        ; bool f.mut f'.mut
        ; typ mode f.typ f'.typ ]
  and rlist mode x = list (bind2 @@ dfield mode) x

  and variant_cmp x y = compare x.tag y.tag
  and dvariant mode x y = match x, y with
    | Ovar_typ t, Ovar_typ t' ->
        (fun x -> plain @@ D.Ovar_typ x) <*> [typ mode t t']
    | Ovar_fields f, Ovar_fields f' ->
        (fun x -> plain @@ D.Ovar_fields x)
        <*> [keyed_list variant_cmp (fmap2 @@ dvfield mode) f f']
    | _ ->  self_diff_then_stitch (dvariant mode) x y

  and dvfield mode f f' =
          (fun tag ampersand conj -> {D.tag;ampersand;conj} )
          <*> [ string f.tag f'.tag;
                bool   f.ampersand f'.ampersand;
                tylist mode f.conj f'.conj]

  and dfn_args mode x = list (fmap2 (fun (label,ty) (label',ty') ->
      pair <*> [ string label label'; typ mode ty ty' ] )) x
end open Type

module Ct = struct
  let constr x y = plain @@ D.Octy_constr (x,y)

  let rec to_list = function
    | Octy_arrow (arg,z) -> let q, e = to_list z in
        Std.(arg :: q), e
    | rest -> Std.[], rest

  let rec arrow = function
    | Std.[], ret -> ret
    | Std.(arg :: q), ret -> plain @@ D.Octy_arrow(arg, arrow (q,ret) )

  let signature x y = plain @@ D.Octy_signature (x,y)

  let constraint' x y = plain @@ D.Ocsg_constraint {D.lhs=x;rhs=y}
  let method' x y z w = plain @@ D.Ocsg_method(x,y,z,w)
  let value x y z w = plain @@ D.Ocsg_value(x,y,z,w)

  let rec ct mode x y = match x, y with
    | Octy_constr (id,tyl), Octy_constr(id',tyl') ->
        constr <*> [id_diff id id'; tylist mode tyl tyl']
    | Octy_arrow _ , Octy_arrow _ ->
        arrow <*> [ ct_args mode x y ]
    | Octy_signature (x,items), Octy_signature(y,items') ->
        signature <*> [ opt_ext (typ mode) x y; item_list mode items items' ]
    | _ -> self_diff_then_stitch (ct mode) x y
  and item_list mode x = list (bind2 @@ items mode ) x
  and ct_args mode c c' =
    let (x,ctx), (y,cty) = to_list c, to_list c' in
    pair <*> [ dfn_args mode x y; (ct mode) ctx cty]
  and items mode x y = match x,y with
    | Ocsg_constraint c, Ocsg_constraint c' ->
        constraint' <*> [typ mode c.lhs c'.lhs; typ mode c.rhs c'.rhs]
    | Ocsg_method(name,priv,virt,ty), Ocsg_method(name',priv',virt',ty') ->
        method' <*> [ string name name';
                      bool priv priv'; bool virt virt';
                      typ mode ty ty' ]
    | Ocsg_value(name,priv,virt,ty), Ocsg_value(name',priv',virt',ty') ->
        value <*> [ string name name';
                    bool priv priv'; bool virt virt';
                    typ mode ty ty' ]
    | _ -> self_diff_then_stitch (items mode) x y

end


module Sig = struct
  let class' a b c d e = plain @@ D.Osig_class(a,b,c,d,e)
  let class_type a b c d e = plain @@ D.Osig_class_type(a,b,c,d,e)
  let typext x y = plain @@ D.Osig_typext(x,y)
  let modtype x y = plain @@ D.Osig_modtype(x,y)
  let module' x y z = plain @@ D.Osig_module(x,y,z)
  let type' x y = plain @@ D.Osig_type(x,y)
  let value x = plain @@ D.Osig_value x
end

module Mty = struct

  let rec list_of_functor = function
    | Omty_functor(arg,res) ->
        let args, res = list_of_functor res in
       Std.( arg  :: args ), res
    | rest -> Std.[], rest

  let rec functor' l res =
    let open Std in
    match l with
    | [] -> res
    | arg :: q ->
        plain @@ D.Omty_functor(arg, functor' q res)

  let ident x = plain @@ D.Omty_ident x
  let signature x = plain @@ D.Omty_signature x
  let alias x = plain @@ D.Omty_alias x
end

let type_decl otype_name otype_params otype_type otype_private otype_immediate
    otype_unboxed otype_cstrs =
  {D.otype_name; otype_params; otype_type; otype_private; otype_immediate;
   otype_unboxed; otype_cstrs}

let extension_constructor oext_name oext_type_name oext_type_params oext_args
    oext_ret_type oext_private =
  {D.oext_name; oext_type_name; oext_type_params; oext_args; oext_ret_type;
   oext_private }

let _type_extension  otyext_name otyext_params otyext_constructors otyext_private =
  {D.otyext_name; otyext_params; otyext_constructors; otyext_private}

let val_decl oval_name oval_type oval_prims oval_attributes =
  {D.oval_name;oval_type;oval_prims;oval_attributes}

let dparam p p' =
  (fun co cn name -> {D.covariant=co;contravariant=cn;name})
  <*> [ bool p.covariant p'.covariant;
        bool p.contravariant p'.contravariant;
        string  p.name p'.name ]

let dct c c' =
  (fun lhs rhs -> {D.lhs;rhs}) <*>
  [typ Inclusion c.lhs c'.lhs; typ Inclusion c.rhs c'.rhs]

let clist = list (fmap2 dct)
let plist = list (fmap2 dparam)

let alist = list (bind2 attr_diff)

let sig_item_key = function
  | Osig_class(_,name,_,_,_) -> "class", name
  | Osig_class_type(_,name,_,_,_) -> "class_type", name
  | Osig_typext (te,_) -> "type_ext", te.oext_name
  | Osig_modtype (name,_) -> "modtype", name
  | Osig_module(name,_,_) -> "module", name
  | Osig_type(name,_) -> "type", name.otype_name
  | Osig_value v -> "val", v.oval_name
  | Osig_ellipsis -> "ellipsis", ""

let sigcmp x y = compare (sig_item_key x) (sig_item_key y)

let rec sig_item mode s1 s2 =
  Type.reset_free ();
  let open Sig in
  match s1, s2 with

  | Osig_class (b,name,params,typ,recs), Osig_class (b',name',params',typ',recs') ->
      class' <*> [ bool b b'; string name name'; plist params params';
                   Ct.ct mode typ typ'; positive_rec recs recs' ]
  | Osig_class_type (b,name,params,typ,recs),
    Osig_class_type (b',name',params',typ',recs') ->
      class_type <*> [ bool b b'; string name name'; plist params params';
                      Ct.ct mode typ typ'; positive_rec recs recs' ]
  | Osig_typext (te,st), Osig_typext (te',st') ->
      let opty = opt_ext (typ mode) in
      typext <*>
     [ extension_constructor
        <*> [ string      te.oext_name        te'.oext_name;
              string      te.oext_type_name   te'.oext_type_name;
              slist       te.oext_type_params te'.oext_type_params;
              tylist mode te.oext_args        te'.oext_args;
              opty        te.oext_ret_type    te'.oext_ret_type;
              priv        te.oext_private     te'.oext_private ]
        ;
        ext st st'
      ]
  | Osig_modtype (name,typ), Osig_modtype (name',typ') ->
      modtype <*> [string name name'; module_type mode typ typ']

  | Osig_module (name,typ,recs), Osig_module (name',typ',recs') ->
      module' <*> [ string           name name';
                    module_type mode typ  typ';
                    positive_rec     recs recs'
                  ]
  | Osig_type (decl, recs),  Osig_type (decl', recs') ->
      type' <*>
      [ type_decl <*> [
            string   decl.otype_name      decl'.otype_name;
            plist    decl.otype_params    decl'.otype_params;
            typ mode decl.otype_type      decl'.otype_type;
            priv     decl.otype_private   decl'.otype_private;
            bool     decl.otype_immediate decl'.otype_immediate;
            bool     decl.otype_unboxed   decl'.otype_unboxed;
            clist    decl.otype_cstrs     decl'.otype_cstrs;
          ];
        negative_rec recs recs']
  | Osig_value v, Osig_value v' ->
      value <*>
      [ val_decl
        <*> [ string   v.oval_name        v'.oval_name;
              typ mode v.oval_type        v'.oval_type;
              slist    v.oval_prims       v'.oval_prims;
              alist    v.oval_attributes  v'.oval_attributes
            ]
      ]
  | _ -> self_diff_then_stitch (sig_item mode) s1 s2

and module_type mode x y =
  let open Mty in
  match x, y with
  | Omty_abstract, Omty_abstract -> pure @@ Decorate.module_type x

  | Omty_functor _ , Omty_functor _ ->
      functor' <*> functor_diff mode x y
  | Omty_ident id, Omty_ident id' -> ident <*> [id_diff id id']
  | Omty_signature s, Omty_signature s' ->
      signature <*> [ module_like sigcmp (bind2 @@ sig_item mode) s s' ]
  | Omty_alias x, Omty_alias y -> alias <*> [id_diff x y]

  | _ -> self_diff_then_stitch (module_type mode) x y

and functor_diff mode t t'=
  let (f,res), (f',res') = Mty.(list_of_functor t, list_of_functor t') in
  let name_diff name name' =
    if name ="_" || name' ="_" then
      similar (plain name) (plain name')
    else
      string name name' in
  let arg (name,mty) (name',mty') =
    pair <*> [ name_diff name name'; opt_ext (module_type mode) mty mty'] in
  [list (fmap2 arg) f f'; module_type mode res res']

(** {2 Exported functions} *)

module Gen = struct

  type ('a,'b) t = mode -> 'a * 'a -> int -> 'b H.t * 'b H.t

  let simplify f mode (x,y)=
  Type.reset_free ();
  match f mode x y with
  | Eq x -> fun fuel -> dup ( x.gen fuel )
  | D r -> fun fuel -> r.gen fuel

  let typ = simplify type'
  let sig_item = simplify sig_item
  let class_type = simplify Ct.ct
  let modtype = simplify module_type

end

type ('a, 'b) t = mode -> 'a * 'a -> 'b H.t * 'b H.t
let simplify f mode x = f mode x @@ get_fuel ()

let typ = simplify Gen.typ
let sig_item = simplify Gen.sig_item
let class_type = simplify Gen.class_type
let modtype = simplify Gen.modtype
