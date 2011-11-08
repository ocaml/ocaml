(* Encoding GADTs in OCaml *)
(* (c) Alain Frisch / Lexifi *)

(* Basic tag *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
;;

(* Tagging data *)

type variant =
  | VInt of int
  | VString of string
  | VList of variant list
  | VPair of variant * variant
 
let rec variantize: type t. t ty -> t -> variant =
  fun ty x ->
    (* type t is abstract here *)
    match ty with
    | Int -> VInt x  (* in this branch: t = int *)
    | String -> VString x (* t = string *)
    | List ty1 ->
        VList (List.map (variantize ty1) x)
        (* t = 'a list for some 'a *)
    | Pair (ty1, ty2) ->
        VPair (variantize ty1 (fst x), variantize ty2 (snd x))
        (* t = ('a, 'b) for some 'a and 'b *)
 
exception VariantMismatch
 
let rec devariantize: type t. t ty -> variant -> t =
  fun ty v ->
    match ty, v with
    | Int, VInt x -> x
    | String, VString x -> x
    | List ty1, VList vl ->
        List.map (devariantize ty1) vl
    | Pair (ty1, ty2), VPair (x1, x2) ->
        (devariantize ty1 x1, devariantize ty2 x2)
    | _ -> raise VariantMismatch
;;

(* Handling records *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
  | Record: 'a record -> 'a ty
 
and 'a record =
    {
     path: string;
     fields: 'a field_ list;
    }
 
and 'a field_ =
  | Field: ('a, 'b) field -> 'a field_
 
and ('a, 'b) field =
    {
     label: string;
     field_type: 'b ty;
     get: ('a -> 'b);
    }
;;

(* Again *)

type variant =
  | VInt of int
  | VString of string
  | VList of variant list
  | VPair of variant * variant
  | VRecord of (string * variant) list

let rec variantize: type t. t ty -> t -> variant =
  fun ty x ->
    (* type t is abstract here *)
    match ty with
    | Int -> VInt x  (* in this branch: t = int *)
    | String -> VString x (* t = string *)
    | List ty1 ->
        VList (List.map (variantize ty1) x)
        (* t = 'a list for some 'a *)
    | Pair (ty1, ty2) ->
        VPair (variantize ty1 (fst x), variantize ty2 (snd x))
        (* t = ('a, 'b) for some 'a and 'b *)
    | Record {fields} ->
        VRecord
          (List.map (fun (Field{field_type; label; get}) ->
                       (label, variantize field_type (get x))) fields)
;;
 
(* Extraction *)

type 'a ty =
  | Int: int ty
  | String: string ty
  | List: 'a ty -> 'a list ty
  | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
  | Record: ('a, 'builder) record -> 'a ty
 
and ('a, 'builder) record =
    {
     path: string;
     fields: ('a, 'builder) field list;
     create_builder: (unit -> 'builder);
     of_builder: ('builder -> 'a);
    }
 
and ('a, 'builder) field =
  | Field: ('a, 'builder, 'b) field_ -> ('a, 'builder) field
 
and ('a, 'builder, 'b) field_ =
  {
   label: string;
   field_type: 'b ty;
   get: ('a -> 'b);
   set: ('builder -> 'b -> unit);
  }
 
let rec devariantize: type t. t ty -> variant -> t =
  fun ty v ->
    match ty, v with
    | Int, VInt x -> x
    | String, VString x -> x
    | List ty1, VList vl ->
        List.map (devariantize ty1) vl
    | Pair (ty1, ty2), VPair (x1, x2) ->
        (devariantize ty1 x1, devariantize ty2 x2)
    | Record {fields; create_builder; of_builder}, VRecord fl ->
        if List.length fields <> List.length fl then raise VariantMismatch;
        let builder = create_builder () in
        List.iter2
          (fun (Field {label; field_type; set}) (lab, v) ->
            if label <> lab then raise VariantMismatch;
            set builder (devariantize field_type v)
          )
          fields fl;
        of_builder builder
    | _ -> raise VariantMismatch
;;

type my_record  =
    {
     a: int;
     b: string list;
    }
 
let my_record =
  let fields =
    [
     Field {label = "a"; field_type = Int;
            get = (fun {a} -> a);
            set = (fun (r, _) x -> r := Some x)};
     Field {label = "b"; field_type = List String;
            get = (fun {b} -> b);
            set = (fun (_, r) x -> r := Some x)};
    ]
  in
  let create_builder () = (ref None, ref None) in
  let of_builder (a, b) =
    match !a, !b with
    | Some a, Some b -> {a; b}
    | _ -> failwith "Some fields are missing in record of type my_record"
  in
  Record {path = "My_module.my_record"; fields; create_builder; of_builder}
;;

(* Extension to recursive types and polymorphic variants *)
(* by Jacques Garrigue *)

type (_,_) ty =
  | Int: (int,_) ty
  | String: (string,_) ty
  | List: ('a,'e) ty -> ('a list, 'e) ty
  | Option: ('a,'e) ty -> ('a option, 'e) ty
  | Pair: (('a,'e) ty * ('b,'e) ty) -> ('a * 'b,'e) ty
  | Var: ('a, 'a -> 'e) ty
  | Rec: ('a, 'a -> 'e) ty -> ('a,'e) ty
  | Pop: ('a, 'e) ty -> ('a, 'b -> 'e) ty
  | Conv: string * ('a -> 'b) * ('b -> 'a) * ('b, 'e) ty -> ('a, 'e) ty

type _ ty_env =
  | Enil : unit ty_env
  | Econs : ('a,'e) ty * 'e ty_env -> ('a -> 'e) ty_env
;;

type variant =
  | VInt of int
  | VString of string
  | VList of variant list
  | VOption of variant option
  | VPair of variant * variant
  | VConv of string * variant

let may_map f = function Some x -> Some (f x) | None -> None

let rec variantize : type a e. e ty_env -> (a,e) ty -> a -> variant =
  fun e ty v ->
  match ty with
  | Int -> VInt v
  | String -> VString v
  | List t -> VList (List.map (variantize e t) v)
  | Option t -> VOption (may_map (variantize e t) v)
  | Pair (t1, t2) -> VPair (variantize e t1 (fst v), variantize e t2 (snd v))
  | Rec t -> variantize (Econs (ty, e)) t v
  | Pop t -> (match e with Econs (_, e') -> variantize e' t v)
  | Var -> (match e with Econs (t, e') -> variantize e' t v)
  | Conv (s, proj, inj, t) -> VConv (s, variantize e t (proj v))
;;

let wrap_A t = Conv ("`A", (fun (`A x) -> x), (fun x -> `A x), t);;

let ty a = Rec (wrap_A (Option (Pair (a, Var)))) ;;
let v = variantize Enil (ty Int);;
let x = v (`A (Some (1, `A (Some (2, `A None))))) ;;
