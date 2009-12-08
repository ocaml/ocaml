open Dyntypes

module X : sig
  val static: 'a ttype -> dyn -> 'a option
end = struct
  let static t d =
    let module M = (val d : DYN) in
    match equal M.t t with
    | None -> None
    | Some eq -> Some (TypEq.app eq M.x)
end

let iteri f =
  let rec aux i = function
    | [] -> ()
    | hd :: tl -> f i hd; aux (succ i) tl
  in
  aux 0

(* An extensible printer of dynamic values. It fails by default on
    abstract values but it can be extended to deal with them. *)

let printers = ref []
let add_printer f = printers := f :: !printers

let add_printer0 extr pr =
  add_printer
    (fun ppf d ->
      match extr d with
      | None -> false
      | Some x -> pr ppf x; true
    )

module Printer1(X : TYPE1)(P : sig val print: Format.formatter -> 'a ttype -> 'a X.t -> unit end) =
  struct
    let () =
      add_printer
        (fun ppf d ->
          match X.inspect d with
          | None -> false
          | Some a ->
              let module A = (val a : X.V) in
              P.print ppf A.b A.x;
              true
        )
  end

let rec print ppf d =
  if List.exists (fun f -> f ppf d) !printers then ()
  else match inspect d with
  | DV_tuple l ->
      Format.fprintf ppf "(";
      iteri (fun i x -> if i <> 0 then Format.fprintf ppf ", "; print ppf x) l;
      Format.fprintf ppf ")"
  | DV_record l ->
      Format.fprintf ppf "{";
      iteri (fun i (s, x) -> if i <> 0 then Format.fprintf ppf "; "; Format.fprintf ppf "%s=%a" s print x) l;
      Format.fprintf ppf "}"
  | DV_constructor (c, []) ->
      Format.fprintf ppf "%s" c
  | DV_constructor (c, l) ->
      Format.fprintf ppf "%s (" c;
      iteri (fun i x -> if i <> 0 then Format.fprintf ppf ", "; print ppf x) l;
      Format.fprintf ppf ")"

type 'a t = A of 'a | B of ('a * 'a) t

let () =
  add_printer0 DInt.inspect (fun ppf x -> Format.fprintf ppf "%i" x);
  add_printer0 DFloat.inspect (fun ppf x -> Format.fprintf ppf "%f" x);
  add_printer0 DString.inspect (fun ppf x -> Format.fprintf ppf "%S" x);

  let module PList = Printer1(DList)(struct
    let print ppf t l =
      Format.fprintf ppf "[";
      iteri
        (fun i x ->
          if i > 0 then Format.fprintf ppf "; ";
          print ppf (dyn t x)
        ) l;
      Format.fprintf ppf "]"
  end) in
  let module PArray = Printer1(DArray)(struct
    let print ppf t l =
      Format.fprintf ppf "[|";
      Array.iteri
        (fun i x ->
          if i > 0 then Format.fprintf ppf "; ";
          print ppf (dyn t x)
        ) l;
      Format.fprintf ppf "|]"
   end) in
  add_printer
    (fun ppf d ->
      match DArrow.inspect d with
      | None -> false
      | Some _ -> Format.fprintf ppf "<fun>"; true
    );
  ()

let () =
  let rec f d =
    Format.printf "--> %a@." print d;
    (match X.static (type _) d with None -> print_endline "None" | Some s -> print_endline s);
    (match X.static (type _) d with None -> print_endline "None" | Some i -> print_endline (string_of_int i));

    match DList.inspect d with
    | None -> print_endline "Not a list"
    | Some w ->
        let module W = (val w : DList.V) in
        List.iter f (List.map (dyn W.b) W.x)
  in
  f (dyn (type _) "ABC");
  f (dyn (type _) 3);
  f (dyn (type _) [["X"; "Y"];[]]);
  f (dyn (type _) (Some (ref (2, "X"))));
  f (dyn (type _) (B (A (3, 4))));
  f (dyn (type _) (stype_of_ttype (type int option)));
  f (dyn (type _) [| (3, false); (0, true) |]);
  f (dyn (type _) succ);
  ()


(* An universal variant type. *)

type variant =
  | V_int of int
  | V_string of string
  | V_float of float
  | V_bool of bool
  | V_list of variant list
  | V_tuple of variant list
  | V_array of variant list
  | V_option of variant option
  | V_record of (string * variant) list
  | V_constructor of string * variant list
  | V_arrow of string * (variant -> variant)

let devariantize_forward = ref (object method toval: 'a. 'a ttype -> variant -> 'a = fun _ _ -> assert false end)
let devariantize t v = (!devariantize_forward) # toval t v

let rec variantize d =
  match DInt.inspect d with Some x -> V_int x | None ->
  match DFloat.inspect d with Some x -> V_float x | None ->
  match DString.inspect d with Some x -> V_string x | None ->
  match DBool.inspect d with Some x -> V_bool x | None ->
  match DList.inspect d with Some v -> let module V = (val v : DList.V) in V_list (List.map (fun e -> variantize (dyn V.b e)) V.x) | None ->
  match DArray.inspect d with Some v -> let module V = (val v : DArray.V) in V_array (List.map (fun e -> variantize (dyn V.b e)) (Array.to_list V.x)) | None ->
  match DOption.inspect d with Some v -> let module V = (val v : DOption.V) in V_option (match V.x with None -> None | Some x -> Some (variantize (dyn V.b x))) | None ->
  match DArrow.inspect d with Some v -> let module V = (val v : DArrow.V) in V_arrow (V.lab, fun arg -> variantize (dyn V.codom (V.f (devariantize V.dom arg)))) | None ->
    match inspect d with
    | DV_tuple l -> V_tuple (List.map variantize l)
    | DV_record l -> V_record (List.map (fun (s, x) -> (s, variantize x)) l)
    | DV_constructor (c, l) -> V_constructor (c, List.map variantize l)

let print_variant ppf (v : variant) =
  print ppf (dyn (type _) v)

let () =
  let f t x = Format.printf "%a@." print_variant (variantize (dyn t x)) in
  f (type _) 10;
  f (type _) [| "A"; "B" |];
  f (type _) (true, Some 10, (None : int option));
  f (type _) succ;
  ()


module Devariantizer1(X : TYPE1)(D : sig val f: 'a ttype -> variant -> 'a X.t end) = struct
  let f (type t) t v =
    match X.check t with
    | None -> None
    | Some m ->
        let module M = (val m : X.T with type a = t) in
        Some (TypEq.app (TypEq.sym M.eq) (D.f M.b v))
end

let rec devariantize: 'a. 'a ttype -> variant -> 'a = fun (type t) t v ->
  let simple tt f () = match equal tt t with Some eq -> Some (TypEq.app eq (f v)) | None -> None in
  let (||) f1 f2 () = match f1 () with Some x -> x | None -> f2 () in
  let module DevariantizeList =
    Devariantizer1(DList)(struct let f t = function
      | V_list x -> List.map (devariantize t) x
      | _ -> failwith "list expected"
    end)
  in
  let module DevariantizeArray =
    Devariantizer1(DArray)(struct let f t = function
      | V_array x -> Array.map (devariantize t) (Array.of_list x)
      | _ -> failwith "array expected"
    end)
  in
  let module DevariantizeOption =
    Devariantizer1(DOption)(struct let f t = function
      | V_option None -> None
      | V_option (Some x) -> Some (devariantize t x)
      | _ -> failwith "array expected"
    end)
  in
  let ofv v = object method toval: 'b. 'b ttype -> 'b = fun t -> devariantize t v end in
  begin
    simple DInt.ttype (function V_int x -> x | _ -> failwith "int expected") ||
    simple DBool.ttype (function V_bool x -> x | _ -> failwith "bool expected") ||
    simple DString.ttype (function V_string x -> x | _ -> failwith "string expected") ||
    simple DFloat.ttype (function V_float x -> x | _ -> failwith "float expected") ||
    (fun () -> DevariantizeList.f t v) ||
    (fun () -> DevariantizeArray.f t v) ||
    (fun () -> DevariantizeOption.f t v) ||
    (fun () ->
      match v with
      | V_tuple vl ->
          build t (DV_tuple (List.map ofv vl))
      | V_constructor (c, vl) ->
          build t (DV_constructor (c, List.map ofv vl))
      | V_record l ->
          build t (DV_record (List.map (fun (s, v) -> s, ofv v) l))
      | V_arrow (lab, f) ->
          begin match DArrow.check t with
          | None -> failwith "arrow type expected"
          | Some m ->
              let module M = (val m : DArrow.T with type a = t) in
              if lab <> M.lab then failwith "wrong function label";
              let f arg =
                devariantize M.codom (f (variantize (dyn M.dom arg)))
              in
              TypEq.app (TypEq.sym M.eq) f
          end
      | _ -> assert false
    )
  end ()

let () =
  devariantize_forward := object
    method toval: 'a. 'a ttype -> variant -> 'a = devariantize
  end

type fr = {x: float; y: float}
type t = A of int * string | B | C | D of bool

let () =
  let f t v =
    Format.printf "=> %a@." print (dyn t (devariantize t v))
  in
  f (type string * string * int) (V_tuple [V_string "A"; V_string "B"; V_int 3]);
  f (type string ref) (V_record ["contents", V_string "X"]);
  f (type fr) (V_record ["x", V_float 3.; "y", V_float 4.]);
  f (type t list) (V_list [V_constructor ("A", [V_int 2; V_string "A"]);
                           V_constructor ("B", []);
                           V_constructor ("C", []);
                           V_constructor ("D", [V_bool true])]);
  let u = devariantize (type int -> int) (V_arrow ("", function V_int x -> V_int (x * 2) | _ -> assert false)) in
  Format.printf "%i // %i@." (u 10) (u 3);
  let v = variantize (dyn (type _) succ) in
  Format.printf "%a@." print_variant (match v with V_arrow (_, f) -> f (V_int 10) | _ -> assert false);
  ()


(* Demonstrating custom abstract type. *)

module MyModule : sig
  type t
  val t: t ttype
  val x: t

  module Abstract: TYPE0 with type t = t
end = struct
  type t = int * int
  let t = (type _)
  let x = (2, 3)

  type t_ = t
  module Abstract = Abstract0(struct let name = "MyModule.t"  type t = t_ end)
  let () =
    add_printer0 Abstract.inspect (fun ppf (x, y) -> Format.fprintf ppf "< %i | %i >" x y)
end

let () =
  Format.printf "%a@." print (dyn MyModule.t MyModule.x)

let () =
  let type = MyModule.t in
  Format.printf "%a@." print (dyn (type _) (Some MyModule.x))

let () =
  let module M = struct
    let type = MyModule.t
    let () = Format.printf "%a@." print (dyn (type _) (Some MyModule.x))
  end
  in
  ()

let () =
  let type = MyModule.Abstract.ttype in
  Format.printf "%a@." print (dyn (type _) (Some MyModule.x))
