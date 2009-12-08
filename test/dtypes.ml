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
      Format.fprintf ppf "(%s " c;
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
  ()



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

let rec variantize d =
  match DInt.inspect d with Some x -> V_int x | None ->
  match DFloat.inspect d with Some x -> V_float x | None ->
  match DString.inspect d with Some x -> V_string x | None ->
  match DBool.inspect d with Some x -> V_bool x | None ->
  match DList.inspect d with Some v -> let module V = (val v : DList.V) in V_list (List.map (fun e -> variantize (dyn V.b e)) V.x) | None ->
  match DArray.inspect d with Some v -> let module V = (val v : DArray.V) in V_array (List.map (fun e -> variantize (dyn V.b e)) (Array.to_list V.x)) | None ->
  match DOption.inspect d with Some v -> let module V = (val v : DOption.V) in V_option (match V.x with None -> None | Some x -> Some (variantize (dyn V.b x))) | None ->
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
  ()
