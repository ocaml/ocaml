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
      Format.fprintf ppf "%s " c;
      Format.fprintf ppf "(";
      iteri (fun i x -> if i <> 0 then Format.fprintf ppf ", "; print ppf x) l;
      Format.fprintf ppf ")"

type 'a t = A of 'a | B of ('a * 'a) t

let () =
  add_printer0 DInt.inspect (fun ppf x -> Format.fprintf ppf "%i" x);
  add_printer0 DFloat.inspect (fun ppf x -> Format.fprintf ppf "%f" x);
  add_printer0 DString.inspect (fun ppf x -> Format.fprintf ppf "%s" x);

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
