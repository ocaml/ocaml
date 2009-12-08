open Dyntypes

module X : sig
  val static: 'a ttype -> dyn -> 'a option
end = struct
  let static t d =
    let module M = (val d : DYN) in
    match equal () M.t t with
    | None -> None
    | Some eq -> Some (TypEq.app eq M.x)
end

let iteri f =
  let rec aux i = function
    | [] -> ()
    | hd :: tl -> f i hd; aux (succ i) tl
  in
  aux 0

let rec print ppf d =
  match inspect d with
  | DV_int i -> Format.fprintf ppf "%i" i
  | DV_string s -> Format.fprintf ppf "%S" s
  | DV_float f -> Format.fprintf ppf "%f" f
  | DV_tuple l ->
      Format.fprintf ppf "(";
      iteri (fun i x -> if i <> 0 then Format.fprintf ppf ", "; print ppf x) l;
      Format.fprintf ppf ")"
  | DV_record l ->
      Format.fprintf ppf "{";
      iteri (fun i (s, x) -> if i <> 0 then Format.fprintf ppf "; "; Format.fprintf ppf "%s=%a" s print x) l;
      Format.fprintf ppf "}"
  | DV_constructor ("[]", []) ->
      Format.fprintf ppf "[]"
  | DV_constructor (c, []) ->
      Format.fprintf ppf "%s" c
  | DV_constructor ("::", [hd; tl]) ->
      Format.fprintf ppf "[%a" print hd;
      let rec loop d =
        match inspect d with
        | DV_constructor ("::", [hd; tl]) ->
            Format.fprintf ppf "; %a" print hd;
            loop tl
        | DV_constructor ("[]", []) ->
            Format.fprintf ppf "]"
        | _ ->
            assert false
      in
      loop tl
  | DV_constructor (c, l) ->
      Format.fprintf ppf "%s " c;
      Format.fprintf ppf "(";
      iteri (fun i x -> if i <> 0 then Format.fprintf ppf ", "; print ppf x) l;
      Format.fprintf ppf ")"

type 'a t = A of 'a | B of ('a * 'a) t

let () =
  let f d =
    Format.printf "--> %a@." print d;
    (match X.static (type _) d with None -> print_endline "None" | Some s -> print_endline s);
    (match X.static (type _) d with None -> print_endline "None" | Some i -> print_endline (string_of_int i))
  in
  f (dyn (type _) "ABC");
  f (dyn (type _) 3);
  f (dyn (type _) (Some (ref (2, "X"))));
  f (dyn (type _) (B (A (3, 4))));
  f (dyn (type _) (stype_of_ttype (type int option)));
  ()
