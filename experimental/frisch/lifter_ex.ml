type variant =
  | Record of (string * variant) list
  | Constr of (string * variant list)
  | List of variant list
  | Tuple of variant list
  | Int of int
  | String of string
  | Char of char
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Location of Location.t

let rec print_variant ppf = function
  | Record fl ->
      Format.fprintf ppf "{";
      List.iter (fun (f, v) -> Format.fprintf ppf "%s=%a;" f print_variant v) fl;
      Format.fprintf ppf "}"
  | Constr (c, []) ->
      Format.fprintf ppf "%s" c
  | Constr (c, hd :: tl) ->
      Format.fprintf ppf "%s(%a" c print_variant hd;
      List.iter (fun v -> Format.fprintf ppf ",%a" print_variant v) tl;
      Format.fprintf ppf ")"
  | List l ->
      Format.fprintf ppf "[";
      List.iter (fun v -> Format.fprintf ppf "%a;" print_variant v) l;
      Format.fprintf ppf "]"
  | Tuple (hd :: tl) ->
      Format.fprintf ppf "(%a" print_variant hd;
      List.iter (fun v -> Format.fprintf ppf ",%a" print_variant v) tl;
      Format.fprintf ppf ")"
  | String s ->
      Format.fprintf ppf "%S" s
  | _ ->
      Format.fprintf ppf ".."

let variantize = object
  inherit [_] Lifter.lifter
  method mk_record x = Record x
  method mk_constr x = Constr x
  method mk_list x = List x
  method mk_tuple x = Tuple x
  method mk_int x = Int x
  method mk_string x = String x
  method mk_char x = Char x
  method mk_int32 x = Int32 x
  method mk_int64 x = Int64 x
  method mk_nativeint x = Nativeint x
  method mk_location x = Location x
end

let e = Parse.expression (Lexing.from_string "1 + 3")
let () =
  Format.printf "%a@." print_variant (variantize # lift_Parsetree_expression e)


