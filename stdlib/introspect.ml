module Desc = struct
  type approx =
    | Any
    | Char
    | Int
    | Constants of string array
    | Polymorphic_variants

  type view =
    | Unknown
    | Array of approx
    | Tuple  of { name: string; tag: int; fields: approx array }
    | Record of { name: string; tag: int; fields: (string * approx) array }
    | Polymorphic_variant
    | Polymorphic_variant_constant of string

  type t = view

  let compare a b =
    match a, b with
    | Unknown, Unknown -> 0
    | Unknown, _ -> -1
    | _, Unknown -> +1
    | a, b -> compare a b

  let equal a b = compare a b = 0

  let view x = x

  let dump_array o f = function
    | [||] -> o "[||]"
    | members ->
      o "[|";
      f o members.(0);
      for i = 1 to Array.length members - 1 do
        o ";";
        f o members.(i)
      done;
      o "|]"

  let dump_escaped o s =
    o (Printf.sprintf "%S" s)

  let dump_approx o = function
    | Any -> o "Any"
    | Char -> o "Char"
    | Int -> o "Int"
    | Polymorphic_variants -> o "Polymorphic_variants"
    | Constants strings ->
        o "Constants ";
        dump_array o dump_escaped strings

  let dump_field o (f, apx) =
    o "(";
    dump_approx o apx;
    o ", ";
    dump_escaped o f;
    o ")"

  let dump o = function
    | Unknown ->
        o "Unknown"
    | Array apx ->
        o "Array ";
        dump_approx o apx
    | Tuple {name; tag; fields} ->
        o "Tuple {name = ";
        dump_escaped o name;
        o "; tag = ";
        o (string_of_int tag);
        o "; fields = ";
        dump_array o dump_approx fields;
        o "}"
    | Record {name; tag; fields} ->
        o "Record {name = ";
        dump_escaped o name;
        o "; tag = ";
        o (string_of_int tag);
        o "; fields = ";
        dump_array o dump_field fields;
        o "}"
    | Polymorphic_variant ->
        o "Polymorphic_variant"
    | Polymorphic_variant_constant name ->
        o "Polymorphic_variant_constant ";
        dump_escaped o name

  let to_string t =
    let buf = Buffer.create 63 in
    dump (Buffer.add_string buf) t;
    Buffer.contents buf

  let hash_combine seed v = Hashtbl.seeded_hash_param 10 100 seed v

  let hash_array h arr =
    (* Avoid dependency on Array *)
    let h = ref h in
    for i = 0 to Array.length arr - 1 do
      h := hash_combine !h arr.(i)
    done;
    !h

  let hash_variant s =
    let accu = ref 0 in
    for i = 0 to String.length s - 1 do
      accu := 223 * !accu + Char.code s.[i]
    done;
    (* reduce to 31 bits *)
    accu := !accu land (1 lsl 31 - 1);
    (* make it signed for 64 bits architectures *)
    if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

  let hash = function
    | Unknown -> 0
    | Array approx ->
        hash_combine 2 approx
    | Tuple { name; tag; fields } ->
        hash_combine (hash_combine (hash_combine 5 tag) name) fields
    | Record { name; tag; fields } ->
        hash_array (hash_combine (hash_combine 6 tag) name) fields
    | Polymorphic_variant -> 7
    | Polymorphic_variant_constant _ -> 0

  external read_self_descriptors : unit -> t list =
    "caml_read_bdsc_section"

  external compiler_descriptors : unit -> t list ref =
    "caml_compiler_block_descs"
end

module Index = struct
  let reserved_mask = (1 lsl Obj.reserved_bits ()) - 1
  let descriptor_index x = reserved_mask land (Desc.hash x)

  type t = {
    descriptors : (int, Desc.t list) Hashtbl.t;
    variants : (int, string list) Hashtbl.t;
  }

  let make () : t = {
    descriptors = Hashtbl.create 17;
    variants = Hashtbl.create 17;
  }

  let register (t : t) = function
    | Desc.Polymorphic_variant_constant name ->
        let i = Desc.hash_variant name in
        begin match Hashtbl.find t.variants i with
        | exception Not_found -> Hashtbl.add t.variants i [name]
        | names -> Hashtbl.replace t.variants i (name :: names)
        end
    | tag ->
        let i = descriptor_index tag in
        begin match Hashtbl.find t.descriptors i with
        | exception Not_found -> Hashtbl.add t.descriptors i [tag]
        | tags -> Hashtbl.replace t.descriptors i (tag :: tags)
        end

  let register_list t tags =
    List.iter (register t) tags

  let lookup (t : t) i =
    try Hashtbl.find t.descriptors i
    with Not_found -> []

  let lookup_by_reserved_bits t o =
    lookup t (Obj.get_reserved o)

  let lookup_variant (t : t) i =
    try Hashtbl.find t.variants i
    with Not_found -> []

  let self_descriptors = ref []

  let update_descriptors index descriptors =
    match !self_descriptors with
    | last_descriptors when descriptors != last_descriptors ->
      self_descriptors := descriptors;
      let rec aux = function
        | descriptors when descriptors == last_descriptors -> ()
        | [] -> ()
        | x :: xs ->
            register index x;
            aux xs
      in
      aux descriptors
    | _ -> ()

  let self_index = lazy (
    let descriptors = Desc.read_self_descriptors () in
    let result = make () in
    update_descriptors result descriptors;
    result
  )

  let self_index () =
    let lazy index = self_index in
    update_descriptors index !(Desc.compiler_descriptors ());
    index
end

module Dyn = struct
  type t = Desc.approx * Obj.t
  let get_approx (approx, _ : t) = approx
  let get_obj (_, obj : t) = obj

  let lift ?(approx=Desc.Any) (obj : Obj.t) : t = (approx, obj)
  let lift_any ?approx obj = lift ?approx (Obj.repr obj)

  type 'a fields = {count: int; get: int -> 'a}
  let field_count f = f.count
  let field_get f i =
    if (i < 0 || i >= f.count) then
      invalid_arg "Introspect.Desc.field_get: index out of bounds";
    f.get i

  type view =
    | String of string
    (* [String "foo"] = "foo" *)
    | Float of float
    (* [Float 12.12] = 12.12 *)
    | Char of char
    (* [Char 'c'] = 'x' *)
    | Int_or_constant of int * string list
    (* [Int_or_constant (1, ["`Bla"])] = 1 or `Bla *)
    | Constant of string list
    (* [Constant ["`Bla"]] = `Bla *)
    | Array of t fields
    (* [Array f] = [|f0, f1, f2, ...|] *)
    | Tuple of { name: string; fields: t fields }
    (* [Tuple f] = (f0, f1, f2, ...) *)
    | Record of { name: string; fields: (string * t) fields }
    (* [Record f] = { fst f0 : snd f0; fst f1 : snd f1; ... } *)
    | Polymorphic_variant of string * t
    | Closure | Lazy | Abstract | Custom | Unknown

  let double_to_wo_shift = match Sys.word_size with
    | 64 -> 0
    | _  -> 1

  let fields_of_block f obj =
    let count, get =
      if Obj.tag obj = Obj.double_array_tag then
        (Obj.size obj lsr double_to_wo_shift,
         fun i -> f i (Obj.repr (Obj.double_field obj i)))
      else
        (Obj.size obj, fun i -> f i (Obj.field obj i))
    in
    {count; get}

  let find_tag t obj =
    let otag = Obj.tag obj in
    if otag = Obj.int_tag then None
    else
      let osize = Obj.size obj in
      let select = function
        | Desc.Array _ -> true
        | Desc.Polymorphic_variant -> osize = 2
        | Desc.Tuple t ->
            otag = t.tag && osize = Array.length t.fields
        | Desc.Record t ->
            otag = t.tag &&
            let len = Array.length t.fields in
            let len =
              if otag = Obj.double_array_tag
              then len lsl double_to_wo_shift else len
            in
            osize = len
        | Desc.Polymorphic_variant_constant _ -> false
        | Desc.Unknown -> false
      in
      List.find_opt select (Index.lookup_by_reserved_bits t obj)

  let no_approx' (_ : int) (obj : Obj.t) = (Desc.Any, obj)

  let view_raw (obj : Obj.t) =
    if Obj.is_int obj then
      Int_or_constant (Obj.obj obj, [])
    else
      let tag = Obj.tag obj in
      if tag <= Obj.last_non_constant_constructor_tag then (
        if tag = 0
        then Tuple { name = ""; fields = fields_of_block no_approx' obj }
        else Tuple { name = "Tag#" ^ string_of_int tag;
                     fields = fields_of_block no_approx' obj }
      ) else if tag = Obj.string_tag then
        String (Obj.obj obj)
      else if tag = Obj.double_tag then
        Float (Obj.obj obj)
      else if tag = Obj.double_array_tag then
        Array (fields_of_block no_approx' obj)
      else if tag = Obj.closure_tag then
        Closure
      else if tag = Obj.lazy_tag then
        Lazy
      else if tag = Obj.abstract_tag then
        Abstract
      else if tag = Obj.custom_tag then
        Custom
      else
        Unknown

  let view ?(index=Index.self_index ()) (approx, obj) =
    if Obj.is_int obj then
      let i = (Obj.obj obj : int) in
      match approx with
      | Desc.Any ->
          Int_or_constant (i, Index.lookup_variant index i)
      | Desc.Int ->
          Int_or_constant (i, [])
      | Desc.Char ->
          (try Char (Char.chr i) with _ -> Int_or_constant (i, []))
      | Desc.Constants names ->
          if i >= 0 && i < Array.length names
          then Constant [names.(i)]
          else Int_or_constant (i, [])
      | Desc.Polymorphic_variants ->
          begin match Index.lookup_variant index i with
          | [] -> Int_or_constant (i, [])
          | names -> Constant (List.map ((^)"`") names)
          end
    else
      match find_tag index obj with
      | Some (Desc.Array approx ) ->
          Array (fields_of_block (fun _ obj -> approx, obj) obj)
      | Some (Desc.Record {name; fields}) ->
          let get_field i obj =
            let fname, fapprox = fields.(i) in
            (fname, (fapprox, obj))
          in
          Record { name; fields = fields_of_block get_field obj }
      | Some (Desc.Tuple {name; fields}) ->
          let get_field i obj = (fields.(i), obj) in
          Tuple {name; fields = fields_of_block get_field obj}
      | Some (Desc.Polymorphic_variant) ->
          let name = (Obj.obj (Obj.field obj 0) : int) in
          let payload = Obj.field obj 1 in
          begin match Index.lookup_variant index name with
          | [] -> Polymorphic_variant (string_of_int name, lift payload)
          | name :: _ -> Polymorphic_variant (name, lift payload)
          end
      | Some (Desc.Unknown)
      | Some (Desc.Polymorphic_variant_constant _)
      | None -> view_raw obj

  let view_obj ?index ?approx obj = view ?index (lift ?approx obj)
  let view_any ?index ?approx obj = view ?index (lift_any ?approx obj)
end

module Print = struct
  open Format

  let rec print_record ppf fields =
    for i = 0 to Dyn.field_count fields - 1 do
      let name, value = Dyn.field_get fields i in
      fprintf ppf "@[%s = %a@];@ " name pp_dynobj value
    done

  and print_fields sep ppf fields =
    for i = 0 to Dyn.field_count fields - 1 do
      let value = Dyn.field_get fields i in
      if i > 0 then fprintf ppf "%s@ " sep;
      fprintf ppf "@[%a@]" pp_dynobj value
    done

  and pp_dynval ppf : Dyn.view -> _ = function
    | Dyn.String s ->
        fprintf ppf "%S" s
    | Dyn.Float f ->
        fprintf ppf "%f" f
    | Dyn.Char c ->
        fprintf ppf "%C" c
    | Dyn.Int_or_constant (i, keys) ->
        fprintf ppf "%d" i;
        List.iter (fprintf ppf " or `%s") keys
    | Dyn.Constant names ->
        fprintf ppf "%s" (String.concat " or " names)
    | Dyn.Array arr ->
        fprintf ppf "[|@[<hv>%a@]|]" (print_fields ";") arr
    | Dyn.Tuple { name ="::"; fields } when Dyn.field_count fields = 2 ->
        fprintf ppf "[@[<hv>%a@]]" (print_list true) fields
    | Dyn.Tuple { name; fields } ->
        fprintf ppf "%s(@[<hv>%a@])"
          name (print_fields ",") fields
    | Dyn.Record {name; fields} ->
        fprintf ppf "%s{@[<hv>%a@]}" name print_record fields
    | Dyn.Polymorphic_variant (name, payload) ->
        fprintf ppf "`%s(@[<hv>%a@])" name pp_dynobj payload
    | Dyn.Closure  -> fprintf ppf "<Closure>"
    | Dyn.Lazy     -> fprintf ppf "<Lazy>"
    | Dyn.Abstract -> fprintf ppf "<Abstract>"
    | Dyn.Custom   -> fprintf ppf "<Custom>"
    | Dyn.Unknown  -> fprintf ppf "<Unknown>"

  and print_list first ppf fields =
    if not first then fprintf ppf ";@ ";
    let car = Dyn.field_get fields 0 in
    let cdr = Dyn.field_get fields 1 in
    fprintf ppf "%a" pp_dynobj car;
    match Dyn.view cdr with
    | Dyn.Constant ["[]"]
    | Dyn.Int_or_constant (0, _) -> ()
    | Dyn.Tuple {name = "::"; fields} when Dyn.field_count fields = 2 ->
        print_list false ppf fields
    | _ -> fprintf ppf "<malformed list>"

  and pp_dynobj ppf obj =
    pp_dynval ppf (Dyn.view obj)

  let format_any ppf obj =
    pp_dynobj ppf (Dyn.lift (Obj.repr obj))

  let print_any obj =
    fprintf Format.std_formatter "%a%!" format_any obj

  let prerr_any obj =
    fprintf Format.err_formatter "%a%!" format_any obj

  let print_any_endline obj =
    fprintf Format.std_formatter "%a\n%!" format_any obj

  let prerr_any_endline obj =
    fprintf Format.err_formatter "%a\n%!" format_any obj
end
