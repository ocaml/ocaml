let enabled = Sys.introspection_enabled

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
    | Tuple of { name: string; tag: int; fields: (string * approx) array }
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
        dump_array o dump_field fields;
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
    known_variants: (string, unit) Hashtbl.t;
    variants : (int, string list) Hashtbl.t;
  }

  let make () : t = {
    descriptors = Hashtbl.create 17;
    known_variants = Hashtbl.create 17;
    variants = Hashtbl.create 17;
  }

  let register (t : t) = function
    | Desc.Polymorphic_variant_constant name ->
        if not (Hashtbl.mem t.known_variants name) then begin
          Hashtbl.add t.known_variants name ();
          let i = Desc.hash_variant name in
          match Hashtbl.find t.variants i with
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

  let self_libraries = ref []

  let update_libraries index libraries =
    match !self_libraries with
    | last_libraries when libraries != last_libraries ->
      self_libraries := libraries;
      let rec aux = function
        | libraries when libraries == last_libraries -> ()
        | [] -> ()
        | library :: libraries ->
            register_list index library;
            aux libraries
      in
      aux libraries
    | _ -> ()

  let dynamic_libraries = ref []

  let self_index () =
    let lazy index = self_index in
    update_descriptors index !(Desc.compiler_descriptors ());
    update_libraries index !dynamic_libraries;
    index

  let add_dynamic_library = function
    | [] -> ()
    | library -> dynamic_libraries := library :: !dynamic_libraries

  let dynamic_libraries () = !dynamic_libraries
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
    | Tuple of { name: string; fields: (string * t) fields }
    (* [Tuple f] = (f0, f1, f2, ...) *)
    | Record of { name: string; fields: (string * t) fields }
    (* [Record f] = { fst f0 : snd f0; fst f1 : snd f1; ... } *)
    | Polymorphic_variant of string * t
    (* [Extension ("Foo", uid, args)] = Foo args
       given exception Foo (of args)
          or type t += Foo (of args) *)
    | Extension of string * int * t fields
    | Closure | Lazy | Abstract | Custom | Unknown
    | Forward of t

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

  let no_approx (_ : int) (obj : Obj.t) = (Desc.Any, obj)

  let no_name (_ : int) (obj : Obj.t) = ("", (Desc.Any, obj))

  let as_extension_tag obj =
    if Obj.tag obj = Obj.object_tag && Obj.size obj = 2 then
      let name = Obj.field obj 0 in
      let uid = Obj.field obj 1 in
      if Obj.is_int uid && Obj.tag name = Obj.string_tag
      then Some ((Obj.obj name : string), (Obj.obj uid : int))
      else None
    else None

  let view_raw (obj : Obj.t) =
    if Obj.is_int obj then
      Int_or_constant (Obj.obj obj, [])
    else
      let tag = Obj.tag obj in
      if tag = 0 then (
        let size = Obj.size obj in
        if size >= 2 then
          match as_extension_tag (Obj.field obj 0) with
          | None ->
             Tuple { name = ""; fields = fields_of_block no_name obj }
          | Some (path, uid) ->
             let get i = no_approx 0 (Obj.field obj (i + 1)) in
             Extension (path, uid, {count = size - 1; get})
        else
          Tuple { name = ""; fields = fields_of_block no_name obj }
      ) else if tag <= Obj.last_non_constant_constructor_tag then
        Tuple { name = "Tag#" ^ string_of_int tag;
                fields = fields_of_block no_name obj }
      else if tag = Obj.string_tag then
        String (Obj.obj obj)
      else if tag = Obj.double_tag then
        Float (Obj.obj obj)
      else if tag = Obj.double_array_tag then
        Array (fields_of_block no_approx obj)
      else if tag = Obj.closure_tag then
        Closure
      else if tag = Obj.lazy_tag then
        Lazy
      else if tag = Obj.abstract_tag then
        Abstract
      else if tag = Obj.custom_tag then
        Custom
      else if tag = Obj.forward_tag then
        Forward (lift (Obj.field obj 0))
      else
        match as_extension_tag obj with
        | Some (path, uid) ->
           let get _ = assert false in
           Extension (path, uid, {count = 0; get})
        | None -> Unknown

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
          let get_field i obj =
            let name, approx = fields.(i) in
            (name, (approx, obj))
          in
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

  module H = Hashtbl.Make(struct type t = Obj.t let equal = (==) let hash = Hashtbl.hash end)

  let pp_fields f sep ppf fields =
    for i = 0 to Dyn.field_count fields - 1 do
      let value = Dyn.field_get fields i in
      if i > 0 then fprintf ppf "%s@ " sep;
      fprintf ppf "@[%a@]" f value
    done

  let pp_tuple_field f ppf = function
    | ("", v) -> fprintf ppf "@[%a@]" f v
    | (k, v)  -> fprintf ppf "~%s:@[%a@]" k f v

  let pp_record_field f ppf (k, v) =
    fprintf ppf "@[%s = %a@]" k f v

  let rec pp_list_elements table f ppf fields =
    begin match Dyn.field_get fields 0 with
    | "", car -> fprintf ppf "@[%a@]" f car;
    | lbl, car -> fprintf ppf "~%s:@[%a@]" lbl f car;
    end;
    let _lbl, cdr = Dyn.field_get fields 1 in
    match Dyn.view cdr with
    | Dyn.Constant ["[]"]
    | Dyn.Int_or_constant (0, _) -> ()
    | Dyn.Tuple {name = "::"; fields} when Dyn.field_count fields = 2 ->
        fprintf ppf ";@ ";
        let raw = Dyn.get_obj cdr in
        if H.mem table raw then
          fprintf ppf "<cycle>"
        else (
          H.add table raw ();
          pp_list_elements table f ppf fields;
          H.remove table raw
        )
    | _ -> fprintf ppf "<malformed list>"

  let pp_dynval table self ppf : Dyn.view -> _ = function
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
        fprintf ppf "[|@[<hv>%a@]|]" (pp_fields self ";") arr
    | Dyn.Tuple { name ="::"; fields } when Dyn.field_count fields = 2 ->
        fprintf ppf "[@[<hv>%a@]]" (pp_list_elements table self) fields
    | Dyn.Tuple { name; fields } ->
        if name <> "" then fprintf ppf "%s " name;
        fprintf ppf "(@[<hv>%a@])" (pp_fields (pp_tuple_field self) ",") fields
    | Dyn.Record {name; fields} ->
        if name <> "" then fprintf ppf "%s " name;
        fprintf ppf "{@[<hv>%a@]}" (pp_fields (pp_record_field self) ";") fields
    | Dyn.Extension (name, uid, fields) when Dyn.field_count fields = 0 ->
        fprintf ppf "%s/%d" name uid
    | Dyn.Extension (name, uid, fields) ->
        fprintf ppf "%s/%d (@[<hv>%a@])"
          name uid (pp_fields self ",") fields
    | Dyn.Polymorphic_variant (name, payload) ->
        fprintf ppf "`%s (@[<hv>%a@])" name self payload
    | Dyn.Closure  -> fprintf ppf "<closure>"
    | Dyn.Lazy     -> fprintf ppf "<lazy>"
    | Dyn.Abstract -> fprintf ppf "<abstract>"
    | Dyn.Custom   -> fprintf ppf "<custom>"
    | Dyn.Unknown  -> fprintf ppf "<unknown>"
    | Dyn.Forward d -> fprintf ppf "lazy (@[<hv>%a@])" self d

  let format_any ?index ?(depth=20) ?(steps=ref max_int) ppf obj =
    let table = H.create 7 in
    let rec aux depth ppf obj =
      if depth <= 0 || !steps <= 0 then
        Format.pp_print_string ppf "..."
      else (
        decr steps;
        let raw = Dyn.get_obj obj in
        let protect = Obj.is_block raw && Obj.tag raw < Obj.no_scan_tag in
        if protect && H.mem table raw then
          Format.pp_print_string ppf "<cycle>"
        else (
          if protect then H.add table raw ();
          pp_dynval table (aux (depth - 1)) ppf (Dyn.view ?index obj);
          if protect then H.remove table raw
        )
      )
    in
    aux (depth - 1) ppf (Dyn.lift (Obj.repr obj))

  let print_any ?index ?depth ?steps obj =
    fprintf Format.std_formatter "@[%a@]%!"
      (format_any ?index ?depth ?steps) obj

  let prerr_any ?index ?depth ?steps obj =
    fprintf Format.err_formatter "@[%a@]%!"
      (format_any ?index ?depth ?steps) obj

  let print_any_endline ?index ?depth ?steps obj =
    fprintf Format.std_formatter "@[%a@]\n%!"
      (format_any ?index ?depth ?steps) obj

  let prerr_any_endline ?index ?depth ?steps obj =
    fprintf Format.err_formatter "@[%a@]\n%!"
      (format_any ?index ?depth ?steps) obj
end
