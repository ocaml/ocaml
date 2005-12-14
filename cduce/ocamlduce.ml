open Cduce_types
open Encodings
module U = Utf8

type t = {{ Any }}

let print ppf v =
  Value.print ppf {: v :}

let to_string v =
  let b = Buffer.create 255 in
  let ppf = Format.formatter_of_buffer b in
  print ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents b

let equal x y = Value.equal {: x :} {:  y :}
let compare x y = Value.compare {: x :} {: y :}
let hash x = Value.hash {: x :}


let get_utf8 x = fst (Value.get_string_utf8 {:x:})
let mk_utf8 x : {{ String }} = Obj.magic (Value.string_utf8 x)

module Utf8 = struct
  type t = {{ String }}
  let make x = match Utf8.mk_check (String.copy x) with 
    | Some x -> mk_utf8 x
    | None -> failwith "Utf8.make"

  let get x = Utf8.get_str (get_utf8 x)
end

module Latin1 = struct
  type t = {{ Latin1 }}

  let get x = Value.get_string_latin1 {: x :}
  let make x : t = Obj.magic (Value.string_latin1 (String.copy x))
end

module Int = struct
  type t = {{ Int }}
  type repr = {{ [('-'|'+')? '0'--'9'+] }}

  let make s = Obj.magic (Value.Integer (Intervals.V.mk {: s :}))
  let get s =
    match {:s:} with
      | Value.Integer i ->
	  Obj.magic (Value.string_latin1 (Intervals.V.to_string i))
      | _ -> assert false
end

module NonnegInt = struct
  type t = {{ 0--** }}
  type repr = {{ ['0'--'9'+] }}

  let make s = Obj.magic (Int.make s)
  let get s = Obj.magic (Int.get s)
end

module Namespace = struct
  open Cduce_types.Ns
  type t = Uri.t
  let make v = Uri.mk (get_utf8 v)
  let get v = mk_utf8 (Uri.value v)
  let empty = empty
  let xml = xml_ns

  let compare = Uri.compare
  let hash = Uri.hash
  let equal = Uri.equal
end


module Atom = struct
  type t = {{ Atom }}
  type qname = Namespace.t * {{ String }}
  let make (ns,s) = 
    Obj.magic (Value.Atom (Cduce_types.Atoms.V.mk (ns, get_utf8 s)))
  let get v = 
    match ({:v:} : Value.t) with
      | Value.Atom v -> 
	  let (ns,s) = Cduce_types.Atoms.V.value v in
	  (ns,mk_utf8 s)
      | _ -> assert false

  let compare x y = compare x y
  let hash x = hash x
  let equal x y = equal x y
end

module NamespaceTable = struct
  type t = Ns.table
  let make l = Ns.mk_table (List.map (fun (pr,ns) -> get_utf8 pr, ns) l)
  let get t = List.map (fun (pr,ns) -> mk_utf8 pr, ns) (Ns.get_table t)

  let resolve_prefix t pr = Ns.map_prefix t (get_utf8 pr) 
  let resolve_qname t s =
    match s with 
	{{ [ pr::_* ':' local::(_ - ':')* ] | (local & (pr := "")) }} ->
	  Atom.make (resolve_prefix t pr, local)
end

module NamespaceScope = struct
  type elt = {{ <(Any) (Any)>Any }}

  let get v = match {: v :} with
    | Value.XmlNs (_,_,_,ns) -> ns
    | _ -> raise Not_found 

  let set v ns =  match {: v :} with
    | Value.XmlNs (v1,v2,v3,_) | Value.Xml (v1,v2,v3) -> 
	Obj.magic (Value.XmlNs (v1,v2,v3,ns))
    | _ -> assert false

  let resolve_prefix e = NamespaceTable.resolve_prefix (get e)
  let resolve_qname e = NamespaceTable.resolve_qname (get e)
end


module Print = struct
  open Value
  open Ident
  exception Error of string * {{ Any }}

  let serialize ~start_elem ~end_elem ~empty_elem ~text ?(ns=Ns.empty_table) v =
    let v = {: v :} in
    let printer = Ns.Printer.printer ns in

    let to_str = U.get_str in

    let mk_attrs =
      List.map (fun (n,v) ->
		  Ns.Printer.attr printer (Label.value n), to_str v) in
    let mk_xmlns = 
      List.map (fun (pr,ns) ->
		  let pr = to_str pr in
		  let pr = if pr = "" then "xmlns" else ("xmlns:" ^ pr) in
		  pr, to_str (Ns.Uri.value ns)) in
    let element_start n xmlns attrs = 
      start_elem (Ns.Printer.tag printer (Atoms.V.value n)) (mk_xmlns xmlns @ mk_attrs attrs)
    and empty_element n xmlns attrs = 
      empty_elem (Ns.Printer.tag printer (Atoms.V.value n))  (mk_xmlns xmlns @ mk_attrs attrs)
    and element_end n = 
      end_elem (Ns.Printer.attr printer (Atoms.V.value n))
    in
    
    let rec register_elt = function
      | Xml (Atom tag, Record attrs, content) 
      | XmlNs (Atom tag, Record attrs, content, _) ->
	  Imap.iter
	    (fun n _ -> Ns.Printer.register_qname printer 
	       (Label.value (Label.from_int n)))
	    attrs;
	  Ns.Printer.register_qname printer (Atoms.V.value tag);
	  register_content content
      | _ -> ()
    and register_content = function
      | String_utf8 (_,_,_,q)
      | String_latin1 (_,_,_,q) -> register_content q
      | Pair (x, q) -> register_elt x; register_content q
      | Concat (x,y) -> register_content x; register_content y
      | _ -> () 
    in
    register_elt v;
    
    let rec print_elt xmlns = function
      | Xml (Atom tag, Record attrs, content)
      | XmlNs (Atom tag, Record attrs, content, _) ->
	  let attrs = 
	    Imap.map_elements
	      (fun n v -> 
                 if is_str v then begin
                   let (s,q) = get_string_utf8 v in
                   match q with
                     | Atom a when a = Sequence.nil_atom -> 
                         (Label.from_int n), s
                     | _ -> 
			 raise 
			   (Error ("Attribute value is not a string", {{{:v:}}}))
                 end else
 		   raise (Error ("Attribute value is not a string", {{{:v:}}}))
	      ) attrs in
	  if Value.equal content Value.nil then
	    empty_element tag xmlns attrs
	  else (
	    element_start tag xmlns attrs;
	    print_content content;
	    element_end tag
	  )
      | v -> raise (Error ("Content value is not valid XML", {{{:v:}}}))
    and print_content v =
      let (s,q) = get_string_utf8 v in
      text (to_str s);
      match normalize q with
	| Pair ((Xml _ | XmlNs _) as x, q) -> print_elt [] x; print_content q
	| Atom a when a = Sequence.nil_atom -> ()
	| v -> raise (Error ("Content value is not valid XML", {{{:v:}}}))
    in
    print_elt (Ns.Printer.prefixes printer) v


  let data f s =
    let rec aux b i =
      if i = String.length s then f (String.sub s b (i-b))
      else match s.[i] with
	| '&' -> f (String.sub s b (i-b)); f "&amp;"; aux (succ i) (succ i)
	| '<' -> f (String.sub s b (i-b)); f "&lt;"; aux (succ i) (succ i)
	| '>' -> f (String.sub s b (i-b)); f "&gt;"; aux (succ i) (succ i)
	| '"' -> f (String.sub s b (i-b)); f "&quot;"; aux (succ i) (succ i)
	| _ -> aux b (succ i)
    in
    aux 0 0

  let escape f s =
    let rec aux b i =
      if i = String.length s then f (String.sub s b (i-b))
      else match s.[i] with
	| '&' -> f (String.sub s b (i-b)); f "&amp;"; aux (succ i) (succ i)
	| '<' -> f (String.sub s b (i-b)); f "&lt;"; aux (succ i) (succ i)
	| '>' -> f (String.sub s b (i-b)); f "&gt;"; aux (succ i) (succ i)
	| '"' -> f (String.sub s b (i-b)); f "&quot;"; aux (succ i) (succ i)
	| _ -> aux b (succ i) in
    aux 0 0

  let print_xml f ?(data=escape f) ?ns v =
    let open_markup tag attrs =
      f ("<" ^ tag);
      List.iter (fun (n,v) -> f (" " ^ n ^ "=\""); data v; f "\"") attrs
    in
    serialize
      ~start_elem:(fun tag attrs -> open_markup tag attrs; f ">")
      ~end_elem:(fun tag -> f ("</" ^ tag ^ ">"))
      ~empty_elem:(fun tag attrs -> open_markup tag attrs; f "/>")
      ~text:data
      ?ns
      v
end


module Load = struct
  open Ident
  open Value
  exception Error of string
  type anyxml = {{ <_ ..>[ (Char | anyxml)* ] }}

  type stk = 
    | Element of Value.t * stk
    | Start of Ns.table * Atoms.V.t * (Ns.Label.t * U.t) list * Ns.table * stk
    | String of string * stk
    | Empty

  type t = { 
    mutable stack : stk; 
    mutable ns_table : Ns.table;

    mutable buffer : string;
    mutable pos : int;
    mutable length : int;
    
    keep_ns: bool
  }

  let make ?(ns=false) () =
    { stack = Empty;
      ns_table = Ns.empty_table;
      buffer = String.create 1024;
      pos = 0;
      length = 1024;
      keep_ns = ns }
      
  let get loader =
    match loader.stack with
      | Element (x,Empty) -> Obj.magic x
      | _ -> raise (Error "No XML stream to parse")

  let resize loader n  =
    let new_len = loader.length * 2 + n in
    let new_buf = String.create new_len in
    String.unsafe_blit loader.buffer 0 new_buf 0 loader.pos;
    loader.buffer <- new_buf;
    loader.length <- new_len
      
  let text loader s =
    let len = String.length s in
    let new_pos = loader.pos + len in
    if new_pos > loader.length then resize loader len;
    String.unsafe_blit s 0 loader.buffer loader.pos len;
    loader.pos <- new_pos
      
  let rec only_ws s i =
    (i = 0) ||
      (let i = pred i in match (String.unsafe_get s i) with
	 | ' ' | '\t' | '\n' | '\r' -> only_ws s i
	 | _ -> false) 
      
      
  let string s q =
    let s = U.mk s in
    String_utf8 (U.start_index s,U.end_index s, s, q)
      

  let attrib att = 
    let att = List.map (fun (n,v) -> Upool.int n, string_utf8 v) att in
    Imap.create (Array.of_list att)

  let elem keep_ns ns tag att child =
    if keep_ns then
      XmlNs (Atom tag, Record (attrib att), child, ns)
    else
      Xml (Atom tag, Record (attrib att), child)
	
  let rec create_elt loader accu = function
    | String (s,st) -> create_elt loader (string s accu) st
    | Element (x,st) -> create_elt loader (Pair (x,accu)) st
    | Start (ns,name,att,old_table,st) -> 
	loader.stack <- Element (elem loader.keep_ns ns name att accu, st);
	loader.ns_table <- old_table
    | Empty -> assert false
	
  let flush_txt loader =
    if not (only_ws loader.buffer loader.pos) then 
      loader.stack <- 
	String (String.sub loader.buffer 0 loader.pos, loader.stack); 
    loader.pos <- 0


  let start_elem loader name att =
    flush_txt loader;
    let (table,name,att) = Ns.process_start_tag loader.ns_table name att in
    loader.stack <- Start (table,Atoms.V.mk name,att,loader.ns_table, loader.stack);
    loader.ns_table <- table
      
  let end_elem loader _ =
    let accu =
      if only_ws loader.buffer loader.pos 
      then nil 
      else string (String.sub loader.buffer 0 loader.pos) nil in
    loader.pos <- 0; 
    create_elt loader accu loader.stack

  let sub loader v =
    flush_txt loader;
    loader.stack <- Element ({:v:}, loader.stack)
end

