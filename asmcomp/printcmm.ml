(* Pretty-printing of C-- code *)

open Format
open Cmm

let machtype_component = function
    Addr -> print_string "addr"
  | Int -> print_string "int"
  | Float -> print_string "float"

let machtype mty =
  match Array.length mty with
    0 -> print_string "unit"
  | n -> machtype_component mty.(0);
         for i = 1 to n-1 do
           print_string "*"; machtype_component mty.(i)
         done

let comparison = function
    Ceq -> print_string "=="
  | Cne -> print_string "!="
  | Clt -> print_string "<"
  | Cle -> print_string "<="
  | Cgt -> print_string ">"
  | Cge -> print_string ">="

let chunk = function
    Byte_unsigned -> print_string "unsigned byte"
  | Byte_signed -> print_string "signed byte"
  | Sixteen_unsigned -> print_string "unsigned half"
  | Sixteen_signed -> print_string "signed half"
  | Word -> ()

let operation = function
    Capply ty -> print_string "app"
  | Cextcall(lbl, ty, alloc) ->
      print_string "extcall \""; print_string lbl; print_string "\""
  | Cproj(ofs, len) ->
      print_string "proj "; print_int ofs;
      if len > 1 then begin print_string "-"; print_int (ofs + len - 1) end
  | Cload mty -> print_string "load"
  | Cloadchunk c -> print_string "load "; chunk c
  | Calloc -> print_string "alloc"
  | Cstore -> print_string "store"
  | Cstorechunk c -> print_string "store "; chunk c
  | Caddi -> print_string "+"
  | Csubi -> print_string "-"
  | Cmuli -> print_string "*"
  | Cdivi -> print_string "/"
  | Cmodi -> print_string "mod"
  | Cand -> print_string "and"
  | Cor -> print_string "or"
  | Cxor -> print_string "xor"
  | Clsl -> print_string "<<"
  | Clsr -> print_string ">>u"
  | Casr -> print_string ">>s"
  | Ccmpi c -> comparison c
  | Cadda -> print_string "+a"
  | Csuba -> print_string "-a"
  | Ccmpa c -> comparison c; print_string "a"
  | Caddf -> print_string "+f"
  | Csubf -> print_string "-f"
  | Cmulf -> print_string "*f"
  | Cdivf -> print_string "/f"
  | Cfloatofint -> print_string "floatofint"
  | Cintoffloat -> print_string "intoffloat"
  | Ccmpf c -> comparison c; print_string "f"
  | Craise -> print_string "raise"
  | Ccheckbound -> print_string "checkbound"

let rec expression = function
    Cconst_int n -> print_int n
  | Cconst_float s -> print_string s
  | Cconst_symbol s -> print_string "\""; print_string s; print_string "\""
  | Cconst_pointer n -> print_int n; print_string "a"
  | Cvar id -> Ident.print id
  | Clet(id, def, (Clet(_, _, _) as body)) ->
      open_hovbox 2;
      print_string "(let"; print_space();
      open_hovbox 1;
      print_string "(";
      open_hovbox 2;
      Ident.print id; print_space(); expression def;
      close_box();
      let rec letdef = function
        Clet(id, def, body) ->
          print_space();
          open_hovbox 2;
          Ident.print id; print_space(); expression def;
          close_box();
          letdef body
      | exp ->
          print_string ")"; close_box();
          print_space(); sequence exp
      in letdef body;
      print_string ")"; close_box()
  | Clet(id, def, body) ->
      open_hovbox 2;
      print_string "(let"; print_space();
      open_hovbox 2;
      Ident.print id; print_space(); expression def;
      close_box(); print_space();
      sequence body;
      print_string ")"; close_box()
  | Cassign(id, exp) ->
      open_hovbox 2;
      print_string "(assign ";
      open_hovbox 2;
      Ident.print id; print_space(); expression exp;
      close_box();
      print_string ")"; close_box()
  | Ctuple el ->
      open_hovbox 1;
      print_string "[";
      let first = ref true in
      List.iter
        (fun e ->
          if !first then first := false else print_space();
          expression e)
        el;
      print_string "]";
      close_box()
  | Cop(op, el) ->
      open_hovbox 2;
      print_string "("; operation op;
      List.iter (fun e -> print_space(); expression e) el;
      begin match op with
        Capply mty -> print_space(); machtype mty
      | Cextcall(_, mty, _) -> print_space(); machtype mty
      | Cload mty -> print_space(); machtype mty
      | _ -> ()
      end;
      print_string ")";
      close_box()
  | Csequence(e1, e2) ->
      open_hovbox 2;
      print_string "(seq "; print_space();
      sequence e1; print_space();
      sequence e2; print_string ")"; close_box()
  | Cifthenelse(e1, e2, e3) ->
      open_hovbox 2;
      print_string "(if";
      print_space(); expression e1;
      print_space(); expression e2;
      print_space(); expression e3;
      print_string ")"; close_box()
  | Cswitch(e1, index, cases) ->
      open_vbox 0;
      open_hovbox 2;
      print_string "(switch"; print_space(); expression e1; print_space();
      close_box();
      for i = 0 to Array.length cases - 1 do
        print_space();
        open_hovbox 2;
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then begin
            print_string "case "; print_int j; print_string ":"; print_space()
          end
        done;
        sequence cases.(i);
        close_box()
      done;
      close_box()
  | Cloop e ->
      open_hovbox 2;
      print_string "(loop";
      print_space(); sequence e;
      print_string ")"; close_box()
  | Ccatch(e1, e2) ->
      open_hovbox 2;
      print_string "(catch";
      print_space(); sequence e1;
      print_break 1 (-2); print_string "with";
      print_space(); sequence e2;
      print_string ")"; close_box()
  | Cexit ->
      print_string "exit"
  | Ctrywith(e1, id, e2) ->
      open_hovbox 2;
      print_string "(try";
      print_space(); sequence e1;
      print_break 1 (-2); print_string "with "; Ident.print id;
      print_space(); sequence e2;
      print_string ")"; close_box()

and sequence = function
    Csequence(e1, e2) ->
      sequence e1; print_space(); sequence e2
  | e ->
      expression e

let fundecl f =
  open_hovbox 1;
  print_string "(function "; print_string f.fun_name; print_break 1 4;
  open_hovbox 1;
  print_string "(";
  let first = ref true in
  List.iter
    (fun (id, ty) -> 
      if !first then first := false else print_space();
      Ident.print id; print_string ": "; machtype ty)
    f.fun_args;
  print_string ")"; close_box(); print_space();
  open_hovbox 0;
  sequence f.fun_body;
  print_string ")";
  close_box(); close_box(); print_newline()

let data_item = function
    Cdefine_symbol s -> print_string "\""; print_string s; print_string "\":"
  | Cdefine_label l -> print_string "L"; print_int l; print_string ":"
  | Cint8 n -> print_string "byte "; print_int n
  | Cint16 n -> print_string "half "; print_int n
  | Cint n -> print_string "int "; print_int n
  | Cfloat f -> print_string "float "; print_string f
  | Csymbol_address s ->
      print_string "addr \""; print_string s; print_string "\""
  | Clabel_address l -> print_string "addr L"; print_int l
  | Cstring s -> print_string "string \""; print_string s; print_string "\""
  | Cskip n -> print_string "skip "; print_int n
  | Calign n -> print_string "align "; print_int n

let data dl =
  open_hvbox 1;
  print_string "(data";
  List.iter (fun d -> print_space(); data_item d) dl;
  print_string ")"; close_box()

let phrase = function
    Cfunction f -> fundecl f
  | Cdata dl -> data dl
