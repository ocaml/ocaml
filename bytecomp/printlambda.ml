open Format
open Asttypes
open Lambda


let rec structured_constant = function
    Const_base(Const_int n) -> print_int n
  | Const_base(Const_char c) ->
      print_string "'"; print_string(Char.escaped c); print_string "'"
  | Const_base(Const_string s) ->
      print_string "\""; print_string(String.escaped s); print_string "\""
  | Const_base(Const_float s) ->
      print_string s
  | Const_block(tag, []) ->
      print_string "["; print_int tag; print_string "]"
  | Const_block(tag, sc1::scl) ->
      open_hovbox 1;
      print_string "["; print_int tag; print_string ":";
      print_space();
      open_hovbox 0;
      structured_constant sc1;
      List.iter (fun sc -> print_space(); structured_constant sc) scl;
      close_box();
      print_string "]";
      close_box()

let primitive = function
    Pidentity -> print_string "id"
  | Pgetglobal id -> print_string "global "; Ident.print id
  | Psetglobal id -> print_string "setglobal "; Ident.print id
  | Pmakeblock sz -> print_string "makeblock "; print_int sz
  | Ptagof -> print_string "tag"
  | Pfield n -> print_string "field "; print_int n
  | Psetfield n -> print_string "setfield "; print_int n
  | Pccall(name, arity) -> print_string name
  | Pupdate -> print_string "update"
  | Praise -> print_string "raise"
  | Psequand -> print_string "&&"
  | Psequor -> print_string "||"
  | Pnot -> print_string "not"
  | Pnegint -> print_string "~"
  | Paddint -> print_string "+"
  | Psubint -> print_string "-"
  | Pmulint -> print_string "*"
  | Pdivint -> print_string "/"
  | Pmodint -> print_string "mod"
  | Pandint -> print_string "and"
  | Porint -> print_string "or"
  | Pxorint -> print_string "xor"
  | Plslint -> print_string "lsl"
  | Plsrint -> print_string "lsr"
  | Pasrint -> print_string "asr"
  | Pcomp(Ceq) -> print_string "=="
  | Pcomp(Cneq) -> print_string "!="
  | Pcomp(Clt) -> print_string "<"
  | Pcomp(Cle) -> print_string "<="
  | Pcomp(Cgt) -> print_string ">"
  | Pcomp(Cge) -> print_string ">="
  | Poffsetint n -> print_int n; print_string "+"
  | Poffsetref n -> print_int n; print_string "+:="
  | Pgetstringchar -> print_string "string.get"
  | Psetstringchar -> print_string "string.set"
  | Pvectlength -> print_string "array.length"
  | Pgetvectitem -> print_string "array.get"
  | Psetvectitem -> print_string "array.set"

let rec lambda = function
    Lvar id ->
      Ident.print id
  | Lconst cst ->
      structured_constant cst
  | Lapply(lfun, largs) ->
      open_hovbox 2;
      print_string "(apply"; print_space();
      lambda lfun;
      List.iter (fun l -> print_space(); lambda l) largs;
      print_string ")";
      close_box()
  | Lfunction(param, body) ->
      open_hovbox 2;
      print_string "(function"; print_space(); Ident.print param;
      print_space(); lambda body; print_string ")"; close_box()
  | Llet(id, arg, body) ->
      open_hovbox 2;
      print_string "(let"; print_space();
      open_hvbox 1;
      print_string "(";
      open_hovbox 2; Ident.print id; print_space(); lambda arg; close_box();
      letbody body;
      print_string ")";
      close_box()
  | Lletrec(id_arg_list, body) ->
      open_hovbox 2;
      print_string "(letrec"; print_space();
      open_hvbox 1;
      print_string "(";
      let spc = ref false in
      List.iter
        (fun (id, l, sz) ->
          if !spc then print_space() else spc := true;
          Ident.print id; print_string " "; lambda l)
        id_arg_list;
      close_box();
      print_string ")";
      print_space(); lambda body;
      print_string ")"; close_box()
  | Lprim(prim, largs) ->
      open_hovbox 2;
      print_string "("; primitive prim;
      List.iter (fun l -> print_space(); lambda l) largs;
      print_string ")";
      close_box()
  | Lswitch(larg, lo, hi, cases) ->
      open_hovbox 1;
      print_string "(switch "; print_int lo; print_string "/";
      print_int hi; print_space();
      lambda larg; print_space();
      open_vbox 0;
      let spc = ref false in
      List.iter
        (fun (n, l) ->
          open_hvbox 1;
          print_string "case "; print_int n; print_string ":"; print_space();
          lambda l;
          close_box();
          if !spc then print_space() else spc := true)
        cases;
      print_string ")"; close_box(); close_box()
  | Lstaticfail ->
      print_string "exit"
  | Lcatch(lbody, lhandler) ->
      open_hovbox 2;
      print_string "(catch"; print_space();
      lambda lbody; print_break(1, -1);
      print_string "with"; print_space(); lambda lhandler;
      print_string ")";
      close_box()
  | Ltrywith(lbody, param, lhandler) ->
      open_hovbox 2;
      print_string "(try"; print_space();
      lambda lbody; print_break(1, -1);
      print_string "with "; Ident.print param; print_space();
      lambda lhandler;
      print_string ")";
      close_box()
  | Lifthenelse(lcond, lif, lelse) ->
      open_hovbox 2;
      print_string "(if"; print_space();
      lambda lcond; print_space();
      lambda lif; print_space();
      lambda lelse; print_string ")";
      close_box()
  | Lsequence(l1, l2) ->
      open_hovbox 2;
      print_string "(seq"; print_space();
      lambda l1; print_space(); sequence l2; print_string ")";
      close_box()
  | Lwhile(lcond, lbody) ->
      open_hovbox 2;
      print_string "(while"; print_space();
      lambda lcond; print_space();
      lambda lbody; print_string ")";
      close_box()
  | Lfor(param, lo, hi, dir, body) ->
      open_hovbox 2;
      print_string "(for "; Ident.print param; print_space();
      lambda lo; print_space();
      print_string(match dir with Upto -> "to" | Downto -> "downto");
      print_space();
      lambda hi; print_space();
      lambda body; print_string ")";
      close_box()
  | Lshared(l, lbl) ->
      lambda l

and sequence = function
    Lsequence(l1, l2) ->
      sequence l1; print_space(); sequence l2
  | l ->
      lambda l

and letbody = function
    Llet(id, arg, body) ->
      print_space();
      open_hovbox 2; Ident.print id; print_space(); lambda arg;
      close_box();
      letbody body
  | Lshared(l, lbl) ->
      letbody l
  | l ->
      print_string ")";
      close_box();
      print_space();
      lambda l

