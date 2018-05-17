(* $Id$ *)

open StdLabels
open Str

let camlbegin = "\\caml"
let camlend = "\\endcaml"
let camlin = {|\\?\1|}
let camlout = {|\\:\1|}
let camlbunderline = "\\<"
let camleunderline = "\\>"

let start newline out s args =
  Format.fprintf out "%s%s" camlbegin s;
  List.iter (Format.fprintf out "{%s}") args;
  if newline then Format.fprintf out "\n"

let stop newline out s =
  Format.fprintf out "%s%s" camlend s;
  if newline then Format.fprintf out "\n"

let code_env ?(newline=true) env out s =
  Format.fprintf out "%a%s\n%a"
    (fun ppf env -> start false ppf env []) env s (stop newline) env

let main = "example"
type example_mode = Toplevel | Verbatim | Signature
let string_of_mode =  function
  | Toplevel -> "toplevel"
  | Verbatim -> "verbatim"
  | Signature -> "signature"

let input_env = "input"
let ok_output ="output"
let error ="error"
let warning ="warn"
let phrase_env = ""


let camllight = ref "TERM=norepeat ocaml"
let verbose = ref true
let linelen = ref 72
let outfile = ref ""
let cut_at_blanks = ref false
let files = ref []
let repo_root = ref ""

let (~!) =
  let memo = ref [] in
  fun key ->
    try List.assq key !memo
    with Not_found ->
      let data = Str.regexp key in
      memo := (key, data) :: !memo;
      data

module Toplevel = struct
  (** Initialize the toplevel loop, redirect stdout and stderr,
      capture warnings and error messages *)

  type output =
    { error:string; (** error message text *)
      warnings:string list; (** warning messages text *)
      values:string; (** toplevel output *)
      stdout:string; (** output printed on the toplevel stdout *)
      underlined: (int * int) list
      (** locations to underline in input phrases *)
    }

  let buffer_fmt () =
    let b = Buffer.create 30 in b, Format.formatter_of_buffer b

  let error_fmt = buffer_fmt ()
  let warning_fmt = buffer_fmt ()

  let out_fmt = buffer_fmt ()

  let flush_fmt (b,fmt) =
    Format.pp_print_flush fmt ();
    let r = Buffer.contents b in
    Buffer.reset b;
    r

  (** Redirect the stdout *)
  let stdout_out, stdout_in = Unix.pipe ~cloexec:true ()
  let () = Unix.dup2 stdout_in Unix.stdout;
    Unix.set_nonblock stdout_out

  let self_error_fmt = Format.formatter_of_out_channel stderr
  let eprintf = Format.eprintf

  let read_stdout =
    let size = 50 in
    let b = Bytes.create size in
    let buffer = Buffer.create 100 in
    let max_retry = 5 in
    let rec loop retry =
      try
        let n = Unix.read stdout_out b 0 size in
        if n = size then
          (Buffer.add_bytes buffer b; loop max_retry)
        else
          Buffer.add_bytes buffer (Bytes.sub b 0 n)
      with Unix.(Unix_error (EAGAIN,_,_) ) ->
        if retry = 0 then () else loop (retry - 1)
    in
    fun () ->
      let () = flush stdout; loop max_retry in
      let r = Buffer.contents buffer in
      Buffer.reset buffer;
      r

  (** Store character intervals directly *)
  let locs = ref []
  let print_loc _ppf (loc : Location.t) =
    let startchar = loc.loc_start.pos_cnum in
    let endchar = loc.loc_end.pos_cnum in
    if startchar >= 0 then
      locs := (startchar, endchar) :: !locs

  (** Capture warnings and keep them in a list *)
  let warnings = ref []
  let print_warning loc _ppf w =
    Location.default_warning_printer loc (snd warning_fmt) w;
    let w = flush_fmt warning_fmt in
    warnings := w :: !warnings

  let fatal ic oc fmt =
    Format.kfprintf
      (fun ppf -> Format.fprintf ppf "@]@."; close_in ic; close_out oc; exit 1)
      self_error_fmt ("@[<hov 2>  Error " ^^ fmt)

  let init () =
    Location.printer := print_loc;
    Location.warning_printer := print_warning;
    Clflags.color := Some Misc.Color.Never;
    Clflags.no_std_include := true;
    Compenv.last_include_dirs := [Filename.concat !repo_root "stdlib"];
    Location.error_reporter :=
      (fun _ e -> Location.default_error_reporter (snd error_fmt) e);
    Compmisc.init_path false;
    try
      Toploop.initialize_toplevel_env ();
      Sys.interactive := false
    with _ ->
      (eprintf "Invalid repo root: %s?%!" !repo_root; exit 2)

  let exec (_,ppf) p =
    try
      ignore @@ Toploop.execute_phrase true ppf p
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      begin try Location.report_exception (snd error_fmt) exn
      with _ ->
        eprintf "Uncaught exception: %s\n%s\n"
          (Printexc.to_string exn)
          (Printexc.raw_backtrace_to_string bt)
      end

  let read_output ()  =
    let ws = !warnings in
    warnings := [];
    let es = flush_fmt error_fmt in
    let s = flush_fmt out_fmt in
    let s = replace_first ~!{|^#\( *\*\)* *|} "" s in
    (* the inner ( *\* )* group is here to clean the starting "*"
       introduced for multiline comments *)
    let underlined = !locs in
    locs := [];
    let msgs = read_stdout () in
    { values = s; warnings=ws; error=es; stdout= msgs; underlined }

  (** exec and ignore all output from the toplevel *)
  let eval b =
    let s = Buffer.contents b in
    let ast = Parse.toplevel_phrase (Lexing.from_string s) in
    exec out_fmt ast;
    ignore (read_output())

end

let () =
  Arg.parse ["-n", Arg.Int (fun n -> linelen := n), "line length";
             "-o", Arg.String (fun s -> outfile := s), "output";
             "-repo-root", Arg.String ((:=) repo_root ), "repo root";
             "-w", Arg.Set cut_at_blanks, "cut at blanks";
             "-v", Arg.Bool (fun b -> verbose := b ), "output result on stderr"
            ]
    (fun s -> files := s :: !files)
    "caml-tex2: ";
  Toplevel.init ()


(** The Output module deals with the analysis and classification
    of the interpreter output and the parsing of status-related options
    or annotations for the caml_example environment *)
module Output = struct

  (** Interpreter output status *)
  type status =
    | Ok
    | Warning of int
    | Error

  type kind =
    | Annotation (** Local annotation: [ [@@expect (*annotation*) ] ]*)
    | Option (** Global environment option:
                 [\begin{caml_example}[option[=value]]
                 ...
                 \end{caml_example}] *)

  (** Pretty printer for status *)
  let pp_status ppf = function
    | Error -> Format.fprintf ppf "error"
    | Ok -> Format.fprintf ppf "ok"
    | Warning n -> Format.fprintf ppf "warning %d" n

  (** Pretty printer for status preceded with an undefined determinant *)
  let pp_a_status ppf = function
    | Error -> Format.fprintf ppf "an error"
    | Ok -> Format.fprintf ppf "an ok"
    | Warning n -> Format.fprintf ppf "a warning %d" n

  (** {1 Related latex environment } *)
  let env = function
    | Error -> error
    | Warning _ -> warning
    | Ok -> ok_output

  (** {1 Exceptions } *)
  exception Parsing_error of kind * string

  type source = { file:string; lines:int * int; phrase:string; output:string }
  type unexpected_report = {source:source; expected:status; got:status}
  exception Unexpected_status of unexpected_report

  let print_source ppf {file; lines = (start, stop); phrase; output} =
    Format.fprintf ppf "%s, lines %d to %d:\n\"\n%s\n\"\n\"\n%s\n\"."
      file start stop phrase output

  let print_unexpected {source; expected; got} =
    if expected = Ok then
      Toplevel.eprintf
        "Error when evaluating a caml_example environment in %a\n\
         Unexpected %a status.\n\
         If %a status was expected, add an [@@expect %a] annotation.\n"
        print_source source
        pp_status got
        pp_a_status got
        pp_status got
    else
      Toplevel.eprintf
        "Error when evaluating a guarded caml_example environment in %a\n\
         Unexpected %a status, %a status was expected.\n\
         If %a status was in fact expected, change the status annotation to \
         [@@expect %a].\n"
        print_source source
        pp_status got
        pp_a_status expected
        pp_a_status got
        pp_status got;
    flush stderr

  let print_parsing_error k s =
    match k with
    | Option ->
        Toplevel.eprintf
          "Unknown caml_example option: [%s].\n\
           Supported options are \"ok\",\"error\", or \"warning=n\" (with n \
           a warning number).\n" s
    | Annotation ->
        Toplevel.eprintf
          "Unknown caml_example phrase annotation: [@@expect %s].\n\
           Supported annotations are [@@expect ok], [@@expect error],\n\
           and [@@expect warning n] (with n a warning number).\n" s

  (** {1 Output analysis} *)
  let catch_error = function
    | "" -> None
    | _ -> Some Error

  let catch_warning =
    function
    | [] -> None
    | s :: _ when string_match ~!{|Warning \([0-9]+\):|} s 0 ->
        Some (Warning (int_of_string @@ matched_group 1 s))
    | _ -> None

  let status ws es =
    match catch_warning ws, catch_error es with
    | Some w, _ -> w
    | None, Some e -> e
    | None, None -> Ok

  (** {1 Parsing caml_example options } *)

  (** Parse [warning=n] options for caml_example options *)
  let parse_warning s =
    if string_match ~!{|warning=\([0-9]+\)|} s 0 then
      Some (Warning (int_of_string @@ matched_group 1 s))
    else
      None

  (** Parse [warning n] annotations *)
  let parse_local_warning s =
    if string_match ~!{|warning \([0-9]+\)|} s 0 then
      Some (Warning (int_of_string @@ matched_group 1 s))
    else
      None

  let parse_error s =
    if s="error" then Some Error else None

  let parse_ok s =
    if s = "ok" then Some Ok else None

  (** Parse the environment-wide expected status output *)
  let expected s =
    match parse_warning s, parse_error s with
    | Some w, _ -> w
    | None, Some e -> e
    | None, None -> raise (Parsing_error (Option,s))

  (** Parse the local (i.e. phrase-wide) expected status output *)
  let local_expected s =
    match parse_local_warning s, parse_error s, parse_ok s with
    | Some w, _, _ -> w
    | None, Some e, _ -> e
    | None, None, Some ok -> ok
    | None, None, None -> raise (Parsing_error (Annotation,s))

end

module Text_transform = struct

  type kind =
    | Underline
    | Ellipsis

  type t = { kind:kind; start:int; stop:int}
  exception Intersection of
      {line:int;
       file:string;
       left: t;
       right: t;
      }

  let pp ppf = function
    | Underline -> Format.fprintf ppf "underline"
    | Ellipsis -> Format.fprintf ppf "ellipsis"

  let underline start stop = { kind = Underline; start; stop}
  let ellipsis start stop = { kind = Ellipsis; start; stop }

  let escape_specials s =
    let s1 = global_replace ~!"\\\\" "\\\\\\\\" s in
    let s2 = global_replace ~!"'" "\\\\textquotesingle\\\\-" s1 in
    let s3 = global_replace ~!"`" "\\\\textasciigrave\\\\-" s2 in
    s3

  let rec apply_transform input (pos,underline_stop,out) t =
    if pos >= String.length input then pos, underline_stop, out
    else match underline_stop with
      | Some stop when stop <= t.start ->
          let f = escape_specials (String.sub input ~pos ~len:(stop - pos)) in
          let out =  {|\>|} :: f :: out in
          apply_transform input (stop,None,out) t
      | _ ->
          let out =
            escape_specials (String.sub input ~pos ~len:(t.start - pos))::out in
          match t.kind with
          | Ellipsis -> t.stop, underline_stop, {|\ldots|} :: out
          | Underline ->
              t.start, Some t.stop, {|\<|} :: out

  (** Check that all ellipsis are strictly nested inside underline transform
      and that otherwise no transform starts before the end of the previous
      transform in a list of transforms *)
  type partition = U of t * t list | E of t
  let check_partition line file l =
    let init = ellipsis 0 0 in
    let rec partition = function
      | [] -> []
      | {kind=Underline; _ } as t :: q -> underline t [] q
      | {kind=Ellipsis; _ } as t :: q -> E t :: partition q
    and underline u n = function
      | [] -> end_underline u n []
      | {kind=Underline; _ } :: _ as q -> end_underline u n q
      | {kind=Ellipsis; _ } as t :: q ->
          if t.stop < u.stop then underline u (t::n) q
          else end_underline u n (t::q)
    and end_underline u n l = U(u,List.rev n) :: partition l in
    let check_elt last t =
      if t.start < last.stop then
        raise (Intersection {line;file; left = last; right = t})
      else
        t in
    let check acc = function
      | E t -> check_elt acc t
      | U(u,n) ->
          let _ = check_elt acc u in
          let _ = List.fold_left ~f:check_elt ~init n in
          u in
    List.fold_left ~f:check ~init (partition l)
    |> ignore

  let apply ts file line s =
    (* remove duplicated transforms that can appear due to
        duplicated parse tree elements. For instance,
        [let f : [%ellipsis] = ()] is transformed to
        [let f: [%ellipsis] = (():[%ellipsis])] with the same location
        for the two ellipses. *)
    let ts = List.sort_uniq compare ts in
    let ts = List.sort (fun x y -> compare x.start y.start) ts in
    check_partition line file ts;
    let last, underline, ls =
      List.fold_left ~f:(apply_transform s) ~init:(0,None,[]) ts in
    let last, ls = match underline with
      | None -> last, ls
      | Some stop ->
          let f = escape_specials (String.sub s ~pos:last ~len:(stop - last)) in
          stop, {|\>|} :: f :: ls in
    let ls =
      let n = String.length s in
      if last = n then ls else
        escape_specials (String.sub s last (n-last)) :: ls in
    String.concat "" (List.rev ls)
end


exception Missing_double_semicolon of string * int

exception Missing_mode of string * int

type incompatibility =
  | Signature_with_visible_answer of string * int
exception Incompatible_options of incompatibility

exception Phrase_parsing of string


module Ellipsis = struct
  (** This module implements the transformation of ellipsis nodes
      to valid ocaml ast, and the recording of the locations of those elided
      nodes.

      An ellipsis is either an [[%ellipsis]] node, or a pair
      of [[%%ellipsis.start]...[%%ellipsis.stop]] nodes.

      The payload of the extension node must be acceptable in the
      context of the node (a pattern payload inside a pattern,
      an expression inside an expression, etc ...).

      Moreover, a default payload is available in all contexts:
      - [[%ellipsis]] <=> [[%ellipsis assert false]] in expression contexts
      - [[%ellipsis]] <=> [[%ellipsis? _ ]] in pattern contexts
      - [[%ellipsis]] <=> [[%ellipsis: _ ] in type expression contexts
      - [[%ellipsis]] <=> [[%ellipsis: (module struct end)]] in module
        expression contexts
      - [[%%ellipsis]] <=> [] in signature or structure



      - [[%ellipsis]] <=> [[%ellipsis: (module struct end)]] in module
        expression contexts
      - [[%ellipsis]] <=> [[%ellipsis object end]] in class expression context
      - [[%ellipsis]] <=> [[%ellipsis object end]] in class type context
      - [[%ellipsis]] <=> [[%ellipsis val ellipsis=unit]]
        in class field context
      - [[%ellipsis]] <=> [[%ellipsis val ellipsis:unit]]
        in class field context

      Note that the last five variants require a somewhat contrived syntax
      to be used:

      {[
        class c = let x = 0 in [%ellipsis class x = let y = 0 in object end]
        class type c =
          [%ellipsis class type x = fun y = 0 in object end]
        class c = object [%%ellipsis object method f = x end] end
        class type c = object
          [%%ellipsis class type x = object method f = x end]
        end
        module type s = [%ellipsis module type x= sig end]
        module X = struct include [%ellipsis (module struct end)]
      ]}
*)

  exception Unmatched_ellipsis of {kind:string; start:int; stop:int}
  (** raised when an [[%%ellipsis.start]] or [[%%ellipsis.stop]] is
      not paired with another ellipsis node *)

  exception Nested_ellipses of {first:int; second:int}
  (** raised by [[%ellipsis.start] [%ellipsis.start]] *)


  open Parsetree

  (** Extension node context *)
  type 'a ctx =
    | Typ : core_type ctx
    | Pat : pattern ctx
    | Expr : expression ctx
    | Str : structure_item list ctx
    | Sig : signature_item list ctx
    | Stri : structure_item ctx
    | Sigi : signature_item ctx
    | Me: module_expr ctx
    (** vv unimplemented vv
        There are no payload that clearly match to these contexts *)
    | Mty: module_type ctx (** module type whatever = $ ?*)
    | Class: class_expr ctx (** class whatever = $ ?*)
    | Class_field: class_field ctx (** object $ end ? *)
    | Class_type: class_type ctx (** class type whatever = $ ?*)
    | Class_type_field: class_type_field ctx
    (** class type whatever = object $ end ?*)

  (** Remove the context type *)
  type actx = Ctx: 'result ctx -> actx

  exception Invalid_ellipsis_contents of {file:string; line:int; ctx: actx}

  module H = Ast_helper
  let name loc s = Location.mkloc (Longident.Lident s) loc
  let assert_false loc =
    H.Exp.mk ~loc @@ Pexp_assert (H.Exp.construct ~loc (name loc "false") None)

  let unit_type loc = H.Typ.constr ~loc (name loc "unit") []

  let class_str loc = H.Cstr.mk (H.Pat.mk ~loc Ppat_any)

  (** lift an extension payload outside of the extension node *)
  let lift file line (type a) (ctx:a ctx) loc extension: a =
    match ctx, extension with
    | Typ, PTyp t-> t
    | Typ, PStr [] -> H.Typ.mk ~loc Ptyp_any
    | Pat, PPat (t,None) -> t
    | Pat, PStr [] -> H.Pat.mk ~loc Ppat_any
    | Expr, PStr [{pstr_desc=Pstr_eval (e,_); _ } ] -> e
    | Expr, PStr [] -> assert_false loc
    | Str, PStr s -> s
    | Sig, PSig s -> s
    | Sig, PStr [] -> []
    | Stri, PStr [s] -> s
    | Sigi, PSig [s] -> s
    | Me, PStr [{pstr_desc=Pstr_eval ({pexp_desc = Pexp_pack me; _}, _ ); _}] ->
        me
    | Me, PStr [] -> H.Mod.structure ~loc []
    | Mty, PStr [{pstr_desc=Pstr_modtype {pmtd_type= Some mty; _ };_}] -> mty
    | Mty, PSig [{psig_desc=Psig_modtype {pmtd_type= Some mty; _ };_}] -> mty
    | Mty, PStr [] -> H.Mty.signature ~loc []
    | Class, PStr[{pstr_desc = Pstr_class [c];_}] ->
        c.pci_expr
    | Class, PStr [] -> H.Cl.structure ~loc (class_str loc [])
    | Class_field, PStr[{pstr_desc = Pstr_eval (exp,_);_}] ->
        begin match exp.pexp_desc with
        | Pexp_object {pcstr_fields = [field]; _ } -> field
        | _ -> raise (Invalid_ellipsis_contents {file;line;ctx=Ctx ctx})
        end
    | Class_field, PStr [] ->
        H.Cf.val_ ~loc
          (Location.mkloc "ellipsis" loc)
          Immutable
          (Cfk_concrete (Fresh,H.Exp.construct ~loc (name loc "()") None))
    | Class_type, PStr[{pstr_desc = Pstr_class_type [c];_}] -> c.pci_expr
    | Class_type, PSig [{psig_desc = Psig_class [c];_}] -> c.pci_expr
    | Class_type, PStr [] ->
        H.Cty.signature ~loc (H.Csig.mk (H.Typ.mk ~loc Ptyp_any) [])
    | Class_type_field, PStr[{pstr_desc = Pstr_class_type [c];_}] ->
        begin match c.pci_expr.pcty_desc with
        | Pcty_signature {pcsig_fields = [field]; _ } -> field
        | _ -> raise (Invalid_ellipsis_contents {file;line;ctx=Ctx ctx})
        end
    | Class_type_field, PStr [] ->
        H.Ctf.val_ ~loc (Location.mkloc "ellipsis" loc)
          Immutable Concrete (unit_type loc)
    | _ -> raise (Invalid_ellipsis_contents {file;line;ctx=Ctx ctx})


  let pp_invalid_ctx ppf (Ctx ctx) = match ctx with
    | Typ -> Format.fprintf ppf "A type payload was expected in this context."
    | Pat ->
        Format.fprintf ppf "A pattern payload was expected in this context."
    | Expr ->
        Format.fprintf ppf "A structure payload with a single toplevel \
                            expression was expected in this context."
    | Str ->
        Format.fprintf ppf "A structure payload was expected in this context."
    | Sig ->
        Format.fprintf ppf "A signature payload was expected in this context."
    | Stri ->
        Format.fprintf ppf
          "A structure item payload was expected in this context."
    | Sigi ->
        Format.fprintf ppf
          "A structure item payload was expected in this context."
    | Me ->
        Format.fprintf ppf
          "A structure payload with a single toplevel expression containing
           a packed module,@ e.g. [%%ellipsis (module F(X))], was expected \
           in this module expression context"
    | Mty ->
        Format.fprintf ppf
          "A structure payload with a single module type declaration,@ \
           [%%ellipsis module type any = sig ... end],@ was expected \
           in this context"
    | Class ->
        Format.fprintf ppf
          "A structure payload with a single class declaration,@ \
           [%%ellipsis class any = object ... end],@ was expected \
           in this class context"
    | Class_type ->
        Format.fprintf ppf
          "A signature payload with a single class declaration,@ \
           [%%ellipsis: class any: int -> object ... end],@ was expected \
           in this class type context"
    | Class_type_field ->
        Format.fprintf ppf
          "A structure payload with a single class type declaration \
           containing only an object structure with a single field,@ \
           [%%ellipsis class type x = object val x: ... end],@ was expected \
           in this class type field context"
    | Class_field ->
        Format.fprintf ppf
          "A structure payload with a single class declaration \
           containing only an object structure with a single field,@ \
           [%%ellipsis class x = object val x= ... end],@ was expected \
           in this class field context"

  open Ast_mapper
  let super = Ast_mapper.default_mapper

  type records =
    { mutable transforms: Text_transform.t list;
      mutable left_mark: (int* int) option }

  let record records name (loc:Location.t) =
    let start = loc.loc_start.Lexing.pos_cnum in
    let stop = loc.loc_end.Lexing.pos_cnum in
    let ellipsis = Text_transform.ellipsis in
    let check_nested () = match records.left_mark with
        | Some (first,_) -> raise (Nested_ellipses {first; second=start})
        | None -> () in
      match name with
      | "ellipsis" ->
          check_nested ();
          records.transforms <- ellipsis start stop :: records.transforms
      | "ellipsis.start" ->
          check_nested ();
          records.left_mark <- Some (start, stop)
      | "ellipsis.stop" ->
          begin match records.left_mark with
          | None -> raise (Unmatched_ellipsis {kind="right"; start; stop})
          | Some (start', stop' ) ->
              records.transforms <-
                (* ellipsis.start-stop are not always ordered in the
                   location increasing order *)
                ellipsis (min start start') (max stop stop')
                :: records.transforms;
              records.left_mark <- None
          end
      | _ -> ()


  let dispatch file line m outer_loc super ty (x:_ Location.loc * _ )=
    match x with
    | {txt = "ellipsis"|"ellipsis.start"|"ellipsis.stop" as n; loc }, payload ->
        record m n outer_loc; lift file line ty loc payload
    | _ -> super

  (** the ast mapper itself *)
  let map file line m =
    let dispatch x = dispatch file line m x in
    let typ this x = match x.ptyp_desc with
      | Ptyp_extension ext -> super.typ this @@ dispatch x.ptyp_loc x Typ ext
      | _ -> super.typ this x in
    let pat this x = match x.ppat_desc with
      | Ppat_extension ext -> super.pat this @@ dispatch x.ppat_loc x Pat ext
      | _ -> super.pat this x in
    let class_expr this x = match x.pcl_desc with
      | Pcl_extension ext ->
          super.class_expr this @@ dispatch x.pcl_loc x Class ext
      | _ -> super.class_expr this x in
    let class_field this x = match x.pcf_desc with
      | Pcf_extension ext ->
          super.class_field this @@  dispatch x.pcf_loc x Class_field ext
      | _ -> super.class_field this x in
    let class_type this x = match x.pcty_desc with
      | Pcty_extension ext ->
          super.class_type this @@ dispatch x.pcty_loc x Class_type ext
      | _ -> super.class_type this x in
    let class_type_field this x = match x.pctf_desc with
      | Pctf_extension ext ->
          super.class_type_field this @@
          dispatch x.pctf_loc x Class_type_field ext
      | _ -> super.class_type_field this x in
    let expr this x = match x.pexp_desc with
      | Pexp_extension ext -> super.expr this @@ dispatch x.pexp_loc x Expr ext
      | _ -> super.expr this x in
    let structure_item this x = match x.pstr_desc with
      | Pstr_extension (ext,_) -> dispatch x.pstr_loc [x] Str ext
      | _ -> [super.structure_item this x] in
    let signature_item this x = match x.psig_desc with
      | Psig_extension (ext,_) -> dispatch x.psig_loc [x] Sig ext
      | _ -> [super.signature_item this x] in
    let module_type this x = match x.pmty_desc with
      | Pmty_extension ext ->
          super.module_type this @@ dispatch x.pmty_loc x Mty ext
      | _ -> super.module_type this x in
    let module_expr this x = match x.pmod_desc with
      | Pmod_extension ext ->
          super.module_expr this @@ dispatch x.pmod_loc x Me ext
      | _ -> super.module_expr this x in
    let structure this x =
      List.rev @@ List.fold_left
        ~f:(fun l x -> structure_item this x @ l)
        ~init:[] x in
    let signature this x =
      List.rev @@ List.fold_left
        ~f:(fun l x -> signature_item this x @ l)
        ~init:[] x in
    {super with typ;pat;expr;structure; signature;
                module_type; module_expr;
                class_expr; class_field; class_type;class_type_field
    }

  let run file line f ast =
    let m = { transforms = []; left_mark = None } in
    let () = f (map file line m) ast in
    m.transforms

  let transform_and_run fname line mode ppf s =
    let lex = Lexing.from_string s in
    Location.init lex fname;
    Location.input_name := fname;
    Location.input_lexbuf := Some lex;
    let pstr it x =
      Toplevel.exec ppf @@ Ptop_def (it.structure it x) in
    let psig it x =
      let s = it.signature it x in
      let name = Location.mknoloc "wrap" in
      let str =
        Ast_helper.[Str.modtype @@ Mtd.mk ~typ:(Mty.signature s) name] in
      Toplevel.exec ppf (Ptop_def str) in
    try
      match mode with
      | Verbatim -> run fname line pstr (Parse.implementation lex)
      | Signature -> run fname line psig (Parse.interface lex)
      | Toplevel ->
          match Parse.toplevel_phrase lex with
          | Ptop_dir _ -> []
          | Ptop_def str -> run fname line pstr str
    with Syntaxerr.Error _ -> raise (Phrase_parsing s)

end

let process_file file =
  prerr_endline ("Processing " ^ file);
  let ic = try open_in file with _ -> failwith "Cannot read input file" in
  let phrase_start = ref 1 and phrase_stop = ref 1 in
  let incr_phrase_start () =
    incr phrase_start;
    phrase_stop := !phrase_start in
  let oc =
    try if !outfile = "-" then
      stdout
    else if !outfile = "" then
      open_out (replace_first ~!"\\.tex$" "" file ^ ".ml.tex")
    else
      open_out_gen [Open_wronly; Open_creat; Open_append; Open_text]
        0x666 !outfile
    with _ -> failwith "Cannot open output file" in
  let tex_fmt = Format.formatter_of_out_channel oc in
  let fatal x = Toplevel.fatal ic oc x in
  let re_spaces = "[ \t]*" in
  let re_start = ~!(
      {|\\begin{caml_example\(\*?\)}|} ^ re_spaces
      ^ {|\({toplevel}\|{verbatim}\|{signature}\)?|} ^ re_spaces
      ^ {|\(\[\(.*\)\]\)?|} ^ re_spaces
      ^ "$"
    ) in
  try while true do
    let input = ref (input_line ic) in
    incr_phrase_start();
    if string_match re_start !input 0
    then begin
      let omit_answer = matched_group 1 !input = "*" in
      let mode =
        match matched_group 2 !input with
        | exception Not_found -> raise (Missing_mode(file, !phrase_stop))
        | "{toplevel}" -> Toplevel
        | "{verbatim}" -> Verbatim
        | "{signature}" -> Signature
        | _ -> assert false in
      if mode = Signature && not omit_answer then raise
          (Incompatible_options(
              Signature_with_visible_answer(file,!phrase_stop))
          );
      let explicit_stop = match mode with
        | Verbatim | Signature -> false
        | Toplevel -> true in
      let global_expected = try Output.expected @@ matched_group 4 !input
        with Not_found -> Output.Ok in
      start true tex_fmt main [string_of_mode mode];
      let first = ref true in
      let read_phrase () =
        let phrase = Buffer.create 256 in
        let rec read () =
          let input = incr phrase_stop; input_line ic in
          let implicit_stop =
            if string_match ~!"\\\\end{caml_example\\*?}[ \t]*$"
                input 0
            then
              begin
                if !phrase_stop = 1 + !phrase_start then
                  raise End_of_file
                else if explicit_stop then
                  raise @@ Missing_double_semicolon (file,!phrase_stop)
                else
                  true
              end
            else false in
          if Buffer.length phrase > 0 then Buffer.add_char phrase '\n';
          let stop =
            implicit_stop ||
            ( not (mode = Signature)
              && string_match ~!"\\(.*\\)[ \t]*;;[ \t]*$" input 0 )
          in
          if not stop then (
            Buffer.add_string phrase input; read ()
          )
          else begin
            decr phrase_stop;
            let last_input = if implicit_stop then "" else matched_group 1 input in
            let expected =
              if string_match ~!{|\(.*\)\[@@expect \(.*\)\]|} last_input 0 then
                ( Buffer.add_string phrase (matched_group 1 last_input);
                  Output.local_expected @@ matched_group 2 last_input )
              else
                (Buffer.add_string phrase last_input; global_expected)
            in
            if not implicit_stop then Buffer.add_string phrase ";;";
            implicit_stop, Buffer.contents phrase, expected
          end in
        read ()
      in
      try while true do
        let implicit_stop, phrase, expected = read_phrase () in
        let ellipses =
          Ellipsis.transform_and_run file !phrase_start mode
            Toplevel.out_fmt phrase in
        let out = Toplevel.read_output () in
        let msgs = String.concat "" (out.warnings @ [out.error]) in
        let raw_output = msgs ^ out.stdout ^ out.values in
        let status = Output.status out.warnings out.error in
        if status <> expected then (
          let source = Output.{
              file;
              lines = (!phrase_start, !phrase_stop);
              phrase;
              output = raw_output
            } in
          raise (Output.Unexpected_status
                   {Output.got=status; expected; source} ) )
        else ( incr phrase_stop; phrase_start := !phrase_stop );
        let phrase =
          let underline =
            List.map (fun (x,y) -> Text_transform.underline x y)
              out.underlined in
          Text_transform.apply (underline @ ellipses)
            file !phrase_stop phrase in
        (* Special characters may also appear in output strings -Didier *)
        let output = Text_transform.escape_specials raw_output in
        let phrase = global_replace ~!{|^\(.\)|} camlin phrase
        and output = global_replace ~!{|^\(.\)|} camlout output
        and msgs =
          if String.length msgs > 0 then
            global_replace ~!{|^\(.\)|} camlout output
          else msgs
        in
        start false tex_fmt phrase_env [];
        code_env ~newline:omit_answer input_env tex_fmt phrase;
        if not omit_answer || String.length msgs > 0  then
          code_env ~newline:false (Output.env status) tex_fmt
            (if omit_answer then msgs else output);
        stop true tex_fmt phrase_env;
        flush oc;
        first := false;
        if implicit_stop then raise End_of_file
      done
      with End_of_file -> phrase_start:= !phrase_stop; stop true tex_fmt main
    end
    else if string_match ~!"\\\\begin{caml_eval}[ \t]*$" !input 0
    then begin
      let eval_buffer = Buffer.create 256 in
      while input := input_line ic;
        not (string_match ~!"\\\\end{caml_eval}[ \t]*$" !input 0)
      do
        Buffer.add_string eval_buffer !input;
        Buffer.add_char eval_buffer '\n';
        if string_match ~!".*;;[ \t]*$" !input 0 then begin
          Buffer.add_string eval_buffer !input;
          Buffer.add_char eval_buffer '\n';
          Toplevel.eval eval_buffer;
          Buffer.reset eval_buffer
        end
      done;
      if Buffer.length eval_buffer > 0 then
        ( Buffer.add_string eval_buffer ";;\n"; Toplevel.eval eval_buffer )
    end else begin
      Format.fprintf tex_fmt "%s\n" !input;
      Format.pp_print_flush tex_fmt ()
    end
  done with
  | End_of_file -> close_in ic; close_out oc
  | Output.Unexpected_status r ->
          ( Output.print_unexpected r; close_in ic; close_out oc; exit 1 )
  | Output.Parsing_error (k,s) ->
      ( Output.print_parsing_error k s;
        close_in ic; close_out oc; exit 1 )
  | Phrase_parsing s -> fatal "when parsing the following phrase:@ %s" s
  | Missing_double_semicolon (file, line_number) ->
      fatal
        "when evaluating a caml_example environment in %s:@;\
         missing \";;\" at line %d@]@." file (line_number-2)
  | Missing_mode (file, line_number) ->
      fatal "when parsing a caml_example environment in %s:@;\
             missing mode argument at line %d,@ \
             available modes {toplevel,verbatim}"
          file (line_number-2)
  | Incompatible_options Signature_with_visible_answer (file, line_number) ->
      fatal
          "when parsing a caml_example environment in@ \
           %s, line %d:@,\
           the signature mode is only compatible with \"caml_example*\"@ \
           Hint: did you forget to add \"*\"?"
          file (line_number-2);
  | Text_transform.Intersection {line;file;left;right} ->
      fatal
        "when evaluating a caml_example environment in %s, line %d:@ \
         Textual transforms must be well-separated.@ The \"%a\" transform \
         spanned the interval %d-%d,@ \
         intersecting with another \"%a\" transform @ \
         on the %d-%d interval.@ \
         Hind: did you try to elide a code fragment which raised a warning?"
        file (line-2)
        Text_transform.pp left.kind left.start left.stop
        Text_transform.pp right.kind right.start right.stop
  | Ellipsis.Unmatched_ellipsis {kind;start;stop} ->
      fatal "when evaluating a caml_example environment,@ \
             the %s mark at position %d-%d was unmatched"
        kind start stop
  | Ellipsis.Nested_ellipses {first;second} ->
      fatal "when evaluating a caml_example environment,@ \
             there were two nested ellipsis attribute.@ The first one \
             started at position %d,@ the second one at %d"
        first second
  | Ellipsis.Invalid_ellipsis_contents {file;line;ctx} ->
      fatal
      "when evaluating a caml_example environment in %s, line %d:@ %a"
      file (line-2) Ellipsis.pp_invalid_ctx ctx

let _ =
  if !outfile <> "-" && !outfile <> "" then begin
    try close_out (open_out !outfile)
    with _ -> failwith "Cannot open output file"
  end;
  List.iter process_file (List.rev !files)
