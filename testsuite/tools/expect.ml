(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Execute a list of phrases from a .ml file and compare the result to the
   expected output, written inside [%%expect ...] nodes. At the end, create
   a .corrected file containing the corrected expectations. The test is
   successful if there is no differences between the two files.

   An [%%expect] node always contains both the expected outcome with and
   without -principal or -rectypes. When they differ the expectation
   is written as follows:

   {[
     [%%expect {|
     output without flags
     |}, Principal{|
     output with -principal
     |}]
   ]}

   {[
     [%%expect {|
     output without flags
     |}, Rectypesl{|
     output with -rectypes
     |}]
   ]}

   In case addional flags are used, for example when manually enabled,
   any flag combinations are combined with a dot (".") as separator.

   {[
     [%%expect {|
     output without flags
     |}, Principal.Rectypes{|
     output with -principal and -rectypes
     |}]
   ]}

   If multiple such combinations result in the same output, then they
   are displayed as a tuple.

   {[
     [%%expect {|
     output without flags
     |}, (Principal, Rectypes){|
     output with -principal or -rectypes
     |}
   ]}
*)

[@@@ocaml.warning "-40"]

open StdLabels

(* representation of: {tag|str|tag} *)
type string_constant =
  { str : string
  ; tag : string
  }

let empty_str = { str = ""; tag = "" }

module Clflag = struct
  type t =
    | Principal
    | Rectypes
    | Classic

  let to_string = function
    | Principal -> "Principal"
    | Rectypes -> "Rectypes"
    | Classic -> "Classic"

  module Set = struct
    module T = Set.Make(struct
        type nonrec t = t
        let compare = compare
      end)
    include T

    module Map = Map.Make(T)

    let original = ref empty

    let get_current () =
      List.fold_left ~f:union
        [ if !Clflags.principal then singleton Principal else empty
        ; if !Clflags.recursive_types then singleton Rectypes else empty
        ; if !Clflags.classic then singleton Classic else empty
        ]
        ~init:empty

    let set_current t =
      Clflags.principal := mem Principal t;
      Clflags.recursive_types := mem Rectypes t;
      Clflags.classic := mem Classic t;
      ()

    let to_string c =
      fold (fun cl acc ->
          (if acc = "" then "" else acc ^ ".") ^ to_string cl
        ) c ""

    let of_longident ~loc lid =
      List.fold_left
        ~f:(fun acc s ->
            match s with
            | "Principal" -> add Principal acc
            | "Rectypes" -> add Rectypes acc
            | "Classic" -> add Classic acc
            | other -> Location.raise_errorf ~loc "unknown flag: %s" other)
        ~init:empty (Longident.flatten lid)
  end
end


type expectation =
  { extid_loc   : Location.t (* Location of "expect" in "[%%expect ...]" *)
  ; payload_loc : Location.t (* Location of the whole payload *)
  ; text        : string_constant Clflag.Set.Map.t
  }

let expectation_equal a b =
  a.extid_loc = b.extid_loc &&
  a.payload_loc = b.payload_loc &&
  Clflag.Set.Map.equal (fun a b -> a.str = b.str && a.tag = b.tag) a.text b.text

(* A list of phrases with the expected toplevel output *)
type chunk =
  { phrases     : Parsetree.toplevel_phrase list
  ; expectation : expectation
  }

module Corrected = struct
  type 'a t =
    { original : 'a
    ; corrected : 'a
    }
end

module Correction = struct
  type 'a t =
    { corrected_expectations : 'a list
    ; trailing_output        : string_constant Clflag.Set.Map.t
    }

  module LocationMap = Map.Make(struct
      include Location

      let compare = compare
    end)

  (* merge expectations, filtering out any where the merged expectation
     equals the uncorrected original *)
  let merge clist =
    let merge_text ?(loc = Location.none) a b =
      Clflag.Set.Map.merge
        (fun key text1 text2 ->
           match text1, text2 with
           | None, None -> None
           | (Some _ as text, None)
           | None, (Some _ as text) -> text
           | Some text1 as text, Some text2 when text1 = text2 ->
               text
           | _ -> Location.raise_errorf
                    ~loc
                    "conflicting outputs for %s" (Clflag.Set.to_string key)
        ) a b
    in
    let corrected_expectations, trailing_output =
      List.fold_left
        ~f:(fun (cmap, tmap) { corrected_expectations; trailing_output } ->
            List.fold_left
              ~f:(fun acc { Corrected.original; corrected } ->
                  LocationMap.update
                    original.extid_loc
                    (function
                      | None ->
                          Some { Corrected.original; corrected }
                      | Some { Corrected.original; corrected = corrected' } ->
                          let text =
                            merge_text
                              ~loc:original.extid_loc
                              corrected.text
                              corrected'.text
                          in
                          Some
                            { Corrected.original
                            ; corrected =
                                { original with
                                  text
                                }
                            }
                    )
                    acc
                )
              ~init:cmap
              corrected_expectations
          , merge_text
              tmap
              trailing_output
        )
      ~init:(LocationMap.empty, Clflag.Set.Map.empty)
      clist
    in
    { corrected_expectations =
        LocationMap.to_list corrected_expectations
        |> List.filter_map
             ~f:(fun (_, { Corrected.original; corrected }) ->
              if expectation_equal original corrected
              then None
              else Some corrected
            )
    ; trailing_output
    }

end


let match_expect_extension (ext : Parsetree.extension) =
  match ext with
  | ({Asttypes.txt="expect"|"ocaml.expect"; loc = extid_loc}, payload) ->
    let invalid_payload ?(loc = extid_loc) msg =
      Location.raise_errorf ~loc
        "invalid [%%%%expect payload] (%s)" msg
    in
    let string_constant (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_constant {pconst_desc = Pconst_string (str, _, Some tag); _} ->
        { str; tag }
      | _ -> invalid_payload ~loc:e.pexp_loc "not a string"
    in
    let expectation =
      match payload with
      | PStr [{ pstr_desc = Pstr_eval (e, []) }] ->
        let text =
          match e.pexp_desc with
          | Pexp_tuple
              ((None, normal)
               :: rest) ->
              List.fold_left
                ~f:(fun acc -> function
                      None
                    , { Parsetree.
                        pexp_desc = Pexp_construct
                            ({ txt = clflags_s; _}, Some b) }
                    | None,
                      { Parsetree.
                        pexp_desc = Pexp_apply
                            ({ pexp_desc = Pexp_construct
                                ({ txt = clflags_s; _}, None) }
                            , [ Nolabel, b ]) }
                      ->
                        Clflag.Set.Map.add
                          (Clflag.Set.of_longident ~loc:b.pexp_loc clflags_s)
                          (string_constant b)
                          acc
                    | None,
                      { Parsetree.
                        pexp_desc = Pexp_apply
                            ({ pexp_desc = Pexp_tuple clflags_tuple; _ }
                            , [ Nolabel, b ]) }
                      ->
                        let str = string_constant b in
                        List.fold_left
                          ~f:(fun acc ->
                              function
                              | None,
                                { Parsetree.
                                  pexp_desc = Pexp_construct
                                      ({ txt = cl; _}, None) } ->
                                  Clflag.Set.Map.add
                                    (Clflag.Set.of_longident ~loc:b.pexp_loc cl)
                                    str
                                    acc
                              | _ ->
                                  invalid_payload
                                    ~loc:b.pexp_loc
                                    "expected Constructor"
                            )
                          ~init:acc clflags_tuple
                    | _, pe ->
                        invalid_payload
                          ~loc:pe.Parsetree.pexp_loc
                          "expected Constructor{|string|}"
                  )
                ~init:(Clflag.Set.Map.singleton
                         Clflag.Set.empty (string_constant normal))
                rest
          | _ ->
              let s = string_constant e in
              Clflag.Set.Map.singleton Clflag.Set.empty s
        in
        { extid_loc
        ; payload_loc = e.pexp_loc
        ; text
        }
      | PStr [] ->
        let s = { tag = ""; str = "" } in
        { extid_loc
        ; payload_loc  = { extid_loc with loc_start = extid_loc.loc_end }
        ; text = Clflag.Set.Map.singleton Clflag.Set.empty s
        }
      | _ -> invalid_payload "not an expectation"
    in
    Some expectation
  | _ ->
    None

(* Split a list of phrases from a .ml file *)
let split_chunks phrases =
  let rec loop (phrases : Parsetree.toplevel_phrase list) code_acc acc =
    match phrases with
    | [] ->
      if code_acc = [] then
        (List.rev acc, None)
      else
        (List.rev acc, Some (List.rev code_acc))
    | phrase :: phrases ->
      match phrase with
      | Ptop_def [] -> loop phrases code_acc acc
      | Ptop_def [{pstr_desc = Pstr_extension(ext, [])}] -> begin
          match match_expect_extension ext with
          | None -> loop phrases (phrase :: code_acc) acc
          | Some expectation ->
            let chunk =
              { phrases     = List.rev code_acc
              ; expectation
              }
            in
            loop phrases [] (chunk :: acc)
        end
      | _ -> loop phrases (phrase :: code_acc) acc
  in
  loop phrases [] []

module Compiler_messages = struct
  let capture ppf ~f =
    Misc.protect_refs
      [ R (Location.formatter_for_warnings, ppf) ]
      f
end

let collect_formatters buf pps ~f =
  let ppb = Format.formatter_of_buffer buf in
  let out_functions = Format.pp_get_formatter_out_functions ppb () in

  List.iter ~f:(fun pp -> Format.pp_print_flush pp ()) pps;
  let save =
    List.map ~f:(fun pp -> Format.pp_get_formatter_out_functions pp ()) pps
  in
  let restore () =
    List.iter2
      ~f:(fun pp out_functions ->
         Format.pp_print_flush pp ();
         Format.pp_set_formatter_out_functions pp out_functions)
      pps save
  in
  List.iter
    ~f:(fun pp -> Format.pp_set_formatter_out_functions pp out_functions)
    pps;
  match f () with
  | x             -> restore (); x
  | exception exn -> restore (); raise exn

(* Invariant: ppf = Format.formatter_of_buffer buf *)
let capture_everything buf ppf ~f =
  collect_formatters buf [Format.std_formatter; Format.err_formatter]
                     ~f:(fun () -> Compiler_messages.capture ppf ~f)

let exec_phrase ppf phrase =
  Location.reset ();
  if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
  if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
  Toploop.execute_phrase true ppf phrase

let parse_contents ~fname contents =
  let lexbuf = Lexing.from_string contents in
  Location.init lexbuf fname;
  Location.input_name := fname;
  Location.input_lexbuf := Some lexbuf;
  Parse.use_file lexbuf

let eval_expectation expectation ~output =
  let s =
    try
      Clflag.Set.Map.find (Clflag.Set.get_current ()) expectation.text
    with
    | Not_found ->
        try
          Clflag.Set.Map.find Clflag.Set.empty expectation.text
        with
        | Not_found -> empty_str
  in
  let s = { s with str = output } in
  { Corrected.original = expectation
  ; corrected = { expectation with
                  text =
                    Clflag.Set.Map.singleton
                      (Clflag.Set.get_current ()) s
                }
  }

let shift_lines delta phrases =
  let position (pos : Lexing.position) =
    { pos with pos_lnum = pos.pos_lnum + delta }
  in
  let location _this (loc : Location.t) =
    { loc with
      loc_start = position loc.loc_start
    ; loc_end   = position loc.loc_end
    }
  in
  let mapper = { Ast_mapper.default_mapper with location } in
  List.map phrases ~f:(function
    | Parsetree.Ptop_dir _ as p -> p
    | Parsetree.Ptop_def st ->
      Parsetree.Ptop_def (mapper.structure mapper st))

let rec min_line_number : Parsetree.toplevel_phrase list -> int option =
function
  | [] -> None
  | (Ptop_dir _  | Ptop_def []) :: l -> min_line_number l
  | Ptop_def (st :: _) :: _ -> Some st.pstr_loc.loc_start.pos_lnum


let visible_inline_code () =
  let open Misc.Style in
  let default = get_styles () in
  let inline_code = { ansi = []; text_open = {|"|}; text_close={|"|} } in
  set_styles { default with inline_code }

let eval_expect_file _fname ~file_contents =
  Warnings.reset_fatal ();
  let chunks, trailing_code =
    parse_contents ~fname:"" file_contents |> split_chunks
  in
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let () =
    visible_inline_code ();
    Misc.Style.set_tag_handling ppf in
  let exec_phrases phrases =
    let phrases =
      match min_line_number phrases with
      | None -> phrases
      | Some lnum -> shift_lines (1 - lnum) phrases
    in
    (* For formatting purposes *)
    Buffer.add_char buf '\n';
    let skipped_phrases =
      List.fold_left phrases ~init:None ~f:(fun acc phrase ->
          match (phrase : Parsetree.toplevel_phrase) with
          | Ptop_def [] -> acc
          | _ ->
          match acc with
          | Some i -> Some (i + 1)
          | None ->
              let snap = Btype.snapshot () in
              try
                if exec_phrase ppf phrase
                then acc
                else Some 0
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                begin try Location.report_exception ppf exn
                with _ ->
                  Format.fprintf ppf "Uncaught exception: %s\n%s\n"
                    (Printexc.to_string exn)
                    (Printexc.raw_backtrace_to_string bt)
                end;
                Btype.backtrack snap;
                Some 0
      )
    in
    Format.pp_print_flush ppf ();
    let len = Buffer.length buf in
    if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
      (* For formatting purposes *)
      Buffer.add_char buf '\n';
    begin match skipped_phrases with
    | None | Some 0 -> ()
    | Some i ->
        Format.fprintf ppf
          "Unexecuted phrases: %i phrases did not execute due to an error\n" i
    end;
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    Buffer.clear buf;
    Misc.delete_eol_spaces s
  in
  let corrected_expectations =
    capture_everything buf ppf ~f:(fun () ->
        List.fold_left chunks ~init:[] ~f:(fun acc chunk ->
            let output = exec_phrases chunk.phrases in
            eval_expectation chunk.expectation ~output :: acc)
        |> List.rev)
  in
  let trailing_output =
    match trailing_code with
    | None -> ""
    | Some phrases ->
      capture_everything buf ppf ~f:(fun () -> exec_phrases phrases)
  in
  let trailing_output =
    Clflag.Set.Map.singleton (Clflag.Set.get_current ())
      { str = trailing_output; tag = "" }
  in
  { Correction.corrected_expectations; trailing_output }

let output_slice oc s a b =
  output_string oc (String.sub s ~pos:a ~len:(b - a))

module String_map = Map.Make(String)

let output_corrected oc ~file_contents (correction : _ Correction.t) =
  let output_body oc { str; tag } =
    Printf.fprintf oc "{%s|%s|%s}" tag str tag
  in
  let output_for_all_clflags ?(output_empty=true) map =
    let normal =
      Clflag.Set.Map.find_opt Clflag.Set.empty map
      |> Option.value ~default:empty_str
    in
    let string_to_flags_and_tag =
      Clflag.Set.Map.fold
        (fun key body acc ->
           if body = normal then acc
           else String_map.add_to_list body.str (key, body.tag) acc
        )
        map
        String_map.empty
    in
    let ordered_by_lowest_flag =
      String_map.fold
        (fun str (clflagss_and_tag) acc ->
           let clflagss =
             List.sort_uniq ~cmp:Clflag.Set.compare
               (List.map ~f:fst clflagss_and_tag)
           in
           let tag = snd (List.hd clflagss_and_tag) in
           let low_flag = List.hd clflagss in
           Clflag.Set.Map.add_to_list low_flag (clflagss, { str; tag }) acc
        )
        string_to_flags_and_tag
        Clflag.Set.Map.empty
    in
    (* don't output empty trailing output *)
    if (not output_empty) && Clflag.Set.Map.is_empty ordered_by_lowest_flag
       && normal.str = empty_str.str
    then ()
    else begin
      output_body oc normal;
      Clflag.Set.Map.iter
        (fun _ list ->
           List.iter ~f:(fun (clflagss, str) ->
               output_string oc ", ";
               let paren = List.length clflagss > 1 in
               if paren then output_string oc "(";
               List.iteri
                 ~f:(fun i clflags ->
                     if i > 0 then output_string oc ", ";
                     output_string oc (Clflag.Set.to_string clflags))
                 clflagss;
               if paren then output_string oc ")";
               output_body oc str;
             )
             list
        )
        ordered_by_lowest_flag;
    end
  in
  let ofs =
    List.fold_left correction.corrected_expectations ~init:0
      ~f:(fun ofs c ->
        output_slice oc file_contents ofs c.payload_loc.loc_start.pos_cnum;
        output_for_all_clflags c.text;
        c.payload_loc.loc_end.pos_cnum)
  in
  output_slice oc file_contents ofs (String.length file_contents);
  output_for_all_clflags correction.trailing_output ~output_empty:false

let write_corrected ~file ~file_contents correction =
  let oc = open_out file in
  output_corrected oc ~file_contents correction;
  close_out oc

let process_expect_file ~startup_clflags fname =
  let corrected_fname = fname ^ ".corrected" in
  let file_contents =
    let ic = open_in_bin fname in
    match really_input_string ic (in_channel_length ic) with
    | s           -> close_in ic; Misc.normalise_eol s
    | exception e -> close_in ic; raise e
  in
  let clflags =
    List.map ~f:Clflag.Set.of_list
      [ []; [ Clflag.Rectypes]; [ Clflag.Principal ] ]
  in
  let warning_state = Warnings.backup () in
  let correction =
    let corrections =
      List.map clflags ~f:(fun clflags ->
          let store = Local_store.fresh () in
          let local_clflags = Clflag.Set.union startup_clflags clflags in
          Clflag.Set.set_current local_clflags;
          Clflag.Set.original := local_clflags;
          Typecore.reset_delayed_checks ();
          Env.reset_required_globals ();
          Lambda.reset_raise_count ();
          Out_type.reset ();
          Out_type.reset_weak_names ();
          Toploop.initialize_toplevel_env ();
          Warnings.with_state warning_state
            (fun () ->
               Local_store.with_store store
                 (fun () ->
                    eval_expect_file fname ~file_contents;
                 )
            )
        )
    in
    Correction.merge corrections
  in
  write_corrected ~file:corrected_fname ~file_contents correction

let repo_root = ref None
let keep_original_error_size = ref false

let main fname =
  if not !keep_original_error_size then
    Clflags.error_size := 0;
  let startup_clflags = Clflag.Set.get_current () in
  Toploop.override_sys_argv
    (Array.sub Sys.argv ~pos:!Arg.current
       ~len:(Array.length Sys.argv - !Arg.current));
  (* Ignore OCAMLRUNPARAM=b to be reproducible *)
  Printexc.record_backtrace false;
  if not !Clflags.no_std_include then begin
    match !repo_root with
    | None -> ()
    | Some dir ->
        (* If we pass [-repo-root], use the stdlib from inside the
           compiler, not the installed one. We use
           [Compenv.last_include_dirs] to make sure that the stdlib
           directory is the last one. *)
        Clflags.no_std_include := true;
        Compenv.last_include_dirs := [Filename.concat dir "stdlib"]
  end;
  Compmisc.init_path ~auto_include:Load_path.no_auto_include ();
  (* We are in interactive mode and should record directive error on stdout *)
  Sys.interactive := true;
  process_expect_file ~startup_clflags fname;
  exit 0

module Options = Main_args.Make_bytetop_options (struct
  include Main_args.Default.Topmain
  let _stdin () = (* disabled *) ()
  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0
  let anonymous s = main s
end);;

let args =
  Arg.align
    ( [ "-repo-root", Arg.String (fun s -> repo_root := Some s),
        "<dir> root of the OCaml repository. This causes the tool to use \
         the stdlib from the current source tree rather than the installed one."
      ; "-keep-original-error-size", Arg.Set keep_original_error_size,
        " truncate long error messages as the compiler would"
      ] @ Options.list
    )

let usage = "Usage: expect <options> [script-file [arguments]]\n\
             options are:"

let () =
(* Early disabling of colors in any output *)
  let () =
    Clflags.color := Some Misc.Color.Never;
    Misc.Style.(setup @@ Some Never)
  in
  try
    Arg.parse args main usage;
    Printf.eprintf "expect: no input file\n";
    exit 2
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
