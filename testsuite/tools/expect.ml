(***************************************Correction.***********************************)
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
     output without -principal
     |}, Principal{|
     output with -principal
     |}]
   ]}

   {[
     [%%expect {|
     output without -rectypes
     |}, Rectypesl{|
     output with -rectypes
     |}]
   ]}
*)

[@@@ocaml.warning "-40"]

open StdLabels

(* representation of: {tag|str|tag} *)
type string_constant =
  { str : string
  ; tag : string
  }

module Clflag = struct
  type t =
    | Principal
    | Rectypes

  let to_string = function
    | Principal -> "Principal"
    | Rectypes -> "Rectypes"

  module Set = struct
    module T = Set.Make(struct
        type nonrec t = t
        let compare = compare
      end)
    include T

    module Map = Map.Make(T)

    let original = ref empty

    let get_current () =
      union
        (if !Clflags.principal then singleton Principal else empty)
        (if !Clflags.recursive_types then singleton Rectypes else empty)

    let set_current t =
      Clflags.principal := mem Principal t;
      Clflags.recursive_types := mem Rectypes t;
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
            | other -> Location.raise_errorf ~loc "unknown flag: %s" other)
        ~init:empty (Longident.flatten lid)
  end
end


type expectation =
  { extid_loc   : Location.t (* Location of "expect" in "[%%expect ...]" *)
  ; payload_loc : Location.t (* Location of the whole payload *)
  ; text        : string_constant Clflag.Set.Map.t
  }

(* A list of phrases with the expected toplevel output *)
type chunk =
  { phrases     : Parsetree.toplevel_phrase list
  ; expectation : expectation
  }

type correct_op =
  | Set of Clflag.Set.t * string_constant
  | Remove of Clflag.Set.t

let apply_correct_op expectation correct_op =
  match correct_op with
  | Set (clflags, s) ->
      { expectation with text = Clflag.Set.Map.add clflags s expectation.text }
  | Remove clflags ->
      { expectation with text = Clflag.Set.Map.remove clflags expectation.text }

module Correction = struct
  type t =
    { corrected_expectations : (expectation * (correct_op list)) list
    ; trailing_output        : string Clflag.Set.Map.t
    }
end

module Merged_correction = struct
  type t =
    { corrected_expectations : expectation list
    ; trailing_output        : string Clflag.Set.Map.t
    }

  module LocationMap = Map.Make(struct
      include Location

      let compare = compare
    end)

  let merge (clist : Correction.t list) : t =
    let corrected_expectations, trailing_output =
      List.fold_left
        ~f:(fun (cmap, tmap) { Correction.corrected_expectations; trailing_output } ->
            List.fold_left
              ~f:(fun acc (expectation, correct_op) ->
                  LocationMap.update
                    expectation.extid_loc
                    (fun current ->
                       Some
                         (List.fold_left
                            ~f:apply_correct_op
                            ~init:(Option.value ~default:expectation current)
                            correct_op
                         )
                    )
                    acc
                )
              ~init:cmap
              corrected_expectations
          , Clflag.Set.Map.merge
              (fun _key to1 to2 ->
                 match to1, to2 with
                 | None, None -> None
                 | Some to1, None -> Some to1
                 | None, Some to2 -> Some to2
                 | Some to1, Some to2 when to1 = to2 -> Some to1
                 | _ -> Location.raise_errorf
                          ~loc:Location.none "conflicting trailing outputs"
              )
              tmap
              trailing_output
        )
      ~init:(LocationMap.empty, Clflag.Set.Map.empty)
      clist
    in
    { corrected_expectations =
        LocationMap.to_list corrected_expectations
        |> List.map ~f:snd
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
      | _ -> invalid_payload "not a string"
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
        | Not_found -> { tag = ""; str = "" }
  in
  let current_clflags = Clflag.Set.get_current () in
  let correct_op =
    if Clflag.Set.equal current_clflags !Clflag.Set.original
    then []
    else if Clflag.Set.Map.mem !Clflag.Set.original expectation.text
    then [ Remove !Clflag.Set.original ]
    else [ ]
  in
  if s.str = output then
    match correct_op with
    | [] -> None
    | _  -> Some (expectation, correct_op)
  else
    let s = { s with str = output } in
    Some (expectation, Set (current_clflags, s) :: correct_op)

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
        match eval_expectation chunk.expectation ~output with
        | None -> acc
        | Some correction -> correction :: acc)
      |> List.rev)
  in
  let trailing_output =
    match trailing_code with
    | None -> ""
    | Some phrases ->
      capture_everything buf ppf ~f:(fun () -> exec_phrases phrases)
  in
  let trailing_output =
    Clflag.Set.Map.singleton (Clflag.Set.get_current ()) trailing_output
  in
  { Correction.corrected_expectations; trailing_output }

let output_slice oc s a b =
  output_string oc (String.sub s ~pos:a ~len:(b - a))

module String_map = Map.Make(String)

let output_corrected oc ~file_contents (correction : Merged_correction.t) =
  let output_body oc { str; tag } =
    Printf.fprintf oc "{%s|%s|%s}" tag str tag
  in
  let ofs =
    List.fold_left correction.corrected_expectations ~init:0
      ~f:(fun ofs c ->
        output_slice oc file_contents ofs c.payload_loc.loc_start.pos_cnum;
        let normal =
          Clflag.Set.Map.find_opt Clflag.Set.empty c.text
          |> Option.value ~default:{ str = ""; tag = "" }
        in
        let smap =
          Clflag.Set.Map.fold
            (fun key body acc ->
               if body.str = normal.str then acc
               else String_map.add_to_list body.str key acc
            )
            c.text
            String_map.empty
        in
        let ordered_by_lowest_flag =
          String_map.fold
            (fun str clflagss acc ->
               let clflagss =
                 List.sort_uniq ~cmp:Clflag.Set.compare clflagss
               in
               let low_flag = List.hd clflagss in
               Clflag.Set.Map.add low_flag (clflagss, str) acc
            )
            smap
            Clflag.Set.Map.empty
        in
        output_body oc normal;
        Clflag.Set.Map.iter
          (fun _ (clflagss, str) ->
             output_string oc ", ";
             let paren = List.length clflagss > 1 in
             if paren then output_string oc "(";
             List.iteri
               ~f:(fun i clflags ->
                   if i > 0 then output_string oc ", ";
                   output_string oc (Clflag.Set.to_string clflags))
               clflagss;
             if paren then output_string oc ")";
             output_body oc { str; tag = "" }
          )
          ordered_by_lowest_flag;
        c.payload_loc.loc_end.pos_cnum)
  in
  output_slice oc file_contents ofs (String.length file_contents);
  ignore correction.trailing_output;
  ()
  (*
  match correction.trailing_output with
  | "" -> ()
  | s  -> Printf.fprintf oc "\n[%%%%expect{|%s|}]\n" s
     *)

let write_corrected ~file ~file_contents correction =
  let oc = open_out file in
  output_corrected oc ~file_contents correction;
  close_out oc

let process_expect_file fname =
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
  let correction =
    let corrections =
      List.map clflags ~f:(fun clflags ->
          let store = Local_store.fresh () in
          Clflag.Set.set_current clflags;
          Clflag.Set.original := Clflag.Set.get_current ();
          Typecore.reset_delayed_checks ();
          Env.reset_required_globals ();
          Out_type.reset ();
          Toploop.initialize_toplevel_env ();
          (* We are in interactive mode and should record directive error on stdout *)
          Sys.interactive := true;
          Local_store.with_store store
            (fun () ->
               eval_expect_file fname ~file_contents;
            )
        )
    in
    Merged_correction.merge corrections
  in
  write_corrected ~file:corrected_fname ~file_contents correction

let repo_root = ref None
let keep_original_error_size = ref false

let main fname =
  if not !keep_original_error_size then
    Clflags.error_size := 0;
  Clflag.Set.original := Clflag.Set.get_current ();
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
  process_expect_file fname;
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
