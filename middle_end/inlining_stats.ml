(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

let vim_trailer = "vim:fdm=expr:filetype=plain:\
  foldexpr=getline(v\\:lnum)=~'^\\\\s*$'&&getline(v\\:lnum+1)=~'\\\\S'?'<1'\\:1"

module Closure_stack = struct
  type t
    = (Closure_id.t * Inlining_stats_types.where_entering_closure) list

  let create () = []

  let _compare t1 t2 =
    match t1, t2 with
    | (id1, _)::_, (id2, _)::_ ->
      let (_ : string) = Format.flush_str_formatter () in
      let (id1 : string) =
        Format.fprintf Format.str_formatter "%a" Closure_id.print id1;
        Format.flush_str_formatter ()
      in
      let id2 =
        Format.fprintf Format.str_formatter "%a" Closure_id.print id2;
        Format.flush_str_formatter ()
      in
      String.compare id1 id2
    | _ -> 0

  let note_entering_closure t ~closure_id ~where =
    if not !Clflags.inlining_stats then t
    else t @ [closure_id, where]

  let pop = function
    | [] -> failwith "Closure_stack.pop on empty stack"
    | hd::tl -> (fst hd), tl

  let save t ~out_channel =
    let print_elt (closure_id, where) ~last_one =
      let current_unit = Compilation_unit.get_current_exn () in
      let output =
        if Closure_id.in_compilation_unit closure_id current_unit then
          Closure_id.output
        else
          Closure_id.output_full
      in
      begin match (where : Inlining_stats_types.where_entering_closure) with
        | Inline_by_copying_function_declaration closure_ids ->
          let closure_ids = Closure_id.Set.remove closure_id closure_ids in
          if Closure_id.Set.cardinal closure_ids < 1 then
            Printf.fprintf out_channel "in copy of %a" output closure_id
          else begin
            Printf.fprintf out_channel "in copy of %a (and" output closure_id;
            Closure_id.Set.iter (fun closure_id ->
                Printf.fprintf out_channel " %a" output closure_id)
              closure_ids;
            Printf.fprintf out_channel ")"
          end
        | Transform_set_of_closures_expression ->
          Printf.fprintf out_channel "decl of %a" output closure_id
        | Inline_by_copying_function_body ->
          Printf.fprintf out_channel "inlined body of %a" output closure_id
        | Inlining_decision ->
          Printf.fprintf out_channel "%a" output closure_id
      end;
      if not last_one then begin
        match (where : Inlining_stats_types.where_entering_closure) with
        | Inline_by_copying_function_declaration _
        | Inline_by_copying_function_body ->
          Printf.fprintf out_channel ": "
        | Transform_set_of_closures_expression
        | Inlining_decision -> Printf.fprintf out_channel " -> "
      end
    in
    let rec loop = function
      | [] -> Printf.fprintf out_channel "[]"
      | [elt] -> print_elt elt ~last_one:true
      | elt::elts ->
        print_elt elt ~last_one:false;
        loop elts
    in
    loop t
end

let time = ref 0

module Line_number_then_time = struct
  type t = Debuginfo.t * int

  let compare_fst (((dbg1, t1) : t), _) (((dbg2, t2) : t), _) =
    match compare dbg1.dinfo_line dbg2.dinfo_line with
    | -1 -> -1
    | 1 -> 1
    | _ -> compare t1 t2

  let create ~debuginfo ~time = debuginfo, time
  let line_number t = (fst t).Debuginfo.dinfo_line
end

let decisions :
  (Line_number_then_time.t
      * (Closure_stack.t * Inlining_stats_types.Decision.t)) list
    Closure_id.Tbl.t = Closure_id.Tbl.create 42

let record_decision decision ~closure_stack ~debuginfo =
  if !Clflags.inlining_stats then begin
    let closure_id, closure_stack = Closure_stack.pop closure_stack in
    let bucket =
      match Closure_id.Tbl.find decisions closure_id with
      | exception Not_found -> []
      | bucket -> bucket
    in
    let key = Line_number_then_time.create ~debuginfo ~time:!time in
    let data = closure_stack, decision in
    (* The order here is important so that the "time rebasing" works
       properly, below. *)
    Closure_id.Tbl.replace decisions closure_id ((key, data) :: bucket);
    incr time
  end

let really_save_then_forget_decisions ~output_prefix =
  let out_channel = open_out (output_prefix ^ ".inlining") in
  Closure_id.Tbl.iter (fun closure_id bucket ->
      Printf.fprintf out_channel "%a\n" Closure_id.output closure_id;
      let bucket =
        let rebased_time = ref (-1) in
        (* Rebase timestamps to start at zero within each bucket. *)
        List.rev_map (fun (key, (closure_stack, decision)) ->
            incr rebased_time;
            key, (!rebased_time, closure_stack, decision))
          bucket
      in
      let bucket = List.sort Line_number_then_time.compare_fst bucket in
      List.iter (fun (key, (time, closure_stack, decision)) ->
          let line = Line_number_then_time.line_number key in
          Printf.fprintf out_channel "  %5d: (%5d) " line time;
          Closure_stack.save closure_stack ~out_channel;
          Printf.fprintf out_channel ": %s\n"
            (Inlining_stats_types.Decision.to_string decision))
        bucket;
      Printf.fprintf out_channel "\n") decisions;
  Printf.fprintf out_channel "# %s\n" vim_trailer;
  close_out out_channel;
  Closure_id.Tbl.clear decisions;
  time := 0

let save_then_forget_decisions ~output_prefix =
  if !Clflags.inlining_stats then begin
    really_save_then_forget_decisions ~output_prefix
  end
