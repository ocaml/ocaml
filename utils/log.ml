(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


module Label_map = Misc.Stdlib.String.Map

module H = Diagnostic_history
module D = Diagnostic
module R = D.Record_introspection
module V = Diagnostic_validation

type ('id,'a) field = ('id,'a) Diagnostic.field
type version = Diagnostic_history.version = { major:int; minor:int }

module Device = struct
  type t =
    {
      initialized: bool ref;
      ppf: Format.formatter ref;
      on_close: unit -> unit;
    }

  let make ?(on_close=ignore) ppf =
    { initialized=ref false; ppf; on_close}
  let err = make (ref Format.err_formatter)
  let std = make (ref Format.std_formatter)

  let out_channel out =
    let on_close () =
      Out_channel.flush out;
      Out_channel.close out
    in
    let ppf = Format.formatter_of_out_channel out in
    make ~on_close (ref ppf)

  let init_if_needed color out =
    if not !(out.initialized) then begin
      out.initialized := true;
      let ppf = !(out.ppf) in
      let color = Misc.Style.enable_color color in
      Misc.Style.set_tag_handling ~color ppf;
      Format.fprintf ppf "@[<v>"
    end

  let flush c =
    Format.fprintf !(c.ppf) "%!"

  let separate c = Format.pp_print_newline !(c.ppf) ()

  let close0 c = c.on_close ()
  let close_stream c =
    if not !(c.initialized) then ()
    else (Format.fprintf !(c.ppf) "@,@]%!"; c.initialized := false);
    c.on_close ()

  let ppf settings out =
    init_if_needed settings out;
    !(out.ppf)
end


type printer = {
  record: Format.formatter -> Diagnostic.typed_record -> unit;
  item: Format.formatter -> string * Diagnostic.typed_val -> unit
}

type 'a mode =
  | Streaming of Device.t
  | Delayed of {store:'a Diagnostic.record; output:Device.t option}

type redirections = {
  mutable map: (Device.t option * redirections) Label_map.t
}
let empty_redirections () = { map = Label_map.empty }

let redirection key r =
  match Label_map.find_opt key r.map with
  | None ->
      let child = empty_redirections () in
      r.map <- Label_map.add key (None,child) r.map;
      None, child
  | Some(d,x) -> d, x

let device_redirection key r =
  match Label_map.find_opt key r.map with
  | None -> None
  | Some(d,_) -> d

let iter_redirection f r =
  Label_map.iter (fun _ (x,_) -> Option.iter f x) r.map

type 'a log =
  {
      redirections: redirections;
      version: Diagnostic_validation.version;
      scheme: 'a Diagnostic.t;
      settings: Misc.Color.setting option;
      mode: 'a mode;
      printer:printer;
  }

let log_store log = match log.mode with
  | Delayed r -> Some r.store
  | Streaming _ -> None

let log_scheme log = log.scheme
let log_version log = Diagnostic_validation.exact_version log.version

type 'a t = 'a log


(** {1:log_scheme_versionning  Current version of the log } *)

let delayed_mode output = Delayed { store=R.empty (); output }

let make ~streaming ~printer settings version scheme output =
  let mode =
    if streaming then Streaming output
    else delayed_mode (Some output)
  in
  {
    redirections = empty_redirections ();
    settings;
    version;
    printer;
    mode;
    scheme;
  }

let tmp scheme =
  {
  settings = None;
  redirections = empty_redirections ();
  version=(Downward_compatible {major=0;minor=0});
  scheme;
  printer = { record = (fun _ _ -> ()); item = (fun _ _ -> ()) };
  mode = delayed_mode None;
}

let redirect log field device  =
  let r = log.redirections in
  let new_redirection = Some device, empty_redirections () in
  r.map <-Label_map.add (Diagnostic.field_name field) new_redirection r.map

let generic_detach label_scheme ~set ~lift ~extract log
    (field: _ Diagnostic.field) =
  let out, redirections = redirection (D.field_name field) log.redirections in
  let mode = match log.mode with
    | Streaming parent -> Streaming (Option.value ~default:parent out)
    | Delayed { store; output } ->
        let output = match out, output with
          | Some _ as out, _ -> out
          | None, out -> out
        in
        let store =
          match Option.bind (R.get store field) extract with
          | Some store -> store
          | None ->
              let field_store = R.empty () in
              set store (V.exact_version log.version) ~field (lift field_store);
              field_store
        in
        Delayed { store; output }
  in
  let child =
    { scheme=label_scheme (D.field_type field);
      mode;
      printer=log.printer;
      version = log.version;
      settings = log.settings;
      redirections;
    } in
  child

let some x = Some x
let detach log field =
  generic_detach D.record_scheme
    ~set:R.set ~lift:Fun.id ~extract:some log field
let detach_item log field =
  generic_detach D.record_list_scheme
    ~set:R.cons
    ~lift:Fun.id
    ~extract:(Fun.const None)
    log field

let set log (field: _ D.field) x =
  let version = log.version in
  match log.mode with
  | Delayed {store; _} -> R.set store (V.exact_version version) ~field x
  | Streaming output ->
      let status = match D.field_info log.scheme field with
        | Some lmd ->
            let v = V.reference_version version in
            H.Lifetime.stage_at (Some v) lmd.status
        | None -> H.Lifetime.Deletion
      in
      match status with
      | Deletion | Future -> ()
      | Inception | Publication | Expansion | Deprecation ->
          let r = device_redirection (D.field_name field) log.redirections in
          let out = Option.value ~default:output r in
          let ppf = Device.ppf log.settings out in
          Format.fprintf ppf "@[<v>%a@,@]%!"
            log.printer.item (D.field_name field, D.V(D.field_type field,x))

let cons log field x =
  match log.mode with
  | Streaming _ -> set log field [x]
  | Delayed {store;_} -> R.cons store (V.exact_version log.version) ~field x

let (.%[]<-) log field x = set log field x

let get log field = match log_store log with
  | None -> None
  | Some store -> R.get store field

let dynamic_get log field = match log_store log with
  | None -> None
  | Some store -> R.dynamic_get store field

let f field log fmt = Format.kasprintf (fun s -> log.%[field] <- s) fmt
let itemf field log fmt = Format.kasprintf (cons log field) fmt

let d field log fmt = Format_doc.kdoc_printf (set log field) fmt
let itemd field log fmt = Format_doc.kdoc_printf (cons log field) fmt

let flush: type a. a log -> unit = fun log ->
  begin match log.mode with
  | Delayed { output=None; store } -> R.reset store
  | Streaming output -> Device.flush output
  | Delayed { output=Some output; store } ->
      let _ = V.diagnostic ~version:log.version log.scheme store in
      let ppf = Device.ppf log.settings output in
      log.printer.record ppf (R(log.scheme, store));
      R.reset store
  end;
  iter_redirection Device.flush log.redirections

let separate log = match log.mode with
  | Streaming d -> Device.separate d
  | _ -> ()

let close: type a. a log -> unit = fun log ->
  match log.mode with
  | Streaming d ->
      Device.close_stream d; iter_redirection Device.close0 log.redirections
  | Delayed { output; _ } ->
      Option.iter Device.close0 output;
      iter_redirection Device.close0 log.redirections

let close log = flush log; close log

let replay ~source ~dest =
  match log_store source with
  | None -> ()
  | Some store ->
      Seq.iter
        (fun (D.F(field,x)) -> dest.%[field] <- x )
        (R.all_fields store)

(** {1:log_publication }*)


let log_if dlog field flag printer x =
  if flag then
    Format.kasprintf (fun s -> dlog.%[field] <- s) "%a" printer x
