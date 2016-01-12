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

module Wsb = Inlining_cost.Whether_sufficient_benefit

let print_stars ppf n =
  let s = String.make n '*' in
  Format.fprintf ppf "%s" s

let print_calculation ~depth ~title ~subfunctions ppf wsb =
  Format.pp_open_vbox ppf (depth + 2);
  Format.fprintf ppf "@[<h>%a %s@]@;@;@[%a@]"
    print_stars (depth + 1)
    title
    (Wsb.print_description ~subfunctions) wsb;
  Format.pp_close_box ppf ();
  Format.pp_print_newline ppf ();
  Format.pp_print_newline ppf ()

module Inlined = struct
  type not_inlined_reason =
    | Unspecialized
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t

  let not_inlined_reason_summary ppf = function
    | Unspecialized ->
      Format.pp_print_text ppf
        " because its parameters could not be specialised."
    | Without_subfunctions _ ->
      Format.pp_print_text ppf
        " because the expected benefit did not \
         outweigh the change in code size."
    | With_subfunctions _ ->
      Format.pp_print_text ppf
        " because the expected benefit did not \
         outweigh the change in code size."

  let not_inlined_reason_calculation ~depth ppf = function
    | Unspecialized -> ()
    | Without_subfunctions wsb ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:false ppf wsb
    | With_subfunctions(_, wsb) ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:true ppf wsb

  type inlined_reason =
    | Unconditionally
    | Decl_local_to_application
    | Stub
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t

  let inlined_reason_summary ppf = function
    | Unconditionally ->
      Format.pp_print_text ppf " because of an annotation."
    | Decl_local_to_application ->
      Format.pp_print_text ppf " because it was local to this application."
    | Stub ->
      Format.pp_print_text ppf " because it was a stub."
    | Without_subfunctions _ ->
      Format.pp_print_text ppf
        " because the expected benefit outweighed the change in code size."
    | With_subfunctions _ ->
      Format.pp_print_text ppf
        " because the expected benefit outweighed the change in code size."

  let inlined_reason_calculation ~depth ppf = function
    | Unconditionally -> ()
    | Decl_local_to_application -> ()
    | Stub -> ()
    | Without_subfunctions wsb ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:false ppf wsb
    | With_subfunctions(_, wsb) ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:true ppf wsb

  type t =
    | Not_inlined of not_inlined_reason
    | Inlined of inlined_reason

  let summary ppf = function
    | Not_inlined r ->
      Format.pp_print_text ppf "This function was not inlined";
      not_inlined_reason_summary ppf r
    | Inlined r ->
      Format.pp_print_text ppf "This function was inlined";
      inlined_reason_summary ppf r

  let calculation ~depth ppf = function
    | Not_inlined r -> not_inlined_reason_calculation ~depth ppf r
    | Inlined r -> inlined_reason_calculation ~depth ppf r

end

module Unrolled = struct
  type t =
    | Unrolling_not_tried
    | Not_unrolled of Wsb.t
    | Unrolled of Wsb.t

  let summary ppf = function
    | Unrolling_not_tried ->
      Format.pp_print_text ppf "This function was not eligible for unrolling."
    | Not_unrolled _ ->
      Format.pp_print_text ppf
        "This function was not unrolled because the expected benefit \
         did not outweigh the change in code size."
    | Unrolled _ ->
      Format.pp_print_text ppf
        "This function was unrolled because the expected benefit \
         outweighed the change in code size."

  let calculation ~depth ppf = function
    | Unrolling_not_tried -> ()
    | Not_unrolled wsb ->
      print_calculation
        ~depth ~title:"Unrolling benefit calculation"
        ~subfunctions:true ppf wsb
    | Unrolled wsb ->
      print_calculation
        ~depth ~title:"Unrolling benefit calculation"
        ~subfunctions:true ppf wsb

end

module Specialised = struct
  type t =
    | Specialising_not_tried
    | Not_specialised of Wsb.t
    | Specialised of Wsb.t

  let summary ppf = function
    | Specialising_not_tried ->
      Format.pp_print_text ppf "This function was not eligible for specialising."
    | Not_specialised _ ->
      Format.pp_print_text ppf
        "This function was not specialised because the expected benefit \
         did not outweigh the change in code size."
    | Specialised _ ->
      Format.pp_print_text ppf
        "This function was specialised because the expected benefit \
         outweighed the change in code size."

  let calculation ~depth ppf = function
    | Specialising_not_tried -> ()
    | Not_specialised wsb ->
      print_calculation
        ~depth ~title:"Specialising benefit calculation"
        ~subfunctions:true ppf wsb
    | Specialised wsb ->
      print_calculation
        ~depth ~title:"Specialising benefit calculation"
        ~subfunctions:true ppf wsb

end

module Nonrecursive = struct
  type t = Inlined.t
end

module Recursive = struct
  type t = Unrolled.t * Specialised.t
end

module Prevented = struct
  type t =
    | Function_obviously_too_large of int
    | Function_prevented_from_inlining
    | Level_exceeded
    | Classic_heuristic

  let summary ppf = function
    | Function_obviously_too_large size ->
      Format.pp_print_text ppf " because it was obviously too large ";
      Format.fprintf ppf "(%i)." size
    | Function_prevented_from_inlining -> ()
    | Level_exceeded ->
      Format.pp_print_text ppf " because the inlining depth was exceeded."
    | Classic_heuristic ->
      Format.pp_print_text ppf " by `-classic-heuristic'."
end

module Decision = struct
  type t =
    | Prevented of Prevented.t
    | Nonrecursive of Nonrecursive.t
    | Recursive of Recursive.t

  let summary ppf = function
    | Prevented p ->
      Format.pp_print_text ppf
        "This function was prevented from inlining or specialising";
      Prevented.summary ppf p
    | Nonrecursive i ->
      Inlined.summary ppf i
    | Recursive (u, s) ->
      Format.fprintf ppf "@[<v>@[%a@]@;@;@[%a@]@]"
        Unrolled.summary u Specialised.summary s

  let calculation ~depth ppf = function
    | Prevented _ -> ()
    | Nonrecursive i -> Inlined.calculation ~depth ppf i
    | Recursive (u, s) ->
      Unrolled.calculation ~depth ppf u;
      Specialised.calculation ~depth ppf s
end
