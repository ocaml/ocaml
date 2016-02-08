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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

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

  type t =
    | Unconditionally
    | Decl_local_to_application
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t

  let summary ppf = function
    | Unconditionally ->
      Format.pp_print_text ppf
        "This function was inlined because of an annotation."
    | Decl_local_to_application ->
      Format.pp_print_text ppf
        "This function was inlined because it was local to this application."
    | Without_subfunctions _ ->
      Format.pp_print_text ppf
        "This function was inlined because \
         the expected benefit outweighed the change in code size."
    | With_subfunctions _ ->
      Format.pp_print_text ppf
        "This function was inlined because \
         the expected benefit outweighed the change in code size."

  let calculation ~depth ppf = function
    | Unconditionally -> ()
    | Decl_local_to_application -> ()
    | Without_subfunctions wsb ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:false ppf wsb
    | With_subfunctions(_, wsb) ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:true ppf wsb

end

module Not_inlined = struct
  type t =
    | Unspecialised
    | Unrolling_depth_exceeded
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t


  let summary ppf = function
    | Unspecialised ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         its parameters could not be specialised."
    | Unrolling_depth_exceeded ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         its unrolling depth was exceeded."
    | Without_subfunctions _ ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         the expected benefit did not outweigh the change in code size."
    | With_subfunctions _ ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         the expected benefit did not outweigh the change in code size."

  let calculation ~depth ppf = function
    | Unspecialised -> ()
    | Unrolling_depth_exceeded -> ()
    | Without_subfunctions wsb ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:false ppf wsb
    | With_subfunctions(_, wsb) ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:true ppf wsb

end

module Specialised = struct
  type t =
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t

  let summary ppf _ =
    Format.pp_print_text ppf
      "This function was specialised because the expected benefit \
       outweighed the change in code size."

  let calculation ~depth ppf = function
    | Without_subfunctions wsb ->
        print_calculation
          ~depth ~title:"Specialising benefit calculation"
          ~subfunctions:false ppf wsb
    | With_subfunctions(_, wsb) ->
        print_calculation
          ~depth ~title:"Specialising benefit calculation"
          ~subfunctions:true ppf wsb
end

module Not_specialised = struct
  type t =
    | Classic_mode
    | Not_recursive
    | Not_closed
    | No_invariant_parameters
    | No_useful_approximations
    | Not_beneficial of Wsb.t * Wsb.t

  let summary ppf = function
    | Classic_mode ->
      Format.pp_print_text ppf
        "This function was prevented from specialising by `-classic-heuristic'."
    | Not_recursive ->
      Format.pp_print_text ppf
        "This function was not specialised because it is not recursive."
    | Not_closed ->
      Format.pp_print_text ppf
        "This function was not specialised because it is not closed."
    | No_invariant_parameters ->
      Format.pp_print_text ppf
        "This function was not specialised because \
         it has no invariant parameters."
    | No_useful_approximations ->
      Format.pp_print_text ppf
        "This function was not specialised because \
         there was no useful information about any of its invariant parameters."
    | Not_beneficial _ ->
      Format.pp_print_text ppf
        "This function was not specialised because \
         the expected benefit did not outweigh the change in code size."

  let calculation ~depth ppf = function
    | Classic_mode
    | Not_recursive
    | Not_closed
    | No_invariant_parameters
    | No_useful_approximations -> ()
    | Not_beneficial(_, wsb) ->
      print_calculation
        ~depth ~title:"Specialising benefit calculation"
        ~subfunctions:true ppf wsb

end

module Prevented = struct
  type t =
    | Function_obviously_too_large of int
    | Function_prevented_from_inlining
    | Level_exceeded
    | Classic_heuristic
    | Self_call

  let summary ppf = function
    | Function_obviously_too_large size ->
      Format.pp_print_text ppf " because it was obviously too large ";
      Format.fprintf ppf "(%i)." size
    | Function_prevented_from_inlining -> ()
    | Level_exceeded ->
      Format.pp_print_text ppf " because the inlining depth was exceeded."
    | Classic_heuristic ->
      Format.pp_print_text ppf " by `-classic-heuristic'."
    | Self_call ->
      Format.pp_print_text ppf " because it was a self call."
end

module Decision = struct
  type t =
    | Prevented of Prevented.t
    | Specialised of Specialised.t
    | Inlined of Not_specialised.t * Inlined.t
    | Unchanged of Not_specialised.t * Not_inlined.t

  let summary ppf = function
    | Prevented p ->
      Format.pp_print_text ppf
        "This function was prevented from inlining or specialising";
      Prevented.summary ppf p
    | Specialised s ->
      Specialised.summary ppf s
    | Inlined (s, i) ->
      Format.fprintf ppf "@[<v>@[%a@]@;@;@[%a@]@]"
        Not_specialised.summary s Inlined.summary i
    | Unchanged (s, i) ->
      Format.fprintf ppf "@[<v>@[%a@]@;@;@[%a@]@]"
        Not_specialised.summary s Not_inlined.summary i

  let calculation ~depth ppf = function
    | Prevented _ -> ()
    | Specialised s ->
      Specialised.calculation ~depth ppf s
    | Inlined (s, i) ->
      Not_specialised.calculation ~depth ppf s;
      Inlined.calculation ~depth ppf i
    | Unchanged (s, i) ->
      Not_specialised.calculation ~depth ppf s;
      Not_inlined.calculation ~depth ppf i
end
