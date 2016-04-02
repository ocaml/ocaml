(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Wsb = Inlining_cost.Whether_sufficient_benefit

let choose_rhyme () = if Random.bool () then `ion else `ound

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
    | Annotation
    | Decl_local_to_application
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t

  let summary ~rhyme ppf = function
    | Annotation ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "This function was inlined because of an annotation\n"
        | `ound ->
            Format.pp_print_string ppf
              "This function was inlined because an annotation was found\n"
        end
    | Decl_local_to_application ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "This function was inlined because it was local to this application."
        | `ound ->
            Format.pp_print_string ppf
              "We inlined this function\n\
               Because its declaration was bound\n\
               To this application\n"
        end
    | With_subfunctions _
    | Without_subfunctions _ ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "We inlined this function\n\
               As the change in code size was outweighed\n\
               By the benefit we expected\n"
        | `ound ->
            Format.pp_print_string ppf
              "Inlining this function was sound\n\
               As the change in code size was outweighed\n\
               By the benefit we expected\n"
        end

  let calculation ~depth ppf = function
    | Annotation -> ()
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
    | Classic_mode
    | Function_obviously_too_large of int
    | Annotation
    | Unspecialised
    | Unrolling_depth_exceeded
    | Self_call
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t


  let summary ~rhyme ppf = function
    | Classic_mode ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "This function was not inlined\n\
               Indeed by the user it was declined\n\
               When he specified the `-Oclassic' option\n"
        | `ound ->
            Format.pp_print_string ppf
              "We did not look around\n\
               To see if we could inline this function\n\
               As the user specified the `-Oclassic' option\n"
        end
    | With_subfunctions _
    | Without_subfunctions _
    | Function_obviously_too_large _ ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "Compared to the code size evolution\n\
               The expected benefit was to slim\n\
               To inline this function on a whim\n"
        | `ound ->
            Format.pp_print_string ppf
              "We once decided of a bound\n\
               Over which no function could be inlined\n\
               And this one was of that kind\n"
        end
    | Annotation ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "This function was not inlined because of an annotation\n"
        | `ound ->
            Format.pp_print_string ppf
              "This function was not inlined because an annotation was found\n"
        end
    | Unspecialised ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "We did not inline this function\n\
               Because its parameters admitted no specialisation\n"
        | `ound ->
            Format.pp_print_string ppf
              "No possible specialisation was found\n\
               For the parameters of this function\n\
               Clearly to being inlined it had no pretention\n"
        end
    | Unrolling_depth_exceeded ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "We did not inline this function\n\
               It could not even be considered\n\
               As its unrolling depth was exceeded\n "
        | `ound ->
            Format.pp_print_string ppf
              "Its unrolling depth was too profound\n\
               For any inlining of this function\n\
               To ever be a viable option\n "
        end
    | Self_call ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "This function was not inlined because it was self recursion."
        | `ound ->
            Format.pp_print_string ppf
              "When we looked at this we frowned\n\
               This function was not inlined\n\
               To self calls we are blind\n "
        end

  let calculation ~depth ppf = function
    | Classic_mode
    | Function_obviously_too_large _
    | Annotation
    | Unspecialised
    | Unrolling_depth_exceeded
    | Self_call -> ()
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
    | Annotation
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t

  let summary ~rhyme ppf = function
    | Annotation ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "This function was specialised because of an annotation\n"
        | `ound ->
            Format.pp_print_string ppf
              "This function was specialised because an annotation was found\n"
        end
    | With_subfunctions _
    | Without_subfunctions _ ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "We specialised this function\n\
               As the change in code size is outweighed\n\
               By the benefit which is expected\n"
        | `ound ->
            Format.pp_print_string ppf
              "This function was specialised this time around\n\
               As the change in code size is outweighed\n\
               By the benefit which is expected\n"
        end


  let calculation ~depth ppf = function
    | Annotation -> ()
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
    | Function_obviously_too_large of int
    | Annotation
    | Not_recursive
    | Not_closed
    | No_invariant_parameters
    | No_useful_approximations
    | Self_call
    | Not_beneficial of Wsb.t * Wsb.t

  let summary ppf = function
    | Classic_mode ->
      Format.pp_print_string ppf
        "The use of `-Oclassic' prevented\n\
         This function from being specialised\n"
    | Function_obviously_too_large _ ->
      Format.pp_print_string ppf
        "Because it was obviously too\n\
         Large This function\n\
         Deserved no specialisation\n\
         At least from our point of view\n"
    | Annotation ->
      Format.pp_print_string ppf
        "The presence of an annotation\n\
         Prevented the specialisation of this function\n"
    | Not_recursive ->
      Format.pp_print_string ppf
        "As it contained no recursion\n\
         We did not specialise this function\n"
    | Not_closed ->
      Format.pp_print_string ppf
        "For this function\n\
         To specialisation\n\
         We were opposed\n\
         As it isn't closed\n"
    | No_invariant_parameters ->
      Format.pp_print_string ppf
        "As none of its parameters is invariant\n\
         This function won't be specialised for the moment\n"
    | No_useful_approximations ->
      Format.pp_print_string ppf
        "We found no information that matters\n\
         About its invariant parameters\n\
         So no specialisation\n\
         Was done to this function\n"
    | Self_call ->
      Format.pp_print_string ppf
        "This was a self call\n\
         And although producing a rhyme we shall\n\
         What matters in the end is the following decision\n\
         We did not specialise this function\n"
    | Not_beneficial _ ->
      Format.pp_print_string ppf
        "In the case of this function\n\
         The expected benefit was to slim\n\
         Compared to the code size evolution\n\
         To specialise it on a whim\n"

  let calculation ~depth ppf = function
    | Classic_mode
    | Function_obviously_too_large _
    | Annotation
    | Not_recursive
    | Not_closed
    | No_invariant_parameters
    | No_useful_approximations
    | Self_call -> ()
    | Not_beneficial(_, wsb) ->
      print_calculation
        ~depth ~title:"Specialising benefit calculation"
        ~subfunctions:true ppf wsb

end

module Prevented = struct
  type t =
    | Function_prevented_from_inlining
    | Level_exceeded

  let summary ~rhyme ppf = function
    | Function_prevented_from_inlining ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "No inlining or specialising could be done with this function\n"
        | `ound ->
            Format.pp_print_string ppf
              "Inlining or specialising would not have been unsound\n\
               However it was of so little worth\n\
               That the operation wasn't even put forth\n"
        end
    | Level_exceeded ->
        begin match rhyme with
        | `ion ->
            Format.pp_print_string ppf
              "From inlining or specialising this function\n\
               Was prevented\n\
               As the inlining depth was exceeded\n"
        | `ound ->
            Format.pp_print_string ppf
              "Since the inlining depth was too profound\n\
               It was decided that no operation\n\
               Neither inlining\n\
               Nor specialising\n\
               Could be done on this function\n"
        end
end

module Decision = struct
  type t =
    | Prevented of Prevented.t
    | Specialised of Specialised.t
    | Inlined of Not_specialised.t * Inlined.t
    | Unchanged of Not_specialised.t * Not_inlined.t

  let summary ~rhyme ppf = function
    | Prevented p ->
      Prevented.summary ~rhyme ppf p
    | Specialised s ->
      Specialised.summary ~rhyme ppf s
    | Inlined (s, i) ->
      Format.fprintf ppf "%a\n%a"
        Not_specialised.summary s (Inlined.summary ~rhyme) i
    | Unchanged (s, i) ->
      Format.fprintf ppf "%a\n%a"
        Not_specialised.summary s (Not_inlined.summary ~rhyme) i

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
