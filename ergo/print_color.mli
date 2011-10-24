(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2010                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*     Alain Mebsout                                                      *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

type style =
    (* users ansi style number *)
  |User of int

      (* the typeset *)
  |Normal
  |Bold  (* the text is in bold *)
  |Bold_off (* the text is unbold *)
  |Underline
  |Underline_off
  |Inverse
  |Inverse_off
  |Blink_off
      (* Foreground color *)
  |F_Black 
  |F_Red
  |F_Green
  |F_Yellow
  |F_Blue
  |F_Magenta
  |F_Cyan
  |F_Gray
  |F_Default
      (* Background color *)
  |G_Black 
  |G_Red
  |G_Green
  |G_Yellow
  |G_Blue
  |G_Magenta
  |G_Cyan
  |G_Gray
  |G_Default
      (* Foreground color bright*)
  |F_Black_B
  |F_Red_B
  |F_Green_B
  |F_Yellow_B
  |F_Blue_B
  |F_Magenta_B
  |F_Cyan_B
  |F_Gray_B
  |F_Default_B
      (* Background color bright*)
  |G_Black_B
  |G_Red_B
  |G_Green_B
  |G_Yellow_B
  |G_Blue_B
  |G_Magenta_B
  |G_Cyan_B
  |G_Gray_B
  |G_Default_B

(* if you net the start you 
   must use the stop in order 
   to have good color *)
val start : style list -> unit
val stop : unit -> unit

(* give a string instead of stdout *)
val sstart : style list-> string
val sstop : unit -> string

(* use Format instead of Printf *)
val format_start : style list-> unit
val format_stop : unit -> unit
val fstartf : Format.formatter -> style list -> unit
val fstopf : Format.formatter -> unit

(*disable all(returned string are empty), but keep track of the start and stop *)
val disable : bool -> unit

(* Les tags sont de la forme "C.<un_style>" (sauf user) *)
val add_to_format_tag : Format.formatter -> unit

val set_margin_with_term_width : Format.formatter -> unit

(* Just some tools *)
  (* print_list separator print_element formatter list *)
(*val print_list : ('a, 'b, 'c, 'd, 'd, 'a) format6 -> (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e list -> unit*)

val print_list : string -> (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e list -> unit
