(* Formatting printing functions *)

val fprintf: out_channel -> ('a, out_channel, unit) format -> 'a
        (* [fprintf outchan format arg1 ... argN] formats the arguments
           [arg1] to [argN] according to the format string [format],
           and outputs the resulting string on the channel [outchan].

           The format is a character string which contains two types of
           objects:  plain  characters, which are simply copied to the
           output channel, and conversion specifications, each of which
           causes  conversion and printing of one argument.

           Conversion specifications consist in the [%] character, followed
           by optional flags and field widths, followed by one conversion
           character. The conversion characters and their meanings are:
-          [d] or [i]: convert an integer argument to signed decimal
-          [u]: convert an integer argument to unsigned decimal
-          [x]: convert an integer argument to unsigned hexadecimal,
                using lowercase letters.
-          [X]: convert an integer argument to unsigned hexadecimal,
                using uppercase letters.
-          [s]: insert a string argument
-          [c]: insert a character argument
-          [f]: convert a floating-point argument to decimal notation,
                in the style [dddd.ddd]
-          [e] or [E]: convert a floating-point argument to decimal notation,
                in the style [d.ddd e+-dd] (mantissa and exponent)
-          [g] or [G]: convert a floating-point argument to decimal notation,
                in style [f] or [e], [E] (whichever is more compact)
-          [b]: convert a boolean argument to the string [true] or [false]
-          [a]: user-defined printer. Takes two arguments and apply the first
                one to [outchan] (the current output channel) and to the second
                argument. The first argument must therefore have type
                [out_channel -> 'b -> unit] and the second ['b].
                The output produced by the function is therefore inserted
                in the output of [fprintf] at the current point.
-          [t]: same as [%a], but takes only one argument (with type
                [out_channel -> unit]) and apply it to [outchan].
-          Refer to the C library [printf] function for the meaning of
           flags and field width specifiers.

           If too few arguments are provided, printing stops just
           before converting the first missing argument. *)

val printf: ('a, out_channel, unit) format -> 'a
        (* Same as [fprintf], but output on [std_out]. *)

val eprintf: ('a, out_channel, unit) format -> 'a
        (* Same as [fprintf], but output on [std_err]. *)

val sprintf: ('a, unit, string) format -> 'a
        (* Same as [printf], but return the result of formatting in a
           string. *)
