(* TEST
 include testing;
 flags = "-no-strict-formats";
*)

(*

A test file for the Format module.

*)

open Testing;;
open Format;;

type tster = { x: string; y: int }

let say s = Printf.printf s;;

try

  say "d/i positive\n%!";
  test (sprintf "%d/%i" 42 43 = "42/43");
  test (sprintf "%-4d/%-5i" 42 43 = "42  /43   ");
  test (sprintf "%04d/%05i" 42 43 = "0042/00043");
  test (sprintf "%+d/%+i" 42 43 = "+42/+43");
  test (sprintf "% d/% i" 42 43 = " 42/ 43");
  test (sprintf "%#d/%#i" 42 43 = "42/43");
  test (sprintf "%4d/%5i" 42 43 = "  42/   43");
  test (sprintf "%*d" (-4) 42 = "42  ");
  test (sprintf "%*d/%*i" 4 42 5 43 = "  42/   43");
  test (sprintf "%-0+#4d/%-0 #5i" 42 43 = "+42 / 43  ");

  say "\nd/i negative\n%!";
  test (sprintf "%d/%i" (-42) (-43) = "-42/-43");
  test (sprintf "%-4d/%-5i" (-42) (-43) = "-42 /-43  ");
  test (sprintf "%04d/%05i" (-42) (-43) = "-042/-0043");
  test (sprintf "%+d/%+i" (-42) (-43) = "-42/-43");
  test (sprintf "% d/% i" (-42) (-43) = "-42/-43");
  test (sprintf "%#d/%#i" (-42) (-43) = "-42/-43");
  test (sprintf "%4d/%5i" (-42) (-43) = " -42/  -43");
  test (sprintf "%*d" (-4) (-42) = "-42 ");
  test (sprintf "%*d/%*i" 4 (-42) 5 (-43) = " -42/  -43");
  test (sprintf "%-0+ #4d/%-0+ #5i" (-42) (-43) = "-42 /-43  ");

  say "\nu positive\n%!";
  test (sprintf "%u" 42 = "42");
  test (sprintf "%-4u" 42 = "42  ");
  test (sprintf "%04u" 42 = "0042");
  test (sprintf "%+u" 42 = "42");
  test (sprintf "% u" 42 = "42");
  test (sprintf "%#u" 42 = "42");
  test (sprintf "%4u" 42 = "  42");
  test (sprintf "%*u" 4 42 = "  42");
  test (sprintf "%*u" (-4) 42 = "42  ");

  say "\nu negative\n%!";
  begin match Sys.int_size with
  | 31 ->
     test (sprintf "%u" (-1) = "2147483647");
  | 63 ->
     test (sprintf "%u" (-1) = "9223372036854775807");
  | _ -> test false
  end;

  say "\nx positive\n%!";
  test (sprintf "%x" 42 = "2a");
  test (sprintf "%-4x" 42 = "2a  ");
  test (sprintf "%04x" 42 = "002a");
  test (sprintf "%+x" 42 = "2a");
  test (sprintf "% x" 42 = "2a");
  test (sprintf "%#x" 42 = "0x2a");
  test (sprintf "%4x" 42 = "  2a");
  test (sprintf "%*x" 5 42 = "   2a");
  test (sprintf "%*x" (-5) 42 = "2a   ");
  test (sprintf "%#*x" 5 42 = " 0x2a");
  test (sprintf "%#*x" (-5) 42 = "0x2a ");
  test (sprintf "%#-*x" 5 42 = "0x2a ");
  test (sprintf "%-0+ #*x" 5 42 = "0x2a ");

  say "\nx negative\n%!";
  begin match Sys.int_size with
  | 31 ->
     test (sprintf "%x" (-42) = "7fffffd6");
  | 63 ->
     test (sprintf "%x" (-42) = "7fffffffffffffd6");
  | _ -> test false
  end;

  say "\nX positive\n%!";
  test (sprintf "%X" 42 = "2A");
  test (sprintf "%-4X" 42 = "2A  ");
  test (sprintf "%04X" 42 = "002A");
  test (sprintf "%+X" 42 = "2A");
  test (sprintf "% X" 42 = "2A");
  test (sprintf "%#X" 42 = "0X2A");
  test (sprintf "%4X" 42 = "  2A");
  test (sprintf "%*X" 5 42 = "   2A");
  test (sprintf "%-0+ #*X" 5 42 = "0X2A ");

  say "\nx negative\n%!";
  begin match Sys.int_size with
  | 31 ->
     test (sprintf "%X" (-42) = "7FFFFFD6");
  | 63 ->
     test (sprintf "%X" (-42) = "7FFFFFFFFFFFFFD6");
  | _ -> test false
  end;

  say "\no positive\n%!";
  test (sprintf "%o" 42 = "52");
  test (sprintf "%-4o" 42 = "52  ");
  test (sprintf "%04o" 42 = "0052");
  test (sprintf "%+o" 42 = "52");
  test (sprintf "% o" 42 = "52");
  test (sprintf "%#o" 42 = "052");
  test (sprintf "%4o" 42 = "  52");
  test (sprintf "%*o" 5 42 = "   52");
  test (sprintf "%-0+ #*o" 5 42 = "052  ");

  say "\no negative\n%!";
  begin match Sys.int_size with
  | 31 ->
     test (sprintf "%o" (-42) = "17777777726");
  | 63 ->
     test (sprintf "%o" (-42) = "777777777777777777726");
  | _ -> test false
  end;

  say "\ns\n%!";
  test (sprintf "%s" "foo" = "foo");
  test (sprintf "%-5s" "foo" = "foo  ");
  test (sprintf "%05s" "foo" = "  foo");
  test (sprintf "%+s" "foo" = "foo");
  test (sprintf "% s" "foo" = "foo");
  test (sprintf "%#s" "foo" = "foo");
  test (sprintf "%5s" "foo" = "  foo");
  test (sprintf "%1s" "foo" = "foo");
  test (sprintf "%*s" 6 "foo" = "   foo");
  test (sprintf "%*s" (-6) "foo" = "foo   ");
  test (sprintf "%*s" 2 "foo" = "foo");
  test (sprintf "%-0+ #5s" "foo" = "foo  ");
  test (sprintf "%s@@" "foo" = "foo@");
  test (sprintf "%s@@inria.fr" "foo" = "foo@inria.fr");
  test (sprintf "%s@@%s" "foo" "inria.fr" = "foo@inria.fr");

  say "\nS\n%!";
  test (sprintf "%S" "fo\"o" = "\"fo\\\"o\"");
  test (sprintf "%-7S" "foo" = "\"foo\"  ");
(*  test (sprintf "%07S" "foo" = "  \"foo\""); *)
  (* %S is incompatible with '0' *)
  test (sprintf "%+S" "foo" = "\"foo\"");
  test (sprintf "% S" "foo" = "\"foo\"");
  test (sprintf "%#S" "foo" = "\"foo\"");
  test (sprintf "%7S" "foo" = "  \"foo\"");
  test (sprintf "%1S" "foo" = "\"foo\"");
  test (sprintf "%*S" 8 "foo" = "   \"foo\"");
  test (sprintf "%*S" (-8) "foo" = "\"foo\"   ");
  test (sprintf "%*S" 2 "foo" = "\"foo\"");
(*  test (sprintf "%-0+ #5S" "foo" = "\"foo\"  ");  padding not done *)
  (* %S is incompatible with '0','+' and ' ' *)
  test (sprintf "%S@@" "foo" = "\"foo\"@");
  test (sprintf "%S@@inria.fr" "foo" = "\"foo\"@inria.fr");
  test (sprintf "%S@@%S" "foo" "inria.fr" = "\"foo\"@\"inria.fr\"");

  say "\nc\n%!";
  test (sprintf "%c" 'c' = "c");
(*  test (sprintf "%-4c" 'c' = "c   ");    padding not done *)
(*  test (sprintf "%04c" 'c' = "   c");    padding not done *)
  test (sprintf "%+c" 'c' = "c");
  test (sprintf "% c" 'c' = "c");
  test (sprintf "%#c" 'c' = "c");
(*  test (sprintf "%4c" 'c' = "   c");     padding not done *)
(*  test (sprintf "%*c" 2 'c' = " c");     padding not done *)
(*  test (sprintf "%-0+ #4c" 'c' = "c   ");  padding not done *)

  say "\nC\n%!";
  test (sprintf "%C" 'c' = "'c'");
  test (sprintf "%C" '\'' = "'\\''");
(*  test (sprintf "%-4C" 'c' = "c   ");    padding not done *)
(*  test (sprintf "%04C" 'c' = "   c");    padding not done *)
  test (sprintf "%+C" 'c' = "'c'");
  test (sprintf "% C" 'c' = "'c'");
  test (sprintf "%#C" 'c' = "'c'");
(*  test (sprintf "%4C" 'c' = "   c");     padding not done *)
(*  test (sprintf "%*C" 2 'c' = " c");     padding not done *)
(*  test (sprintf "%-0+ #4C" 'c' = "c   ");  padding not done *)

  say "\nf\n%!";
  test (sprintf "%f" (-42.42) = "-42.420000");
  test (sprintf "%-13f" (-42.42) = "-42.420000   ");
  test (sprintf "%013f" (-42.42) = "-00042.420000");
  test (sprintf "%+f" 42.42 = "+42.420000");
  test (sprintf "% f" 42.42 = " 42.420000");
  test (sprintf "%#f" 42.42 = "42.420000");
  test (sprintf "%13f" 42.42 = "    42.420000");
  test (sprintf "%*f" 12 42.42 = "   42.420000");
  test (sprintf "%-0+ #12f" 42.42 = "+42.420000  ");
  test (sprintf "%.3f" (-42.42) = "-42.420");
  test (sprintf "%-13.3f" (-42.42) = "-42.420      ");
  test (sprintf "%013.3f" (-42.42) = "-00000042.420");
  test (sprintf "%+.3f" 42.42 = "+42.420");
  test (sprintf "% .3f" 42.42 = " 42.420");
  test (sprintf "%#.3f" 42.42 = "42.420");
  test (sprintf "%13.3f" 42.42 = "       42.420");
  test (sprintf "%*.*f" 12 3 42.42 = "      42.420");
  test (sprintf "%-0+ #12.3f" 42.42 = "+42.420     ");

  (* Under Windows (mingw and maybe also MSVC), the stdlib uses three
     digits for the exponent instead of the two used by Linux and BSD.
     Check that the two strings are equal, except that there may be an
     extra zero, and if there is one, there may be a missing space or
     zero. All in the first string relative to the second. *)
  let ( =* ) s1 s2 =
    let ss1 = s1 ^ "$" in
    let ss2 = s2 ^ "$" in
    let rec loop i1 i2 extra missing =
      if i1 = String.length ss1 && i2 = String.length ss2 then begin
        if extra then true else not missing
      end else if i1 = String.length ss1 || i2 = String.length ss2 then
        false
      else begin
        match ss1.[i1], ss2.[i2] with
        | x, y when x = y -> loop (i1+1) (i2+1) extra missing
        | '0', _ when not extra -> loop (i1+1) i2 true missing
        | _, (' '|'0') when not missing -> loop i1 (i2+1) extra true
        | _, _ -> false
      end
    in
    loop 0 0 false false
  in

  say "\nF\n%!";
  test (sprintf "%F" 42.42 = "42.42");
  test (sprintf "%F" 42.42e42 =* "4.242e+43");
  test (sprintf "%F" 42.00 = "42.");
  test (sprintf "%F" 0.042 = "0.042");
  test (sprintf "%4F" 3. = "  3.");
  test (sprintf "%-4F" 3. = "3.  ");
  test (sprintf "%04F" 3. = "003.");
(* plus-padding unsupported
  test (sprintf "%+4F" 3. = " +3.");
*)
(* no precision
  test (sprintf "%.3F" 42.42 = "42.420");
  test (sprintf "%12.3F" 42.42e42 = "   4.242e+43");
  test (sprintf "%.3F" 42.00 = "42.000");
  test (sprintf "%.3F" 0.0042 = "0.004");
*)

  say "\nh\n%!";
  test (sprintf "%+h" (+0.) = "+0x0p+0");
  test (sprintf "%+h" (-0.) = "-0x0p+0");
  test (sprintf "%+h" (+1.) = "+0x1p+0");
  test (sprintf "%+h" (-1.) = "-0x1p+0");
  test (sprintf "%+h" (+1024.) = "+0x1p+10");
  test (sprintf "%+h" (-1024.) = "-0x1p+10");
  test (sprintf "%h" 0x123.456 = "0x1.23456p+8");
  test (sprintf "%h" 0x123456789ABCDE. = "0x1.23456789abcdep+52");
  test (sprintf "%h" epsilon_float = "0x1p-52");
  test (sprintf "%h" nan = "nan");
  test (sprintf "%h" infinity = "infinity");
  test (sprintf "%h" neg_infinity = "-infinity");
  test (sprintf "%h" (4. *. atan 1.) = "0x1.921fb54442d18p+1");

  say "\nH\n%!";
  test (sprintf "%+H" (+0.) = "+0X0P+0");
  test (sprintf "%+H" (-0.) = "-0X0P+0");
  test (sprintf "%+H" (+1.) = "+0X1P+0");
  test (sprintf "%+H" (-1.) = "-0X1P+0");
  test (sprintf "%+H" (+1024.) = "+0X1P+10");
  test (sprintf "%+H" (-1024.) = "-0X1P+10");
  test (sprintf "%H" 0X123.456 = "0X1.23456P+8");
  test (sprintf "%H" 0X123456789ABCDE. = "0X1.23456789ABCDEP+52");
  test (sprintf "%H" epsilon_float = "0X1P-52");
  test (sprintf "%H" nan = "NAN");
  test (sprintf "%H" infinity = "INFINITY");
  test (sprintf "%H" neg_infinity = "-INFINITY");
  test (sprintf "%H" (4. *. atan 1.) = "0X1.921FB54442D18P+1");

  say "\ne\n%!";
  test (sprintf "%e" (-42.42) =* "-4.242000e+01");
  test (sprintf "%-15e" (-42.42) =* "-4.242000e+01  ");
  test (sprintf "%015e" (-42.42) =* "-004.242000e+01");
  test (sprintf "%+e" 42.42 =* "+4.242000e+01");
  test (sprintf "% e" 42.42 =* " 4.242000e+01");
  test (sprintf "%#e" 42.42 =* "4.242000e+01");
  test (sprintf "%15e" 42.42 =* "   4.242000e+01");
  test (sprintf "%*e" 14 42.42 =* "  4.242000e+01");
  test (sprintf "%-0+ #14e" 42.42 =* "+4.242000e+01 ");
  test (sprintf "%.3e" (-42.42) =* "-4.242e+01");
  test (sprintf "%-15.3e" (-42.42) =* "-4.242e+01     ");
  test (sprintf "%015.3e" (-42.42) =* "-000004.242e+01");
  test (sprintf "%+.3e" 42.42 =* "+4.242e+01");
  test (sprintf "% .3e" 42.42 =* " 4.242e+01");
  test (sprintf "%#.3e" 42.42 =* "4.242e+01");
  test (sprintf "%15.3e" 42.42 =* "      4.242e+01");
  test (sprintf "%*.*e" 11 3 42.42 =* "  4.242e+01");
  test (sprintf "%-0+ #14.3e" 42.42 =* "+4.242e+01    ");

  say "\nE\n%!";
  test (sprintf "%E" (-42.42) =* "-4.242000E+01");
  test (sprintf "%-15E" (-42.42) =* "-4.242000E+01  ");
  test (sprintf "%015E" (-42.42) =* "-004.242000E+01");
  test (sprintf "%+E" 42.42 =* "+4.242000E+01");
  test (sprintf "% E" 42.42 =* " 4.242000E+01");
  test (sprintf "%#E" 42.42 =* "4.242000E+01");
  test (sprintf "%15E" 42.42 =* "   4.242000E+01");
  test (sprintf "%*E" 14 42.42 =* "  4.242000E+01");
  test (sprintf "%-0+ #14E" 42.42 =* "+4.242000E+01 ");
  test (sprintf "%.3E" (-42.42) =* "-4.242E+01");
  test (sprintf "%-15.3E" (-42.42) =* "-4.242E+01     ");
  test (sprintf "%015.3E" (-42.42) =* "-000004.242E+01");
  test (sprintf "%+.3E" 42.42 =* "+4.242E+01");
  test (sprintf "% .3E" 42.42 =* " 4.242E+01");
  test (sprintf "%#.3E" 42.42 =* "4.242E+01");
  test (sprintf "%15.3E" 42.42 =* "      4.242E+01");
  test (sprintf "%*.*E" 11 3 42.42 =* "  4.242E+01");
  test (sprintf "%-0+ #14.3E" 42.42 =* "+4.242E+01    ");

(* %g gives strange results that correspond to neither %f nor %e
  say "\ng\n%!";
  test (sprintf "%g" (-42.42) = "-42.42000");
  test (sprintf "%-15g" (-42.42) = "-42.42000      ");
  test (sprintf "%015g" (-42.42) = "-00000042.42000");
  test (sprintf "%+g" 42.42 = "+42.42000");
  test (sprintf "% g" 42.42 = " 42.42000");
  test (sprintf "%#g" 42.42 = "42.42000");
  test (sprintf "%15g" 42.42 = "       42.42000");
  test (sprintf "%*g" 14 42.42 = "      42.42000");
  test (sprintf "%-0+ #14g" 42.42 = "+42.42000     ");
  test (sprintf "%.3g" (-42.42) = "-42.420");
*)

(* Same for %G
  say "\nG\n%!";
*)

  say "\nB\n%!";
  test (sprintf "%B" true = "true");
  test (sprintf "%B" false = "false");
 (* test (sprintf "%8B" false = "   false"); *)
  (* padding not done *)

  say "\nld/li positive\n%!";
  test (sprintf "%ld/%li" 42l 43l = "42/43");
  test (sprintf "%-4ld/%-5li" 42l 43l = "42  /43   ");
  test (sprintf "%04ld/%05li" 42l 43l = "0042/00043");
  test (sprintf "%+ld/%+li" 42l 43l = "+42/+43");
  test (sprintf "% ld/% li" 42l 43l = " 42/ 43");
  test (sprintf "%#ld/%#li" 42l 43l = "42/43");
  test (sprintf "%4ld/%5li" 42l 43l = "  42/   43");
  test (sprintf "%*ld/%*li" 4 42l 5 43l = "  42/   43");
  test (sprintf "%-0+#4ld/%-0 #5li" 42l 43l = "+42 / 43  ");

  say "\nld/li negative\n%!";
  test (sprintf "%ld/%li" (-42l) (-43l) = "-42/-43");
  test (sprintf "%-4ld/%-5li" (-42l) (-43l) = "-42 /-43  ");
  test (sprintf "%04ld/%05li" (-42l) (-43l) = "-042/-0043");
  test (sprintf "%+ld/%+li" (-42l) (-43l) = "-42/-43");
  test (sprintf "% ld/% li" (-42l) (-43l) = "-42/-43");
  test (sprintf "%#ld/%#li" (-42l) (-43l) = "-42/-43");
  test (sprintf "%4ld/%5li" (-42l) (-43l) = " -42/  -43");
  test (sprintf "%*ld/%*li" 4 (-42l) 5 (-43l) = " -42/  -43");
  test (sprintf "%-0+ #4ld/%-0+ #5li" (-42l) (-43l) = "-42 /-43  ");

  say "\nlu positive\n%!";
  test (sprintf "%lu" 42l = "42");
  test (sprintf "%-4lu" 42l = "42  ");
  test (sprintf "%04lu" 42l = "0042");
  test (sprintf "%+lu" 42l = "42");
  test (sprintf "% lu" 42l = "42");
  test (sprintf "%#lu" 42l = "42");
  test (sprintf "%4lu" 42l = "  42");
  test (sprintf "%*lu" 4 42l = "  42");
  test (sprintf "%-0+ #6ld" 42l = "+42   ");

  say "\nlu negative\n%!";
  test (sprintf "%lu" (-1l) = "4294967295");

  say "\nlx positive\n%!";
  test (sprintf "%lx" 42l = "2a");
  test (sprintf "%-4lx" 42l = "2a  ");
  test (sprintf "%04lx" 42l = "002a");
  test (sprintf "%+lx" 42l = "2a");
  test (sprintf "% lx" 42l = "2a");
  test (sprintf "%#lx" 42l = "0x2a");
  test (sprintf "%4lx" 42l = "  2a");
  test (sprintf "%*lx" 5 42l = "   2a");
  test (sprintf "%-0+ #*lx" 5 42l = "0x2a ");

  say "\nlx negative\n%!";
  test (sprintf "%lx" (-42l) = "ffffffd6");

  say "\nlX positive\n%!";
  test (sprintf "%lX" 42l = "2A");
  test (sprintf "%-4lX" 42l = "2A  ");
  test (sprintf "%04lX" 42l = "002A");
  test (sprintf "%+lX" 42l = "2A");
  test (sprintf "% lX" 42l = "2A");
  test (sprintf "%#lX" 42l = "0X2A");
  test (sprintf "%4lX" 42l = "  2A");
  test (sprintf "%*lX" 5 42l = "   2A");
  test (sprintf "%-0+ #*lX" 5 42l = "0X2A ");

  say "\nlx negative\n%!";
  test (sprintf "%lX" (-42l) = "FFFFFFD6");

  say "\nlo positive\n%!";
  test (sprintf "%lo" 42l = "52");
  test (sprintf "%-4lo" 42l = "52  ");
  test (sprintf "%04lo" 42l = "0052");
  test (sprintf "%+lo" 42l = "52");
  test (sprintf "% lo" 42l = "52");
  test (sprintf "%#lo" 42l = "052");
  test (sprintf "%4lo" 42l = "  52");
  test (sprintf "%*lo" 5 42l = "   52");
  test (sprintf "%-0+ #*lo" 5 42l = "052  ");

  say "\nlo negative\n%!";
  test (sprintf "%lo" (-42l) = "37777777726");

  (* Nativeint not tested: looks like too much work, and anyway it should
     work like Int32 or Int64. *)

  say "\nLd/Li positive\n%!";
  test (sprintf "%Ld/%Li" 42L 43L = "42/43");
  test (sprintf "%-4Ld/%-5Li" 42L 43L = "42  /43   ");
  test (sprintf "%04Ld/%05Li" 42L 43L = "0042/00043");
  test (sprintf "%+Ld/%+Li" 42L 43L = "+42/+43");
  test (sprintf "% Ld/% Li" 42L 43L = " 42/ 43");
  test (sprintf "%#Ld/%#Li" 42L 43L = "42/43");
  test (sprintf "%4Ld/%5Li" 42L 43L = "  42/   43");
  test (sprintf "%*Ld/%*Li" 4 42L 5 43L = "  42/   43");
  test (sprintf "%-0+#4Ld/%-0 #5Li" 42L 43L = "+42 / 43  ");

  say "\nLd/Li negative\n%!";
  test (sprintf "%Ld/%Li" (-42L) (-43L) = "-42/-43");
  test (sprintf "%-4Ld/%-5Li" (-42L) (-43L) = "-42 /-43  ");
  test (sprintf "%04Ld/%05Li" (-42L) (-43L) = "-042/-0043");
  test (sprintf "%+Ld/%+Li" (-42L) (-43L) = "-42/-43");
  test (sprintf "% Ld/% Li" (-42L) (-43L) = "-42/-43");
  test (sprintf "%#Ld/%#Li" (-42L) (-43L) = "-42/-43");
  test (sprintf "%4Ld/%5Li" (-42L) (-43L) = " -42/  -43");
  test (sprintf "%*Ld/%*Li" 4 (-42L) 5 (-43L) = " -42/  -43");
  test (sprintf "%-0+ #4Ld/%-0+ #5Li" (-42L) (-43L) = "-42 /-43  ");

  say "\nLu positive\n%!";
  test (sprintf "%Lu" 42L = "42");
  test (sprintf "%-4Lu" 42L = "42  ");
  test (sprintf "%04Lu" 42L = "0042");
  test (sprintf "%+Lu" 42L = "42");
  test (sprintf "% Lu" 42L = "42");
  test (sprintf "%#Lu" 42L = "42");
  test (sprintf "%4Lu" 42L = "  42");
  test (sprintf "%*Lu" 4 42L = "  42");
  test (sprintf "%-0+ #6Ld" 42L = "+42   ");

  say "\nLu negative\n%!";
  test (sprintf "%Lu" (-1L) = "18446744073709551615");

  say "\nLx positive\n%!";
  test (sprintf "%Lx" 42L = "2a");
  test (sprintf "%-4Lx" 42L = "2a  ");
  test (sprintf "%04Lx" 42L = "002a");
  test (sprintf "%+Lx" 42L = "2a");
  test (sprintf "% Lx" 42L = "2a");
  test (sprintf "%#Lx" 42L = "0x2a");
  test (sprintf "%4Lx" 42L = "  2a");
  test (sprintf "%*Lx" 5 42L = "   2a");
  test (sprintf "%-0+ #*Lx" 5 42L = "0x2a ");

  say "\nLx negative\n%!";
  test (sprintf "%Lx" (-42L) = "ffffffffffffffd6");

  say "\nLX positive\n%!";
  test (sprintf "%LX" 42L = "2A");
  test (sprintf "%-4LX" 42L = "2A  ");
  test (sprintf "%04LX" 42L = "002A");
  test (sprintf "%+LX" 42L = "2A");
  test (sprintf "% LX" 42L = "2A");
  test (sprintf "%#LX" 42L = "0X2A");
  test (sprintf "%4LX" 42L = "  2A");
  test (sprintf "%*LX" 5 42L = "   2A");
  test (sprintf "%-0+ #*LX" 5 42L = "0X2A ");

  say "\nLx negative\n%!";
  test (sprintf "%LX" (-42L) = "FFFFFFFFFFFFFFD6");

  say "\nLo positive\n%!";
  test (sprintf "%Lo" 42L = "52");
  test (sprintf "%-4Lo" 42L = "52  ");
  test (sprintf "%04Lo" 42L = "0052");
  test (sprintf "%+Lo" 42L = "52");
  test (sprintf "% Lo" 42L = "52");
  test (sprintf "%#Lo" 42L = "052");
  test (sprintf "%4Lo" 42L = "  52");
  test (sprintf "%*Lo" 5 42L = "   52");
  test (sprintf "%-0+ #*Lo" 5 42L = "052  ");

  say "\nLo negative\n%!";
  test (sprintf "%Lo" (-42L) = "1777777777777777777726");

  say "\na\n%!";
  let x = ref () in
  let f () y = if y == x then "ok" else "wrong" in
  test (sprintf "%a" f x = "ok");

  say "\nt\n%!";
  let f () = "ok" in
  test (sprintf "%t" f = "ok");

(* %{ fmt %} prints the signature of [fmt], i.e. a canonical representation
   of the conversions present in [fmt].
*)
  say "\n{...%%}\n%!";
  let f = format_of_string "%f/%s" in
  test (sprintf "%{%f%s%}" f = "%f%s");

  say "\n(...%%)\n%!";
  let f = format_of_string "%d/foo/%s" in
  test (sprintf "%(%d%s%)" f 42 "bar" = "42/foo/bar");

  say "\n! %% @ , and constants\n%!";
  test (sprintf "%!" = "");
  test (sprintf "%%" = "%");
  test (sprintf "%@" = "@");
  test (sprintf "%," = "");
  test (sprintf "@@" = "@");
  test (sprintf "@@@@" = "@@");
  test (sprintf "@@%%" = "@%");

  say "\nDelayed printf\n%!";
  let t1 = dprintf "%i - %s" 1 "bar" in
  test (asprintf "foo %t" t1 = "foo 1 - bar");
  let t2 = dprintf "%a@]" (pp_print_list pp_print_int) [1 ; 2 ; 3] in
  test (asprintf "foo @[<v>%t@,%s" t2 "bar" = "foo 1\n    2\n    3\nbar");
  test (asprintf "%t @[<h>%t" t1 t2 = "1 - bar 123");

  say "\n\nHeterogeneous List tests\n\n%!";

  say "d/i positive\n%!";
  test (lasprintf "%d/%i" [42; 43] = "42/43");
  test (lasprintf "%-4d/%-5i" [42; 43] = "42  /43   ");
  test (lasprintf "%04d/%05i" [42; 43] = "0042/00043");
  test (lasprintf "%+d/%+i" [42; 43] = "+42/+43");
  test (lasprintf "% d/% i" [42; 43] = " 42/ 43");
  test (lasprintf "%#d/%#i" [42; 43] = "42/43");
  test (lasprintf "%4d/%5i" [42; 43] = "  42/   43");
  test (lasprintf "%*d" [-4; 42] = "42  ");
  test (lasprintf "%*d/%*i" [4; 42; 5; 43] = "  42/   43");
  test (lasprintf "%-0+#4d/%-0 #5i" [42; 43] = "+42 / 43  ");

  say "\nd/i negative\n%!";
  test (lasprintf "%d/%i" [-42; -43] = "-42/-43");
  test (lasprintf "%-4d/%-5i" [-42; -43] = "-42 /-43  ");
  test (lasprintf "%04d/%05i" [-42; -43] = "-042/-0043");
  test (lasprintf "%+d/%+i" [-42; -43] = "-42/-43");
  test (lasprintf "% d/% i" [-42; -43] = "-42/-43");
  test (lasprintf "%#d/%#i" [-42; -43] = "-42/-43");
  test (lasprintf "%4d/%5i" [-42; -43] = " -42/  -43");
  test (lasprintf "%*d" [-4; -42] = "-42 ");
  test (lasprintf "%*d/%*i" [4; -42; 5; -43] = " -42/  -43");
  test (lasprintf "%-0+ #4d/%-0+ #5i" [-42; -43] = "-42 /-43  ");

  say "\nu positive\n%!";
  test (lasprintf "%u" [42] = "42");
  test (lasprintf "%-4u" [42] = "42  ");
  test (lasprintf "%04u" [42] = "0042");
  test (lasprintf "%+u" [42] = "42");
  test (lasprintf "% u" [42] = "42");
  test (lasprintf "%#u" [42] = "42");
  test (lasprintf "%4u" [42] = "  42");
  test (lasprintf "%*u" [4; 42] = "  42");
  test (lasprintf "%*u" [-4; 42] = "42  ");

  say "\nu negative\n%!";
  begin match Sys.int_size with
  | 31 ->
    test (lasprintf "%u" [-1] = "2147483647");
  | 63 ->
    test (lasprintf "%u" [-1] = "9223372036854775807");
  | _ -> test false
  end;

  say "\nx positive\n%!";
  test (lasprintf "%x" [42] = "2a");
  test (lasprintf "%-4x" [42] = "2a  ");
  test (lasprintf "%04x" [42] = "002a");
  test (lasprintf "%+x" [42] = "2a");
  test (lasprintf "% x" [42] = "2a");
  test (lasprintf "%#x" [42] = "0x2a");
  test (lasprintf "%4x" [42] = "  2a");
  test (lasprintf "%*x" [5; 42] = "   2a");
  test (lasprintf "%*x" [-5; 42] = "2a   ");
  test (lasprintf "%#*x" [5; 42] = " 0x2a");
  test (lasprintf "%#*x" [-5; 42] = "0x2a ");
  test (lasprintf "%#-*x" [5; 42] = "0x2a ");
  test (lasprintf "%-0+ #*x" [5; 42] = "0x2a ");

  say "\nx negative\n%!";
  begin match Sys.int_size with
  | 31 ->
    test (lasprintf "%x" [-42] = "7fffffd6");
  | 63 ->
    test (lasprintf "%x" [-42] = "7fffffffffffffd6");
  | _ -> test false
  end;

  say "\nX positive\n%!";
  test (lasprintf "%X" [42] = "2A");
  test (lasprintf "%-4X" [42] = "2A  ");
  test (lasprintf "%04X" [42] = "002A");
  test (lasprintf "%+X" [42] = "2A");
  test (lasprintf "% X" [42] = "2A");
  test (lasprintf "%#X" [42] = "0X2A");
  test (lasprintf "%4X" [42] = "  2A");
  test (lasprintf "%*X" [5; 42] = "   2A");
  test (lasprintf "%-0+ #*X" [5; 42] = "0X2A ");

  say "\nx negative\n%!";
  begin match Sys.int_size with
  | 31 ->
    test (lasprintf "%X" [-42] = "7FFFFFD6");
  | 63 ->
    test (lasprintf "%X" [-42] = "7FFFFFFFFFFFFFD6");
  | _ -> test false
  end;

  say "\no positive\n%!";
  test (lasprintf "%o" [42] = "52");
  test (lasprintf "%-4o" [42] = "52  ");
  test (lasprintf "%04o" [42] = "0052");
  test (lasprintf "%+o" [42] = "52");
  test (lasprintf "% o" [42] = "52");
  test (lasprintf "%#o" [42] = "052");
  test (lasprintf "%4o" [42] = "  52");
  test (lasprintf "%*o" [5; 42] = "   52");
  test (lasprintf "%-0+ #*o" [5; 42] = "052  ");

  say "\no negative\n%!";
  begin match Sys.int_size with
  | 31 ->
    test (lasprintf "%o" [-42] = "17777777726");
  | 63 ->
    test (lasprintf "%o" [-42] = "777777777777777777726");
  | _ -> test false
  end;

  say "\ns\n%!";
  test (lasprintf "%s" ["foo"] = "foo");
  test (lasprintf "%-5s" ["foo"] = "foo  ");
  test (lasprintf "%05s" ["foo"] = "  foo");
  test (lasprintf "%+s" ["foo"] = "foo");
  test (lasprintf "% s" ["foo"] = "foo");
  test (lasprintf "%#s" ["foo"] = "foo");
  test (lasprintf "%5s" ["foo"] = "  foo");
  test (lasprintf "%1s" ["foo"] = "foo");
  test (lasprintf "%*s" [6; "foo"] = "   foo");
  test (lasprintf "%*s" [-6; "foo"] = "foo   ");
  test (lasprintf "%*s" [2; "foo"] = "foo");
  test (lasprintf "%-0+ #5s" ["foo"] = "foo  ");
  test (lasprintf "%s@@" ["foo"] = "foo@");
  test (lasprintf "%s@@inria.fr" ["foo"] = "foo@inria.fr");
  test (lasprintf "%s@@%s" ["foo"; "inria.fr"] = "foo@inria.fr");

  say "\nS\n%!";
  test (lasprintf "%S" ["fo\"o"] = "\"fo\\\"o\"");
  test (lasprintf "%-7S" ["foo"] = "\"foo\"  ");
  (* test (lasprintf "%07S" ["foo"] = "  \"foo\""); *)
  (* %S is incompatible with '0' *)
  test (lasprintf "%+S" ["foo"] = "\"foo\"");
  test (lasprintf "% S" ["foo"] = "\"foo\"");
  test (lasprintf "%#S" ["foo"] = "\"foo\"");
  test (lasprintf "%7S" ["foo"] = "  \"foo\"");
  test (lasprintf "%1S" ["foo"] = "\"foo\"");
  test (lasprintf "%*S" [8; "foo"] = "   \"foo\"");
  test (lasprintf "%*S" [-8; "foo"] = "\"foo\"   ");
  test (lasprintf "%*S" [2; "foo"] = "\"foo\"");
  (* test (lasprintf "%-0+ #5S" ["foo"] = "\"foo\"  ");  padding not done *)
  (* %S is incompatible with '0','+' and ' ' *)
  test (lasprintf "%S@@" ["foo"] = "\"foo\"@");
  test (lasprintf "%S@@inria.fr" ["foo"] = "\"foo\"@inria.fr");
  test (lasprintf "%S@@%S" ["foo"; "inria.fr"] = "\"foo\"@\"inria.fr\"");

  say "\nc\n%!";
  test (lasprintf "%c" ['c'] = "c");
  (* test (lasprintf "%-4c" ['c'] = "c   ");    padding not done *)
  (* test (lasprintf "%04c" ['c'] = "   c");    padding not done *)
  test (lasprintf "%+c" ['c'] = "c");
  test (lasprintf "% c" ['c'] = "c");
  test (lasprintf "%#c" ['c'] = "c");
  (* test (lasprintf "%4c" ['c'] = "   c");     padding not done *)
  (* test (lasprintf "%*c" [2; 'c'] = " c");     padding not done *)
  (* test (lasprintf "%-0+ #4c" ['c'] = "c   ");  padding not done *)

  say "\nC\n%!";
  test (lasprintf "%C" ['c'] = "'c'");
  test (lasprintf "%C" ['\''] = "'\\''");
  (* test (lasprintf "%-4C" ['c'] = "c   ");    padding not done *)
  (* test (lasprintf "%04C" ['c'] = "   c");    padding not done *)
  test (lasprintf "%+C" ['c'] = "'c'");
  test (lasprintf "% C" ['c'] = "'c'");
  test (lasprintf "%#C" ['c'] = "'c'");
  (* test (lasprintf "%4C" ['c'] = "   c");     padding not done *)
  (* test (lasprintf "%*C" [2; 'c'] = " c");     padding not done *)
  (* test (lasprintf "%-0+ #4C" ['c'] = "c   ");  padding not done *)

  say "\nf\n%!";
  test (lasprintf "%f" [-42.42] = "-42.420000");
  test (lasprintf "%-13f" [-42.42] = "-42.420000   ");
  test (lasprintf "%013f" [-42.42] = "-00042.420000");
  test (lasprintf "%+f" [42.42] = "+42.420000");
  test (lasprintf "% f" [42.42] = " 42.420000");
  test (lasprintf "%#f" [42.42] = "42.420000");
  test (lasprintf "%13f" [42.42] = "    42.420000");
  test (lasprintf "%*f" [12; 42.42] = "   42.420000");
  test (lasprintf "%-0+ #12f" [42.42] = "+42.420000  ");
  test (lasprintf "%.3f" [-42.42] = "-42.420");
  test (lasprintf "%-13.3f" [-42.42] = "-42.420      ");
  test (lasprintf "%013.3f" [-42.42] = "-00000042.420");
  test (lasprintf "%+.3f" [42.42] = "+42.420");
  test (lasprintf "% .3f" [42.42] = " 42.420");
  test (lasprintf "%#.3f" [42.42] = "42.420");
  test (lasprintf "%13.3f" [42.42] = "       42.420");
  test (lasprintf "%*.*f" [12; 3; 42.42] = "      42.420");
  test (lasprintf "%-0+ #12.3f" [42.42] = "+42.420     ");

  say "\nF\n%!";
  test (lasprintf "%F" [42.42] = "42.42");
  test (lasprintf "%F" [42.42e42] =* "4.242e+43");
  test (lasprintf "%F" [42.00] = "42.");
  test (lasprintf "%F" [0.042] = "0.042");
  test (lasprintf "%4F" [3.] = "  3.");
  test (lasprintf "%-4F" [3.] = "3.  ");
  test (lasprintf "%04F" [3.] = "003.");
(* plus-padding unsupported
  test (sprintf "%+4F" 3. = " +3.");
*)
(* no precision
  test (sprintf "%.3F" 42.42 = "42.420");
  test (sprintf "%12.3F" 42.42e42 = "   4.242e+43");
  test (sprintf "%.3F" 42.00 = "42.000");
  test (sprintf "%.3F" 0.0042 = "0.004");
*)

  say "\nh\n%!";
  test (lasprintf "%+h" [+0.] = "+0x0p+0");
  test (lasprintf "%+h" [-0.] = "-0x0p+0");
  test (lasprintf "%+h" [+1.] = "+0x1p+0");
  test (lasprintf "%+h" [-1.] = "-0x1p+0");
  test (lasprintf "%+h" [+1024.] = "+0x1p+10");
  test (lasprintf "%+h" [-1024.] = "-0x1p+10");
  test (lasprintf "%h" [0x123.456] = "0x1.23456p+8");
  test (lasprintf "%h" [0x123456789ABCDE.] = "0x1.23456789abcdep+52");
  test (lasprintf "%h" [epsilon_float] = "0x1p-52");
  test (lasprintf "%h" [nan] = "nan");
  test (lasprintf "%h" [infinity] = "infinity");
  test (lasprintf "%h" [neg_infinity] = "-infinity");
  test (lasprintf "%h" [4. *. atan 1.] = "0x1.921fb54442d18p+1");

  say "\nH\n%!";
  test (lasprintf "%+H" [+0.] = "+0X0P+0");
  test (lasprintf "%+H" [-0.] = "-0X0P+0");
  test (lasprintf "%+H" [+1.] = "+0X1P+0");
  test (lasprintf "%+H" [-1.] = "-0X1P+0");
  test (lasprintf "%+H" [+1024.] = "+0X1P+10");
  test (lasprintf "%+H" [-1024.] = "-0X1P+10");
  test (lasprintf "%H" [0X123.456] = "0X1.23456P+8");
  test (lasprintf "%H" [0X123456789ABCDE.] = "0X1.23456789ABCDEP+52");
  test (lasprintf "%H" [epsilon_float] = "0X1P-52");
  test (lasprintf "%H" [nan] = "NAN");
  test (lasprintf "%H" [infinity] = "INFINITY");
  test (lasprintf "%H" [neg_infinity] = "-INFINITY");
  test (lasprintf "%H" [4. *. atan 1.] = "0X1.921FB54442D18P+1");

  say "\ne\n%!";
  test (lasprintf "%e" [-42.42] =* "-4.242000e+01");
  test (lasprintf "%-15e" [-42.42] =* "-4.242000e+01  ");
  test (lasprintf "%015e" [-42.42] =* "-004.242000e+01");
  test (lasprintf "%+e" [42.42] =* "+4.242000e+01");
  test (lasprintf "% e" [42.42] =* " 4.242000e+01");
  test (lasprintf "%#e" [42.42] =* "4.242000e+01");
  test (lasprintf "%15e" [42.42] =* "   4.242000e+01");
  test (lasprintf "%*e" [14; 42.42] =* "  4.242000e+01");
  test (lasprintf "%-0+ #14e" [42.42] =* "+4.242000e+01 ");
  test (lasprintf "%.3e" [-42.42] =* "-4.242e+01");
  test (lasprintf "%-15.3e" [-42.42] =* "-4.242e+01     ");
  test (lasprintf "%015.3e" [-42.42] =* "-000004.242e+01");
  test (lasprintf "%+.3e" [42.42] =* "+4.242e+01");
  test (lasprintf "% .3e" [42.42] =* " 4.242e+01");
  test (lasprintf "%#.3e" [42.42] =* "4.242e+01");
  test (lasprintf "%15.3e" [42.42] =* "      4.242e+01");
  test (lasprintf "%*.*e" [11; 3; 42.42] =* "  4.242e+01");
  test (lasprintf "%-0+ #14.3e" [42.42] =* "+4.242e+01    ");

  say "\nE\n%!";
  test (lasprintf "%E" [-42.42] =* "-4.242000E+01");
  test (lasprintf "%-15E" [-42.42] =* "-4.242000E+01  ");
  test (lasprintf "%015E" [-42.42] =* "-004.242000E+01");
  test (lasprintf "%+E" [42.42] =* "+4.242000E+01");
  test (lasprintf "% E" [42.42] =* " 4.242000E+01");
  test (lasprintf "%#E" [42.42] =* "4.242000E+01");
  test (lasprintf "%15E" [42.42] =* "   4.242000E+01");
  test (lasprintf "%*E" [14; 42.42] =* "  4.242000E+01");
  test (lasprintf "%-0+ #14E" [42.42] =* "+4.242000E+01 ");
  test (lasprintf "%.3E" [-42.42] =* "-4.242E+01");
  test (lasprintf "%-15.3E" [-42.42] =* "-4.242E+01     ");
  test (lasprintf "%015.3E" [-42.42] =* "-000004.242E+01");
  test (lasprintf "%+.3E" [42.42] =* "+4.242E+01");
  test (lasprintf "% .3E" [42.42] =* " 4.242E+01");
  test (lasprintf "%#.3E" [42.42] =* "4.242E+01");
  test (lasprintf "%15.3E" [42.42] =* "      4.242E+01");
  test (lasprintf "%*.*E" [11; 3; 42.42] =* "  4.242E+01");
  test (lasprintf "%-0+ #14.3E" [42.42] =* "+4.242E+01    ");

(* %g gives strange results that correspond to neither %f nor %e
  say "\ng\n%!";
  test (sprintf "%g" (-42.42) = "-42.42000");
  test (sprintf "%-15g" (-42.42) = "-42.42000      ");
  test (sprintf "%015g" (-42.42) = "-00000042.42000");
  test (sprintf "%+g" 42.42 = "+42.42000");
  test (sprintf "% g" 42.42 = " 42.42000");
  test (sprintf "%#g" 42.42 = "42.42000");
  test (sprintf "%15g" 42.42 = "       42.42000");
  test (sprintf "%*g" 14 42.42 = "      42.42000");
  test (sprintf "%-0+ #14g" 42.42 = "+42.42000     ");
  test (sprintf "%.3g" (-42.42) = "-42.420");
*)

(* Same for %G
  say "\nG\n%!";
*)

  say "\nB\n%!";
  test (lasprintf "%B" [true] = "true");
  test (lasprintf "%B" [false] = "false");
  (* test (lasprintf "%8B" [false] = "   false"); *)
  (* padding not done *)

  say "\nld/li positive\n%!";
  test (lasprintf "%ld/%li" [42l; 43l] = "42/43");
  test (lasprintf "%-4ld/%-5li" [42l; 43l] = "42  /43   ");
  test (lasprintf "%04ld/%05li" [42l; 43l] = "0042/00043");
  test (lasprintf "%+ld/%+li" [42l; 43l] = "+42/+43");
  test (lasprintf "% ld/% li" [42l; 43l] = " 42/ 43");
  test (lasprintf "%#ld/%#li" [42l; 43l] = "42/43");
  test (lasprintf "%4ld/%5li" [42l; 43l] = "  42/   43");
  test (lasprintf "%*ld/%*li" [4; 42l; 5; 43l] = "  42/   43");
  test (lasprintf "%-0+#4ld/%-0 #5li" [42l; 43l] = "+42 / 43  ");

  say "\nld/li negative\n%!";
  test (lasprintf "%ld/%li" [-42l; -43l] = "-42/-43");
  test (lasprintf "%-4ld/%-5li" [-42l; -43l] = "-42 /-43  ");
  test (lasprintf "%04ld/%05li" [-42l; -43l] = "-042/-0043");
  test (lasprintf "%+ld/%+li" [-42l; -43l] = "-42/-43");
  test (lasprintf "% ld/% li" [-42l; -43l] = "-42/-43");
  test (lasprintf "%#ld/%#li" [-42l; -43l] = "-42/-43");
  test (lasprintf "%4ld/%5li" [-42l; -43l] = " -42/  -43");
  test (lasprintf "%*ld/%*li" [4; -42l; 5; -43l] = " -42/  -43");
  test (lasprintf "%-0+ #4ld/%-0+ #5li" [-42l; -43l] = "-42 /-43  ");

  say "\nlu positive\n%!";
  test (lasprintf "%lu" [42l] = "42");
  test (lasprintf "%-4lu" [42l] = "42  ");
  test (lasprintf "%04lu" [42l] = "0042");
  test (lasprintf "%+lu" [42l] = "42");
  test (lasprintf "% lu" [42l] = "42");
  test (lasprintf "%#lu" [42l] = "42");
  test (lasprintf "%4lu" [42l] = "  42");
  test (lasprintf "%*lu" [4; 42l] = "  42");
  test (lasprintf "%-0+ #6ld" [42l] = "+42   ");

  say "\nlu negative\n%!";
  test (lasprintf "%lu" [-1l] = "4294967295");

  say "\nlx positive\n%!";
  test (lasprintf "%lx" [42l] = "2a");
  test (lasprintf "%-4lx" [42l] = "2a  ");
  test (lasprintf "%04lx" [42l] = "002a");
  test (lasprintf "%+lx" [42l] = "2a");
  test (lasprintf "% lx" [42l] = "2a");
  test (lasprintf "%#lx" [42l] = "0x2a");
  test (lasprintf "%4lx" [42l] = "  2a");
  test (lasprintf "%*lx" [5; 42l] = "   2a");
  test (lasprintf "%-0+ #*lx" [5; 42l] = "0x2a ");

  say "\nlx negative\n%!";
  test (lasprintf "%lx" [-42l] = "ffffffd6");

  say "\nlX positive\n%!";
  test (lasprintf "%lX" [42l] = "2A");
  test (lasprintf "%-4lX" [42l] = "2A  ");
  test (lasprintf "%04lX" [42l] = "002A");
  test (lasprintf "%+lX" [42l] = "2A");
  test (lasprintf "% lX" [42l] = "2A");
  test (lasprintf "%#lX" [42l] = "0X2A");
  test (lasprintf "%4lX" [42l] = "  2A");
  test (lasprintf "%*lX" [5; 42l] = "   2A");
  test (lasprintf "%-0+ #*lX" [5; 42l] = "0X2A ");

  say "\nlX negative\n%!";
  test (lasprintf "%lX" [-42l] = "FFFFFFD6");

  say "\nlo positive\n%!";
  test (lasprintf "%lo" [42l] = "52");
  test (lasprintf "%-4lo" [42l] = "52  ");
  test (lasprintf "%04lo" [42l] = "0052");
  test (lasprintf "%+lo" [42l] = "52");
  test (lasprintf "% lo" [42l] = "52");
  test (lasprintf "%#lo" [42l] = "052");
  test (lasprintf "%4lo" [42l] = "  52");
  test (lasprintf "%*lo" [5; 42l] = "   52");
  test (lasprintf "%-0+ #*lo" [5; 42l] = "052  ");

  say "\nlo negative\n%!";
  test (lasprintf "%lo" [-42l] = "37777777726");

  (* Nativeint not tested: looks like too much work, and anyway it should
     work like Int32 or Int64. *)

  say "\nLd/Li positive\n%!";
  test (lasprintf "%Ld/%Li" [42L; 43L] = "42/43");
  test (lasprintf "%-4Ld/%-5Li" [42L; 43L] = "42  /43   ");
  test (lasprintf "%04Ld/%05Li" [42L; 43L] = "0042/00043");
  test (lasprintf "%+Ld/%+Li" [42L; 43L] = "+42/+43");
  test (lasprintf "% Ld/% Li" [42L; 43L] = " 42/ 43");
  test (lasprintf "%#Ld/%#Li" [42L; 43L] = "42/43");
  test (lasprintf "%4Ld/%5Li" [42L; 43L] = "  42/   43");
  test (lasprintf "%*Ld/%*Li" [4; 42L; 5; 43L] = "  42/   43");
  test (lasprintf "%-0+#4Ld/%-0 #5Li" [42L; 43L] = "+42 / 43  ");

  say "\nLd/Li negative\n%!";
  test (lasprintf "%Ld/%Li" [-42L; -43L] = "-42/-43");
  test (lasprintf "%-4Ld/%-5Li" [-42L; -43L] = "-42 /-43  ");
  test (lasprintf "%04Ld/%05Li" [-42L; -43L] = "-042/-0043");
  test (lasprintf "%+Ld/%+Li" [-42L; -43L] = "-42/-43");
  test (lasprintf "% Ld/% Li" [-42L; -43L] = "-42/-43");
  test (lasprintf "%#Ld/%#Li" [-42L; -43L] = "-42/-43");
  test (lasprintf "%4Ld/%5Li" [-42L; -43L] = " -42/  -43");
  test (lasprintf "%*Ld/%*Li" [4; -42L; 5; -43L] = " -42/  -43");
  test (lasprintf "%-0+ #4Ld/%-0+ #5Li" [-42L; -43L] = "-42 /-43  ");

  say "\nLu positive\n%!";
  test (lasprintf "%Lu" [42L] = "42");
  test (lasprintf "%-4Lu" [42L] = "42  ");
  test (lasprintf "%04Lu" [42L] = "0042");
  test (lasprintf "%+Lu" [42L] = "42");
  test (lasprintf "% Lu" [42L] = "42");
  test (lasprintf "%#Lu" [42L] = "42");
  test (lasprintf "%4Lu" [42L] = "  42");
  test (lasprintf "%*Lu" [4; 42L] = "  42");
  test (lasprintf "%-0+ #6Ld" [42L] = "+42   ");

  say "\nLu negative\n%!";
  test (lasprintf "%Lu" [-1L] = "18446744073709551615");

  say "\nLx positive\n%!";
  test (lasprintf "%Lx" [42L] = "2a");
  test (lasprintf "%-4Lx" [42L] = "2a  ");
  test (lasprintf "%04Lx" [42L] = "002a");
  test (lasprintf "%+Lx" [42L] = "2a");
  test (lasprintf "% Lx" [42L] = "2a");
  test (lasprintf "%#Lx" [42L] = "0x2a");
  test (lasprintf "%4Lx" [42L] = "  2a");
  test (lasprintf "%*Lx" [5; 42L] = "   2a");
  test (lasprintf "%-0+ #*Lx" [5; 42L] = "0x2a ");

  say "\nLx negative\n%!";
  test (lasprintf "%Lx" [-42L] = "ffffffffffffffd6");

  say "\nLX positive\n%!";
  test (lasprintf "%LX" [42L] = "2A");
  test (lasprintf "%-4LX" [42L] = "2A  ");
  test (lasprintf "%04LX" [42L] = "002A");
  test (lasprintf "%+LX" [42L] = "2A");
  test (lasprintf "% LX" [42L] = "2A");
  test (lasprintf "%#LX" [42L] = "0X2A");
  test (lasprintf "%4LX" [42L] = "  2A");
  test (lasprintf "%*LX" [5; 42L] = "   2A");
  test (lasprintf "%-0+ #*LX" [5; 42L] = "0X2A ");

  say "\nLX negative\n%!";
  test (lasprintf "%LX" [-42L] = "FFFFFFFFFFFFFFD6");

  say "\nLo positive\n%!";
  test (lasprintf "%Lo" [42L] = "52");
  test (lasprintf "%-4Lo" [42L] = "52  ");
  test (lasprintf "%04Lo" [42L] = "0052");
  test (lasprintf "%+Lo" [42L] = "52");
  test (lasprintf "% Lo" [42L] = "52");
  test (lasprintf "%#Lo" [42L] = "052");
  test (lasprintf "%4Lo" [42L] = "  52");
  test (lasprintf "%*Lo" [5; 42L] = "   52");
  test (lasprintf "%-0+ #*Lo" [5; 42L] = "052  ");

  say "\nLo negative\n%!";
  test (lasprintf "%Lo" [-42L] = "1777777777777777777726");

  say "\na\n%!";
  let x = ref () in
  let f y ppf _ = 
    if y == !x then lfprintf ppf "ok" []
    else lfprintf ppf "wrong" []
  in
  test (lasprintf "%a" [f (); x] = "ok");

  say "\nt\n%!";
  let f ppf = lfprintf ppf "ok" [] in
  test (lasprintf "%t" [f] = "ok");

  (* %{ fmt %} prints the signature of [fmt], i.e. a canonical representation
    of the conversions present in [fmt].
  *)
  say "\n{...%%}\n%!";
  let f = format_of_string "%f/%s" in
  test (lasprintf "%{%f%s%}" [f] = "%f%s");

  say "\n(...%%)\n%!";
  let f = format_of_string "%d/foo/%s" in
  test (lasprintf "%(%d%s%)" [f; 42; "bar"] = "42/foo/bar");

  say "\n! %% @ , and constants\n%!";
  test (lasprintf "%!" [] = "");
  test (lasprintf "%%" [] = "%");
  test (lasprintf "%@" [] = "@");
  test (lasprintf "%," [] = "");
  test (lasprintf "@@" [] = "@");
  test (lasprintf "@@@@" [] = "@@");
  test (lasprintf "@@%%" [] = "@%");

  say "\nDelayed printf\n%!";
  let t1 fmt = lfprintf fmt "%i - %s" [1; "bar"] in
  test (lasprintf "foo %t" [t1] = "foo 1 - bar");
  let t2 fmt = lfprintf fmt "%a@]" [(pp_print_list pp_print_int); [1; 2; 3]] in
  test (lasprintf "foo @[<v>%t@,%s" [t2; "bar"] = "foo 1\n    2\n    3\nbar");
  test (lasprintf "%t @[<h>%t" [t1; t2] = "1 - bar 123");

  say "\nMiscellaneous tests\n%!";
  test (lasprintf "%d, %.2f, %s" [42; 3.14159; "ocaml"] = "42, 3.14, ocaml");

  let pp_custom fmt p = lfprintf fmt "x = %s, y = %d" [p.x; p.y] in
  let p = { x = "ocaml"; y = 42 } in
  test (lasprintf "%a" [pp_custom; p] = "x = ocaml, y = 42");

  test (lasprintf "%s, %d, %.1f, %a" ["world"; 7; 2.718; pp_custom; p] = "world, 7, 2.7, x = ocaml, y = 42");

  let pp_padded_list fmt l =
    lfprintf fmt "[%s]" [ List.map (fun x -> lasprintf "%04d" [ x ]) l |> String.concat "; " ] in
  test (lasprintf "%a" [pp_padded_list; [1; 23; 456]] = "[0001; 0023; 0456]");

  test (lasprintf "%a" [(Format.pp_print_list Format.pp_print_int); [1; 23; 456]] = "123\n456");

  let nested_format ppf = lfprintf ppf "%s, %d, %a" [ "final"; 42; pp_custom; p ] in
  test (lasprintf "%s -> %t" [ "outer"; nested_format ] = "outer -> final, 42, x = ocaml, y = 42");

  let delayed_format fmt = lfprintf fmt "%s, %d, %a" [ "final"; 42; pp_custom; p ] in
  test (lasprintf "%s, %d, %.3f, %a, %t"
    ["last"; 100; 3.141; pp_custom; p; delayed_format] 
    = "last, 100, 3.141, x = ocaml, y = 42, final, 42, x = ocaml, y = 42");

  let l : _ Arg_list.t = [ "ocaml"; 42; 3.14; 'c'; pp_custom; p ] in
  test (lasprintf "%s, %d, %.3f, %c, %a" l = "ocaml, 42, 3.140, c, x = ocaml, y = 42");

  (* emulating kfprintf *)
  let rf = ref false in
  let lvl = ref 0 in
  let er fmt args =
    lasprintf fmt
      ((fun ppf -> rf := true; lvl := !lvl + 1;
        lfprintf ppf "[test %d]: " [ !lvl ]) 
      :: args)
  in
  test (er "%t%d %s" [ 42; "ocaml" ] = "[test 1]: 42 ocaml");
  test (er "%t%.02f %i" [ 3.14; 42 ] = "[test 2]: 3.14 42");
  test (er "%t%S %s" [ "ocaml"; "ocaml" ] = "[test 3]: \"ocaml\" ocaml");
  test (!rf);

  say "\nend of tests\n%!";

with e ->
  say "unexpected exception: %s\n%!" (Printexc.to_string e);
  test false;
;;
