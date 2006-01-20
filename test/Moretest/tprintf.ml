open Testing;;

open Printf;;

let test0 () =
  sprintf "%d\n" 1 = "1\n" &&
  sprintf "%.0f" 1.0 = "1" &&
  sprintf "%.0f." 1.7 = "2." &&
  sprintf "%.1f." 1.0 = "1.0." &&
  sprintf "%0.1f." 12.0 = "12.0." &&
  sprintf "%3.1f." 12.0 = "12.0." &&
  sprintf "%5.1f." 12.0 = " 12.0." &&
  sprintf "%10.1f." 12.0 = "      12.0." &&
  sprintf "%010.1f." 12.0 = "00000012.0." &&
  sprintf "% 10.1f." 12.0 = "      12.0." &&
  sprintf "%+10.1f." 12.0 = "     +12.0." &&
  sprintf "%+10.1f." (-12.0) = "     -12.0." &&

  sprintf "%010.5f." 12.0 = "0012.00000." &&
  sprintf "%010.0f." 12.0 = "0000000012." &&
  sprintf "% 10.0f." 12.0 = "        12." &&

  sprintf "%0.1f." 12.0 = "12.0." &&
  sprintf "%10.1f." 1.001 = "       1.0." &&
  sprintf "%05.1f." 1.001 = "001.0."
;;

test (test0 ());;

(* Padding integers (cf bug 3955).
   Testing * width specifications. *)
let test1 () =
  sprintf "%05d\n" 1 = "00001" &&
  sprintf "%*d\n" 5 1 = "    1" &&
  sprintf "%0*d\n" 5 1 = "    1";;

test (test1 ());;
