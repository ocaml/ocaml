(* TEST_BELOW *)

(* Int and float plus and minus operators *)
-1;;
+1;;
-1.0;;
+1.0;;
-.1.0;;
+.1.0;;

(* Prefix operator *)
~+2;;

(* With attributes attached to the argument *)
-(1[@foo]);;
+(1[@foo]);;
-(1.0[@foo]);;
+(1.0[@foo]);;
-.(1.0[@foo]);;
+.(1.0[@foo]);;

~+(2[@foo]);;

(* TEST
 flags = "-dparsetree -dno-locations -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
