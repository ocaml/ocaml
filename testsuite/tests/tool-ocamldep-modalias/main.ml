(* TEST
{
  readonly_files = "A.ml B.ml C.ml D.ml lib_impl.ml lib.mli Makefile.build Makefile.build2";
  SET sources = "A.ml B.ml C.ml D.ml";
  SET links = "LibA.ml LibB.ml LibC.ml LibD.ml";
  SET stdlib = "-nostdlib -I ${ocamlsrcdir}/stdlib";
  SET OCAMLC = "${ocamlrun} ${ocamlc_byte} ${stdlib}";
  SET OCAMLOPT = "${ocamlrun} ${ocamlopt_byte} ${stdlib}";
  {
    compiler_directory_suffix = ".depend.mk";
    compiler_output = "${test_build_directory}/depend.mk";
    setup-ocamlc.byte-build-env;

    src = "A.ml";
    dst = "LibA.ml";
    copy;

    src = "B.ml";
    dst = "LibB.ml";
    copy;

    src = "C.ml";
    dst = "LibC.ml";
    copy;

    src = "D.ml";
    dst = "LibD.ml";
    copy;

    src = "lib_impl.ml";
    dst = "lib.ml";
    copy;

    commandline = "-depend -as-map lib.ml lib.mli";
    ocamlc.byte;

    commandline = "-depend -map lib.ml -open Lib ${links}";
    ocamlc.byte;

    compiler_reference = "${test_source_directory}/depend.mk.reference";
    check-ocamlc.byte-output;

    hasunix;

    script = "rm -f ${links}";
    script;

    script = "${MAKE} -f Makefile.build byte";
    script;

    native-compiler;

    script = "${MAKE} -f Makefile.build opt";
    script;
  }{
    compiler_directory_suffix = ".depend.mk2";
    compiler_output = "${test_build_directory}/depend.mk2";
    setup-ocamlc.byte-build-env;

    src = "A.ml";
    dst = "LibA.ml";
    copy;

    src = "B.ml";
    dst = "LibB.ml";
    copy;

    src = "C.ml";
    dst = "LibC.ml";
    copy;

    src = "D.ml";
    dst = "LibD.ml";
    copy;

    commandline = "-depend -map lib.mli -open Lib ${links}";
    ocamlc.byte;

    compiler_reference = "${test_source_directory}/depend.mk2.reference";
    check-ocamlc.byte-output;

    hasunix;

    script = "rm -f ${links}";
    script;

    script = "${MAKE} -f Makefile.build2 byte";
    script;

    native-compiler;

    script = "${MAKE} -f Makefile.build2 opt";
    script;
  }{
    compiler_directory_suffix = ".depend.mod";
    setup-ocamlc.byte-build-env;

    src = "A.ml";
    dst = "LibA.ml";
    copy;

    src = "B.ml";
    dst = "LibB.ml";
    copy;

    src = "C.ml";
    dst = "LibC.ml";
    copy;

    src = "D.ml";
    dst = "LibD.ml";
    copy;

    src = "lib_impl.ml";
    dst = "lib.ml";
    copy;

    commandline = "-depend -as-map -modules lib.ml lib.mli";
    ocamlc.byte;

    commandline = "-depend -modules -map lib.ml -open Lib ${links}";
    ocamlc.byte;

    compiler_reference = "${test_source_directory}/depend.mod.reference";
    check-ocamlc.byte-output;
  }{
    compiler_directory_suffix = ".depend.mod2";
    setup-ocamlc.byte-build-env;

    src = "A.ml";
    dst = "LibA.ml";
    copy;

    src = "B.ml";
    dst = "LibB.ml";
    copy;

    src = "C.ml";
    dst = "LibC.ml";
    copy;

    src = "D.ml";
    dst = "LibD.ml";
    copy;

    commandline = "-depend -modules -map lib.mli ${links}";
    ocamlc.byte;

    compiler_reference = "${test_source_directory}/depend.mod2.reference";
    check-ocamlc.byte-output;
  }{
    compiler_directory_suffix = ".depend.mod3";
    setup-ocamlc.byte-build-env;

    src = "A.ml";
    dst = "LibA.ml";
    copy;

    src = "B.ml";
    dst = "LibB.ml";
    copy;

    src = "C.ml";
    dst = "LibC.ml";
    copy;

    src = "D.ml";
    dst = "LibD.ml";
    copy;

    commandline = "-depend -modules -as-map -map lib.mli -open Lib ${links}";
    ocamlc.byte;

    compiler_reference = "${test_source_directory}/depend.mod3.reference";
    check-ocamlc.byte-output;
  }
}
*)

open Lib

let () = Printf.printf "B.g 3 = %d\n%!" (B.g 3)
