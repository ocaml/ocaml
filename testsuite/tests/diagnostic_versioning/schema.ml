(* TEST
 {reference = "${ocamlsrcdir}/diagnostic_schemes/compiler_scheme.json";
 readonly_files = "${ocamlsrcdir}/diagnostic_schemes/compiler_scheme.json";
 output = "compiler_scheme.output";
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamldiaginfo -schema-format json -schema compiler -o ${output}";
 script;
 check-program-output;
}
{
 reference = "${ocamlsrcdir}/diagnostic_schemes/toplevel_scheme.json";
 readonly_files = "${ocamlsrcdir}/diagnostic_schemes/toplevel_scheme.json";
 output = "toplevel_scheme.output";
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamldiaginfo -schema-format json -schema toplevel -o ${output}";
 script;
 check-program-output;
}
{
 reference = "${ocamlsrcdir}/diagnostic_schemes/config_scheme.json";
 readonly_files = "${ocamlsrcdir}/diagnostic_schemes/config_scheme.json";
 output = "config_scheme.output";
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamldiaginfo -schema-format json -schema config -o ${output}";
 script;
 check-program-output;
}
*)
