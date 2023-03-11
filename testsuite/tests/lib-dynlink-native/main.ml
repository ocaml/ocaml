(* TEST

readonly_files = "a.ml api.ml b.ml bug.ml c.ml factorial.c pack_client.ml \
         packed1_client.ml packed1.ml plugin2.ml plugin4.ml plugin_ext.ml \
         plugin_high_arity.ml plugin.ml plugin.mli plugin_ref.ml \
         plugin_simple.ml plugin_thread.ml"
subdirectories = "sub"

* hassysthreads
include systhreads
include dynlink

** native-dynlink
libraries = "" (* We will add them manually where appropriated *)
*** setup-ocamlopt.byte-build-env
ocamlopt_default_flags = "" (* Removes the -ccopt -no-pie on ised on OpenBSD *)

**** ocamlopt.byte
module = "api.ml"
***** ocamlopt.byte
flags = "-opaque"
module = "plugin.mli"
****** ocamlopt.byte
flags = ""
module = "plugin.ml"
******* ocamlopt.byte
module= ""
flags = "-shared"
program = "plugin.so"
all_modules = "plugin.cmx"
******** script
script = "mv plugin.cmx plugin.cmx.bak"
********* ocamlopt.byte
flags = ""
module = "plugin2.ml"
********** script
script = "mv plugin.cmx.bak plugin.cmx"
*********** ocamlopt.byte
module= ""
flags = "-shared"
program = "plugin2.so"
all_modules = "plugin2.cmx"
************ ocamlopt.byte
flags = ""
module = "sub/plugin.ml"
************* ocamlopt.byte
module = ""
flags = "-shared"
program = "sub/plugin.so"
all_modules = "sub/plugin.cmx"
************** cd
cwd = "sub"
*************** ocamlopt.byte
module = "api.mli"
flags = "-opaque"
**************** ocamlopt.byte
flags = ""
module = "api.ml"
***************** script
script = "mv api.cmx api.cmx.bak"
****************** ocamlopt.byte
module = "plugin3.ml"
******************* script
script = "mv api.cmx.bak api.cmx"
******************** cd
cwd = ".."
********************* ocamlopt.byte
module = ""
flags = "-shared"
program = "sub/plugin3.so"
all_modules = "sub/plugin3.cmx"
********************** ocamlopt.byte
flags = ""
module = "plugin4.ml"
*********************** ocamlopt.byte
module = ""
flags = "-shared"
program = "plugin4.so"
all_modules = "plugin4.cmx"
************************ ocamlopt.byte
module = "packed1.ml"
flags = "-for-pack Mypack"
************************* ocamlopt.byte
flags = "-S -pack"
module = ""
program = "mypack.cmx"
all_modules = "packed1.cmx"
************************** ocamlopt.byte
program = "mypack.so"
flags = "-shared"
all_modules = "mypack.cmx"
*************************** ocamlopt.byte
program = "packed1.so"
flags = "-shared"
all_modules = "packed1.cmx"
**************************** ocamlopt.byte
flags = ""
module = "pack_client.ml"
***************************** ocamlopt.byte
module = ""
program = "pack_client.so"
flags = "-shared"
all_modules = "pack_client.cmx"
****************************** ocamlopt.byte
flags = ""
module = "plugin_ref.ml"
******************************* ocamlopt.byte
module = ""
program = "plugin_ref.so"
flags = "-shared"
all_modules = "plugin_ref.cmx"
******************************** ocamlopt.byte
flags = ""
module = "plugin_high_arity.ml"
********************************* ocamlopt.byte
module = ""
program = "plugin_high_arity.so"
flags = "-shared"
all_modules = "plugin_high_arity.cmx"
********************************** ocamlopt.byte
flags = "-ccopt ${shared_library_cflags}"
module = "factorial.c"
*********************************** ocamlopt.byte
flags = ""
module = "plugin_ext.ml"
************************************ ocamlopt.byte
module = ""
program = "plugin_ext.so"
flags = "-shared"
all_modules = "factorial.${objext} plugin_ext.cmx"
************************************* ocamlopt.byte
module = "plugin_simple.ml"
flags = ""
************************************** ocamlopt.byte
module = ""
program = "plugin_simple.so"
flags = "-shared"
all_modules = "plugin_simple.cmx"
************************************** ocamlopt.byte
module = "bug.ml"
flags = ""
*************************************** ocamlopt.byte
module = ""
program = "bug.so"
flags = "-shared"
all_modules = "bug.cmx"
*************************************** ocamlopt.byte
module = "plugin_thread.ml"
flags = ""
**************************************** ocamlopt.byte
module = ""
program = "plugin_thread.so"
flags = "-shared"
all_modules = "plugin_thread.cmx"
***************************************** ocamlopt.byte
program = "plugin4_unix.so"
all_modules = "unix.cmxa plugin4.cmx"
****************************************** ocamlopt.byte
flags = ""
compile_only = "true"
all_modules = "a.ml b.ml c.ml main.ml"
******************************************* ocamlopt.byte
module = ""
compile_only = "false"
flags = "-shared"
program = "a.so"
all_modules = "a.cmx"
******************************************** ocamlopt.byte
program = "b.so"
all_modules = "b.cmx"
********************************************* ocamlopt.byte
program = "c.so"
all_modules = "c.cmx"
********************************************** ocamlopt.byte
program = "mylib.cmxa"
flags = "-a"
all_modules = "plugin.cmx plugin2.cmx"
*********************************************** ocamlopt.byte
program = "mylib.so"
flags = "-shared -linkall"
all_modules = "mylib.cmxa"
************************************************ ocamlopt.byte
program = "${test_build_directory}/main.exe"
libraries = "unix threads dynlink"
flags = "-linkall"
all_modules = "api.cmx main.cmx"
(*
On OpenBSD, the compiler produces warnings like
/usr/bin/ld: warning: creating a DT_TEXTREL in a shared object.
So the compiler output is not empty on OpenBSD so an emptiness check
would fail on this platform.

We thus do not check compiler output. This was not done either before the
test was ported to ocamltest.
*)

************************************************* run
arguments = "plugin.so plugin2.so plugin_thread.so"
************************************************** check-program-output
*)

let () =
  Api.add_cb (fun () -> print_endline "Callback from main")

let ()  =
  Dynlink.allow_unsafe_modules true;
  for i = 1 to Array.length Sys.argv - 1 do
    let name = Sys.argv.(i) in
    Printf.printf "Loading %s\n" name; flush stdout;
    try
      if name.[0] = '-'
      then Dynlink.loadfile_private
        (String.sub name 1 (String.length name - 1))
      else Dynlink.loadfile name
    with
      | Dynlink.Error err ->
          Printf.printf "Dynlink error: %s\n"
            (Dynlink.error_message err)
      | exn ->
          Printf.printf "Error: %s\n" (Printexc.to_string exn)
  done;
  flush stdout;
  try
    let oc = open_out_bin "marshal.data" in
    Marshal.to_channel oc !Api.cbs [Marshal.Closures];
    close_out oc;
    let ic = open_in_bin "marshal.data" in
    let l = (Marshal.from_channel ic : (unit -> unit) list) in
    close_in ic;
    List.iter (fun f -> f()) l
  with Failure s ->
    Printf.printf "Failure: %s\n" s
