#!/bin/sh

exe() {
    echo
    echo "# $@"
    eval $@
}

OCAMLC="${ocamlrun} ${ocamlc_byte} -nostdlib -I ${ocamlsrcdir}/stdlib"

mkdir -p d

cat > d/a.ml <<EOF
let f () = print_endline "A"
EOF

cat > main.ml <<EOF
let () = A.f ()
EOF

# Check basic use

exe \${OCAMLC} -c d/a.ml

exe \${OCAMLC} -c main.ml

exe \${OCAMLC} -inc d/a.cmi -c main.ml

# Check precedence between -I and -inc

mkdir -p d2

cat > d2/a.ml <<EOF
let f () = 12
EOF

exe \${OCAMLC} -c d2/a.ml

exe \${OCAMLC} -inc d/a.cmi -I d2 -c main.ml

exe \${OCAMLC} -I d2 -inc d/a.cmi -c main.ml

# Check precedence between -inc

exe \${OCAMLC} -inc d/a.cmi -inc d2/a.cmi -c main.ml

exe \${OCAMLC} -inc d2/a.cmi -inc d/a.cmi -c main.ml

# Check handling of non-existent files

exe \${OCAMLC} -inc foo/a.cmi -c main.ml

exe \${OCAMLC} -inc foo/foo.cmi -inc d/a.cmi -c main.ml

# Check non-relative paths

exe \${OCAMLC} -inc "\$(pwd)/d/a.cmi" -c main.ml

# Check non-cmi case

cat > main.ml <<EOF
let _ = Longident.last (Longident.Lident "hola")
EOF

exe \${OCAMLC} -inc \${ocamlsrcdir}/parsing/longident.cmi main.ml

exe \${OCAMLC} \
    -inc \${ocamlsrcdir}/parsing/longident.cmi \
    -inc \${ocamlsrcdir}/compilerlibs/ocamlcommon.cma ocamlcommon.cma main.ml

exit 0
