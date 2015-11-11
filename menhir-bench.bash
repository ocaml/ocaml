#!/bin/bash

SOURCES=utils
INCLUDES="-I stdlib -I utils -I parsing -I typing"

test_parse() {
  for i in `seq 1 $1`
  do
      OCAML_PARSERS=$3 $2 $INCLUDES -stop-after parsing $SOURCES/*.{mli,ml}
  done
}

test_type() {
  for i in `seq 1 $1`
  do
      OCAML_PARSERS=$3 $2 $INCLUDES -c -i $SOURCES/*.{mli,ml} > /dev/null
  done
}

test_compile() {
  for i in `seq 1 $1`
  do
      OCAML_PARSERS=$3 $2 $INCLUDES $SOURCES/*.{mli,ml} > /dev/null
  done
}

# best of N
BESTOF=${BESTOF:-5}
bench() {
  for i in `seq 1 $BESTOF`
  do
      time ("$1" "$2" "$3" "$4")
  done 2>&1 | grep real | cut -f 2 | sort | head -n 1
}

# This beautiful function is an idea of Frédéric Bour
seconds_number() {
    echo $1 | sed 's/m/ * 60 +/' | sed 's/s//' | bc -l
}

ratio() {
    echo "scale=4; $(seconds_number $2) / $(seconds_number $1)" | bc
}


cat <<EOF

  This is a benchmark of Yacc- and Menhir-generated parsers used in
  the OCaml compiler. We compare performances for three usage modes:
  - parse only
  - parse and type-check the parsed code
  - full compile with the bytecode compiler

  (This script assumes that it is run from the source tree of an OCaml
   compiler that is already built (./ocamlc and ./ocamlc.opt are there).)

  The code used for this test are the files in the $SOURCES/ directory.
  We compare the performances of the native-compiled and
  bytecode-compiled (bytecode) compiler, ocamlc.opt and
  ocamlc. Because the Menhir runtime is implemented in OCaml
  (while the OCamlyacc runtime is C code), we expect the Menhir
  overhead to be larger in bytecode mode.

  Each test iterates its computation enough times to run in around one
  second on my machine. Each test is run $BESTOF times, with only the
  best time of five reported. You can change the number of "best of"
  runs by setting the BESTOF environment variable.

EOF

run_test_with_compiler() {
  echo -n "yacc:      "
  YACC=$(bench "$2" "$3" "$1" yacc)
  echo $YACC
  echo -n "menhir:    "
  MENHIR=$(bench "$2" "$3" "$1" menhir)
  echo $MENHIR
  echo "m/y ratio: $(ratio $YACC $MENHIR)"
  echo
}

run_test() {
  echo "# $1"
  echo "## native ($3 iterations)"
  (run_test_with_compiler "./ocamlc.opt" "$2" "$3")
  echo "## byte ($4 iterations)"
  (run_test_with_compiler "./runtime/ocamlrun ./ocamlc" "$2" "$4")
}

run_test "Parse only" test_parse 30 10

run_test "Parse and Type" test_type 4 1

run_test "Full compile" test_compile 3 1
