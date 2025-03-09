#!/bin/sh

test_source_directory="$1"
test_build_directory="$2"
ocamltest_response="$3"
test_name="$4"

awk -f ${test_source_directory}/sanitize.awk \
    ${test_build_directory}/${test_name}.opt.output > ${test_build_directory}/${test_name}.opt.awk.output

mv ${test_build_directory}/${test_name}.opt.output ${test_build_directory}/${test_name}.opt.output.bak
cp ${test_build_directory}/${test_name}.opt.awk.output ${test_build_directory}/${test_name}.opt.output
