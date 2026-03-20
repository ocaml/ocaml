#!/bin/sh

test_name="$1"

awk -f ${test_source_directory}/sanitize.awk \
    ${test_build_directory}/${test_name}.opt.output > ${test_build_directory}/${test_name}.opt.awk.output

mv ${test_build_directory}/${test_name}.opt.output ${test_build_directory}/${test_name}.opt.output.bak
cp ${test_build_directory}/${test_name}.opt.awk.output ${test_build_directory}/${test_name}.opt.output
