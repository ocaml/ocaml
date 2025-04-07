echo -n $1
if [ -z $1 ]
then
  echo "Expected an argument" > ${ocamltest_response}
  exit ${TEST_FAIL}
else
  ${ocamlrun} ${ocamlsrcdir}/ocaml \
    -nostdlib -I ${ocamlsrcdir}/stdlib/ \
    -I +compilerlibs -I ${ocamlsrcdir}/utils/ \
    ${ocamlsrcdir}/compilerlibs/ocamlcommon.cma \
    ${test_source_directory}/encode_build_path_prefix_map.ml \
    $1 \
    > ${ocamltest_response} \
  && exit ${TEST_PASS} \
  || exit ${TEST_FAIL}
fi
