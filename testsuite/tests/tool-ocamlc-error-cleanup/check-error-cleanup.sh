if [ -f test.cmo ]
then
    exit ${TEST_FAIL}
else
    exit ${TEST_PASS}
fi
