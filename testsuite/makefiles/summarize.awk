#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#         Damien Doligez, projet Gallium, INRIA Rocquencourt            #
#                                                                       #
#   Copyright 2013 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

function check() {
    if (!in_test){
        printf("error at line %d: found test result without test start\n", NR);
        errored = 1;
    }
}

function clear() {
    curfile = "";
    in_test = 0;
}

function record_pass() {
    check();
    RESULTS[key] = "p";
    clear();
}

function record_skip() {
    check();
    RESULTS[key] = "s";
    clear();
}

# The output cares only if the test passes at least once so if a test passes,
# but then fails in a re-run triggered by a different test, ignore it.
function record_fail() {
    check();
    if (!(key in RESULTS)){
        RESULTS[key] = "f";
    }
    clear();
}

function record_unexp() {
    if (!(key in RESULTS)){
        RESULTS[key] = "e";
    }
    clear();
}

/Running tests from '[^']*'/ {
    if (in_test) record_unexp();
    match($0, /Running tests from '[^']*'/);
    curdir = substr($0, RSTART+20, RLENGTH - 21);
    key = curdir;
    DIRS[key] = key;
    curfile = "";
}

/ ... testing.* ... testing/ {
    printf("error at line %d: found two test results on the same line\n", NR);
    errored = 1;
}

/^ ... testing '[^']*'/ {
    if (in_test) record_unexp();
    match($0, /... testing '[^']*'/);
    curfile = substr($0, RSTART+13, RLENGTH-14);
    key = sprintf ("%s/%s", curdir, curfile);
    DIRS[key] = curdir;
    in_test = 1;
}

/^ ... testing with / {
    if (in_test) record_unexp();
    key = curdir;
    DIRS[key] = curdir;
    in_test = 1;
}

/=> passed/ {
    record_pass();
}

/=> skipped/ {
    record_skip();
}

/=> failed/ {
    record_fail();
}

/=> unexpected error/ {
    record_unexp();
}

/^re-ran / {
    if (in_test){
        printf("error at line %d: found re-ran inside a test\n", NR);
        errored = 1;
    }else{
        RERAN[substr($0, 8, length($0)-7)] += 1;
        ++ reran;
    }
}

# Not displaying "skipped" for the moment, as most of the skipped tests
# print nothing at all and are not counted.

END {
    if (errored){
        printf ("\n#### Some fatal error occurred during testing.\n\n");
        exit (3);
    }else{
        if (!retries){
            for (key in RESULTS){
                switch (RESULTS[key]) {
                case "p":
                    ++ passed;
                    break
                case "f":
                    ++ failed;
                    fail[failidx++] = key;
                    break
                case "e":
                    ++ unexped;
                    unexp[unexpidx++] = key;
                    break
                }
            }
            printf("\n");
            printf("Summary:\n");
            printf("  %3d test(s) passed\n", passed);
            printf("  %3d test(s) failed\n", failed);
            printf("  %3d unexpected error(s)\n", unexped);
            if (reran != 0){
                printf("  %3d test dir re-run(s)\n", reran);
            }
            if (failed != 0){
                printf("\nList of failed tests:\n");
                for (i=0; i < failed; i++) printf("    %s\n", fail[i]);
            }
            if (unexped != 0){
                printf("\nList of unexpected errors:\n");
                for (i=0; i < unexped; i++) printf("    %s\n", unexp[i]);
            }
            printf("\n");
            if (failed || unexped){
                printf("#### Some tests failed. Exiting with error status.\n\n");
                exit 4;
            }
        }else{
            for (key in RESULTS){
                if (RESULTS[key] == "f" || RESULTS[key] == "e"){
                    key = DIRS[key];
                    if (!(key in RERUNS)){
                        RERUNS[key] = 1;
                        if (RERAN[key] < max_retries){
                            printf("%s\n", key);
                        }
                    }
                }
            }
        }
    }
}
