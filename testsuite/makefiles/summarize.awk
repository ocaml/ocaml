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
    ++ passed;
    clear();
}

function record_fail() {
    check();
    ++ failed;
    fail[failidx++] = sprintf ("%s/%s", curdir, curfile);
    clear();
}

function record_unexp() {
    ++ unexped;
    unexp[unexpidx++] = sprintf ("%s/%s", curdir, curfile);
    clear();
}

/Running tests from '[^']*'/ {
    if (in_test) record_unexp();
    match($0, /Running tests from '[^']*'/);
    curdir = substr($0, RSTART+20, RLENGTH - 21);
    curfile = "";
}

/^ ... testing '[^']*'/ {
    if (in_test) record_unexp();
    match($0, /... testing '[^']*'/);
    curfile = substr($0, RSTART+13, RLENGTH-14);
    in_test = 1;
}

/^ ... testing with / {
    if (in_test) record_unexp();
    in_test = 1;
}

/=> passed/ {
    record_pass();
}

/=> failed/ {
    record_fail();
}

/=> unexpected error/ {
    record_unexp();
}

END {
    if (errored){
        exit (3);
    }else{
        printf("\n");
        printf("Summary:\n");
        printf("  %3d test(s) passed\n", passed);
        printf("  %3d test(s) failed\n", failed);
        printf("  %3d unexpected error(s)\n", unexped);
        if (failed != 0){
            printf("\nList of failed tests:\n");
            for (i in fail) printf("    %s\n", fail[i]);
        }
        if (unexped != 0){
            printf("\nList of unexpected errors:\n");
            for (i in unexp) printf("    %s\n", unexp[i]);
        }
        printf("\n");
        exit (failed || unexped ? 4 : 0);
    }
}
