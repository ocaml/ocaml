#**************************************************************************
#*        ^o3                                                             *
#* ~/\_/\_|)                       OCaml                                  *
#* |/=_=\|                                                                *
#* "     "                                                                *
#*         Damien Doligez, projet Gallium, INRIA Rocquencourt             *
#*                                                                        *
#*   Copyright 2013 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Insertion sort in awk, because asort is a GNU extension. Lifted carefully
# with tongs out of the mawk manpage.
function isort(A, n) {
  for (i = 1; i < n; i++) {
    hold = A[j = i];
    while (A[j-1] > hold) {
      j--;
      A[j+1] = A[j];
    }
    A[j] = hold;
  }
}

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
    if (!(key in RESULTS)) ++nresults;
    RESULTS[key] = "p";
    delete SKIPPED[curdir];
    clear();
}

function record_skip() {
    check();
    if (!(key in RESULTS)) ++nresults;
    RESULTS[key] = "s";
    if (curdir in SKIPPED) SKIPPED[curdir] = 1;
    clear();
}

function record_na() {
    check();
    if (!(key in RESULTS)) ++nresults;
    RESULTS[key] = "n";
    if (curdir in SKIPPED) SKIPPED[curdir] = 1;
    clear();
}

function record_fail() {
    check();
    if (!(key in RESULTS)) ++nresults;
    RESULTS[key] = "f";
    delete SKIPPED[curdir];
    clear();
}

function record_unexp() {
    if (!(key in RESULTS) || RESULTS[key] == "s"){
        if (!(key in RESULTS)) ++nresults;
        RESULTS[key] = "e";
    }
    delete SKIPPED[curdir];
    clear();
}

/^> / {
    next;
}

/Running tests from '[^']*'/ {
    if (in_test) record_unexp();
    match($0, /Running tests from '[^']*'/);
    curdir = substr($0, RSTART+20, RLENGTH - 21);
    # Use SKIPPED[curdir] as a sentinel to detect no output
    SKIPPED[curdir] = 0;
    key = curdir;
    DIRS[key] = key;
    curfile = "";
}

/ ... testing.* ... testing/ {
    printf("error at line %d: found two test results on the same line\n", NR);
    errored = 1;
}

/^ ... testing '[^']*' with / {
    if (in_test) record_unexp();
    next;
}

/^ ... testing '[^']*'/ {
    if (in_test) record_unexp();
    match($0, /... testing '[^']*'/);
    curfile = substr($0, RSTART+13, RLENGTH-14);
    if (match($0, /\(wall clock: .*s\)/)){
        duration = substr($0, RSTART+13, RLENGTH-15);
        if (duration + 0.0 > 10.0)
          slow[slowcount++] = sprintf("%s: %s", curfile, duration);
    }
    key = sprintf ("%s/%s", curdir, curfile);
    DIRS[key] = curdir;
    in_test = 1;
}

/=> passed/ {
    record_pass();
}

/=> skipped/ {
    record_skip();
}

/=> n\/a/ {
    record_na();
}

/=> failed/ {
    record_fail();
}

/=> unexpected error/ {
    record_unexp();
}

/make[^:]*: \*\*\* \[[^]]*\] Error/ {
    errored = 1;
}

END {
    if (ENVIRON["GITHUB_ACTIONS"] == "true") start_group = "::group::";
    if (in_test) record_unexp();

    if (errored){
        printf ("\n#### Some fatal error occurred during testing.\n\n");
        exit (3);
    }else{
        for (key in SKIPPED){
            if (!SKIPPED[key]){
                ++ empty;
                blanks[emptyidx++] = key;
                delete SKIPPED[key];
            }
        }
        for (key in RESULTS){
            r = RESULTS[key];
            if (r == "p"){
                ++ passed;
            }else if (r == "f"){
                ++ failed;
                fail[failidx++] = key;
            }else if (r == "e"){
                ++ unexped;
                unexp[unexpidx++] = key;
            }else if (r == "s"){
                ++ skipped;
                curdir = DIRS[key];
                if (curdir in SKIPPED){
                    if (SKIPPED[curdir]){
                        SKIPPED[curdir] = 0;
                        skips[skipidx++] = curdir;
                    }
                }else{
                    skips[skipidx++] = key;
                }
            }else if (r == "n"){
                ++ ignored;
            }
        }
        isort(skips, skipidx);
        isort(blanks, empty);
        isort(fail, failed);
        isort(unexp, unexped);
        isort(slow, slowcount);
        printf("\n");
        if (skipped != 0){
            printf("\n%sList of skipped tests:\n", start_group);
            for (i=0; i < skipidx; i++) printf("    %s\n", skips[i]);
            if (ENVIRON["GITHUB_ACTIONS"] == "true") print "::endgroup::";
        }
        if (slowcount != 0){
            printf("\n\n%sTests taking longer than 10s:\n", start_group);
            for (i=0; i < slowcount; i++) printf("    %s\n", slow[i]);
            if (ENVIRON["GITHUB_ACTIONS"] == "true") print "::endgroup::";
        }
        if (empty != 0){
            printf("\nList of directories returning no results:\n");
            for (i=0; i < empty; i++) printf("    %s\n", blanks[i]);
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
        printf("Summary:\n");
        printf("  %4d tests passed\n", passed);
        printf("  %4d tests skipped\n", skipped);
        printf("  %4d tests failed\n", failed);
        printf("  %4d tests not started (parent test skipped or failed)\n",
               ignored);
        printf("  %4d unexpected errors\n", unexped);
        printf("  %4d tests considered", nresults);
        if (nresults != passed + skipped + ignored + failed + unexped){
            printf (" (totals don't add up??)");
        }
        printf ("\n");
        if (failed || unexped){
            printf("#### Something failed. Exiting with error status.\n\n");
            exit 4;
        }
    }
}
