#!/bin/bash

BASE36=($(echo {0..9} {A..Z}));

while read arg
do
    for i in $(bc <<< "obase=36; $arg")
    do
        echo -n ${BASE36[$(( 10#$i ))]}
    done
done

echo
