#!/bin/sh

# This somewhat hackily passes any extra words in CC to gfortran
# This means for a 32-bit build (configured with CC="gcc -m32" the -m32
# gets passed to gfortran)
shift 1
gfortran "$@"
