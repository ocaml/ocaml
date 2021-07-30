#!/bin/sh

grep "wrong argument 'typing'" compiler-output.raw | grep "save-ir-after" | sed 's/^.*: wrong argument/wrong argument/'
