#!/bin/sh

grep "wrong argument 'scheduling'" compiler-output.raw | grep "stop-after" | sed 's/^.*: wrong argument/wrong argument/'
