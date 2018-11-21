#!/bin/sh

grep -v 'REMOVE_ME for ' $1 | sed 's/Dynlink_/Compdynlink_/g' > $2
