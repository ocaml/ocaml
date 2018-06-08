#!/bin/sh
grep -oE '[a-zA-Z_]+\.ml(:[0-9]+)?|(line|characters) [0-9-]+'
