#!/bin/sh
sed -e "s/^.*in file/File/" -e 's/ (inlined)//' -e "s/, raise.*//" | grep ^File
