#!/bin/sh
set -e
cd `dirname $0`

myfiglet() {
  figlet $@ | sed 's/  *$//'
}

if figlet ""; then
  BANNER=myfiglet
else
  echo "Install figlet to have a better output, press enter to continue with echo"
  read
  BANNER=echo
fi

HERE=`pwd`

$BANNER Test2
./test2/test.sh $@
$BANNER Test3
./test3/test.sh $@
$BANNER Test4
./test4/test.sh $@
$BANNER Test5
./test5/test.sh $@
$BANNER Test6
./test6/test.sh $@
$BANNER Test7
./test7/test.sh $@
$BANNER Test8
./test8/test.sh $@
$BANNER Test9
./test9/test.sh $@
$BANNER Test Virtual Targets
./test_virtual/test.sh $@
