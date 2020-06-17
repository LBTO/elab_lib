#!/bin/sh

MAINFILENAME=/tmp/test_main.pro

rm -f test_*.diff
echo -n > $MAINFILENAME

for f in test_*.pro; do

   ff=`basename $f .pro`
   echo "run_test, '$ff'" >> $MAINFILENAME

done

echo "end" >> $MAINFILENAME

idl -e ".r $MAINFILENAME"
