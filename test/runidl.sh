
curdir=`pwd`
updir=`dirname $curdir`
IDL_STARTUP="" IDL_PATH="$updir:$updir/test/testlib:<IDL_DEFAULT>" ADOPT_MEAS=$updir/test/data idl
