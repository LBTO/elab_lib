
ELAB_LIB TEST
-------------

IMPORTANT: It is better to delete the files in elab_cache before running the tests!

This directory contains a series of test scripts, together with representative test data.

A test is an IDL main file that executes some code. The resulting standard output is
compared with a reference output, and the test is passed if the two are equal.

Before comparing, the output is processed with the "sed" utility using the
commands found in "filters.sed", common to all tests. This is used to remove
spurious output that can change between runs and would make a test fail.

Each test is composed by a series of files, with the following naming scheme:

test_<name>.pro: (committed in SVN) an IDL main file containing the code to be run
test_<name>.ref: (committed in SVN) reference output of the IDL main file
test_<name>.out: actual output after a test run
test_<name>.err: standard error after a test run
test_<name>.orig: original ouptut after a test run, before sed filtering
test_<name>.diff: (in case of failed test) difference betwen actual and reference output

Some helper scripts are provided:

runidl.sh:  starts an IDL session with the path set to the elab_lib copy
            that contains the tests.

run_test.py: Python procedure to run tests and update reference outputs

run_test.pro: low-level IDL procedure for a single test


WHAT TODO IF THE DATA ARCHIVE IS UPDATED
----------------------------------------

The "data" directory containing the TNs used for test is not archived,
because it is too big (300+MB). If the data is updated:

1. Make sure that the new "data" directory has a new name, for example
   using a timestamp like data_YYYYMMDD
2. Edit "run_test.pro" and modify the data directory name near line 14.
3. Delete any previous data directories, if any


HOW TO IMPLEMENT A TEST
-----------------------

1. Write the main test file. Here is an example of a very simple main file,
   that we will call test_flao.pro:

ao_test_init
a = getaoelab('20180129_220007')
a->summary
end

2. Verify that the test runs correctly with the special IDL session:

./runidl.sh
IDL> .r test_flao

3. Produce the reference file:

./run_test.py update test_flao

This will create the test_flao.ref file with the test output.
Check that this file contains the correct output.

4. Check that the test passes

./run_test.py test_flao

5. Add and commit the pro and ref files:

git add test_flao.pro test_flao.ref
git commit test_flao.pro test_flao.ref
(git push)

RUN ALL TESTS
-------------

Before committing and/or installing, all tests should pass:

./run_test.py all

(1/10) test_modalplot: PASSED
(2/10) test_tel: PASSED
(3/10) test_control: PASSED
(4/10) test_ncpa: PASSED
(5/10) test_pupimage: PASSED
(6/10) test_computedelay: PASSED
(7/10) test_flao: PASSED
(8/10) test_gopt: PASSED
(9/10) test_soul: PASSED
(10/10) test_slopes2d: PASSED


TEST DATA
---------

20190525_250000: TN saved by gopt procedure
20190708_234139: TN saved by NCPA calibration procedure
