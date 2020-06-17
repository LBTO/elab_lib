#!/usr/bin/env python

from __future__ import print_function

import os
import sys
import glob

mainfile = '/tmp/test_main.pro'

update=False
do_all=False
tests=[]

def usage():
   usage = '''
Usage:

run_test.py all                - run all tests
run_test.py test1 [test2]...   - run one or more tests
run_test.py update test1 [test2...]  - updates one or more reference output
'''

   print(usage)

for arg in sys.argv[1:]:

   if arg == 'update':
       update=True
   elif arg == 'all':
       do_all=True
   else:
       tests.append(arg)

if do_all and len(tests)>0:
    usage()
    sys.exit(1)

if do_all:
    tests = list(map(lambda x: x.replace('.pro',''), glob.glob('test_*.pro')))

if len(tests)==0:
    print('No tests to run')
    sys.exit(0)

if update:
    updatestr = ',/UPDATE'
else:
    updatestr = ''
    
status = 0
statements=[]
for i,test in enumerate(tests):

    if not os.path.exists(test+'.pro'):
       print('Error: test %s not found' % test)
       sys.exit(-1)

    try:
        os.unlink(test+'.diff')
    except OSError:
        pass

    print(test)
    prefix='(%d/%d)' % (i+1, len(tests))
    idlcmd="run_test, '%s', PREFIX='%s', FAILED=FAILED %s" % (test, prefix, updatestr)
    statements.append(idlcmd)

statements.append('if FAILED ne 0 then EXIT, status=FAILED')
statements.append('end')
open(mainfile, 'w').write('\n'.join(statements))
status += os.system('idl -e ".r %s"' % mainfile)

if status != 0:
    print('Test failed')
    sys.exit(1)

