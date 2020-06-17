
pro run_test, testname, UPDATE_REF=UPDATE_REF, FAILED=FAILED, PREFIX=PREFIX

  if n_elements(FAILED) eq 0 then FAILED=0
  if n_elements(PREFIX) eq 0 then PREFIX=''

  cd, current=c
  updir = file_dirname(c)
  testlibdir = c+path_sep()+'testlib'
  startup='IDL_STARTUP=""'
  path='IDL_PATH="'+updir+':'+testlibdir+':'+!path+'"'

  ;;; Change this path when the data archive is updated
  meas='ADOPT_MEAS='+c+path_sep()+'data_20200617'

  idlfile = testname+'.pro'
  reffile = testname+'.ref'
  outfile = testname+'.out'
  errfile = testname+'.err'
  difffile = testname+'.diff'
  origfile = testname+'.orig'

  if keyword_set(UPDATE_REF) then outfile = reffile

  cmd = startup+' '+path+' '+meas+' idl -e ".r '+idlfile+'" > '+origfile+' 2> '+errfile
  spawn, cmd, exit_status=exit_status

  if exit_status ne 0 then begin
     print, testname+': execution failed'
     return
  endif

  cmd = 'sed -f filter.sed '+origfile+' > '+outfile
  spawn, cmd

  if keyword_set(UPDATE_REF) then return

  cmd = 'diff '+reffile+' '+outfile+' | tee '+difffile
  spawn, cmd, diffoutput
  a = strjoin(diffoutput)
  if a eq '' then begin
     print, prefix+' '+testname+': PASSED'
     file_delete, difffile
  endif else begin
     print, prefix+' '+testname+': FAILED'
     FAILED=1
  endelse
end
