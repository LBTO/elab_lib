
; Make sure that we compile the elab routines from the
; source directory and not the system libraries.

pro ao_test_init, right=right, left=left

    cd, '..',current=testDir
    cd, current=sourceDir
    elabdir = testDir+path_sep()+'elab_cache'
    phasemapdir = testDir+path_sep()+'phasemaps'
    ao_init, right=right,left=left,elabdir=elabdir,phasemapdir=phasemapdir,/noarcetri
    cd, sourceDir

end
