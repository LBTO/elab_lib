pro corquad, fitsfile
; you must have $HOME/.corquad 
;a0      -3.343291E-4            
;a1      7.858745E-6
;a2      -7.819456E-8
;a3      3.687237E-10
;a4      -6.67760E-13
;kern0   -0.0025
;kern1   -0.0023
;kern2   -0.00065
    if file_test(filepath(root=getenv('HOME'), '.corquad')) eq 0 then $
        message, 'File $HOME/.corquad is missing. This is probably wrong. Look in corquad_pisces.pro'

    tmp = getenv('IDL_TMPDIR')
    thisfile = routine_info("corquad", /source)
    thisdir = file_dirname(thisfile.path)

    imas = readfits(fitsfile, hdr, /silent)
    imaout = imas
    naxis = long(aoget_fits_keyword(hdr, 'NAXIS'))
    nframes = (naxis eq 2) ? 1 : long(aoget_fits_keyword(hdr, 'NAXIS3'))
    for i=0, nframes-1 do begin
        writefits, filepath(root=tmp, 'pippo.fits'), imas[*,*,i], hdr
        ;spawn, '/home/aoacct/AO/current/idl/elab_lib/corquad/corquad /tmp/pippo.fits'
        spawn, string(format='(%"%s %s")', $
             filepath(root=thisdir, sub=['corquad'], 'corquad'),  filepath(root=tmp, 'pippo.fits'))
        cq = readfits( 'pippo.cq.fits', /silent)
        imaout[*,*,i]=cq
    endfor
    sxaddpar, hdr, 'CORQUAD', 'Y'
    writefits, fitsfile+'.cq', imaout, hdr
end


pro corquad_pisces, set
    fitsfiles = set->value('pisces.fname')
    for i=0, n_elements(fitsfiles)-1 do begin
        fname = fitsfiles[i]
        if strmid(fname, 1,/rev) eq 'cq' then continue
        print, 'corquadding '+fname
        corquad, fname
    endfor
end

