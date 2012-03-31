pro show_sigs, synsx, synsy, expsx, expsy
	common syncalib_common
	nsub = long(total(scobj->exp_slmask()))

	pupobj = ((scobj->imobj())->wfs_status())->pupils()
	indpup = (pupobj->indpup())
	fr_sz =80L		;pixels
	mypup = 0	;use this pupil info to remap signals
	cx  = (pupobj->cx())[mypup]
	cy  = (pupobj->cy())[mypup]
	rad = (pupobj->radius())[mypup]
	xr = [floor(cx-rad),ceil(cx+rad)]
	yr = [floor(cy-rad),ceil(cy+rad)]
	sl2d_w = xr[1]-xr[0]+1
	sl2d_h = yr[1]-yr[0]+1

	range = minmax([synsx,synsy,expsx,expsy])

	s2d = fltarr(fr_sz,fr_sz)
	s2d[indpup[*,mypup]] = expsx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = expsy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	sl_2d = [s2d_tmpA,s2d_tmpB]
	wset,0
	image_show, sl_2d, /as,/sh, title='experimental', min_value=range[0], max_val=range[1]

	s2d = fltarr(fr_sz,fr_sz)
	s2d[indpup[*,mypup]] = synsx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = synsy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	sl_2d = [s2d_tmpA,s2d_tmpB]
	wset,1
	image_show, sl_2d, /as,/sh, title='synthetic', min_value=range[0], max_val=range[1]

	s2d = fltarr(fr_sz,fr_sz)
	s2d[indpup[*,mypup]] = expsx - synsx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = expsy - synsy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	sl_2d = [s2d_tmpA,s2d_tmpB]
	WSET,2
	image_show, sl_2d, /as,/sh, title='difference'

	WSET,3
	plot, [synsx,synsy],yrange=minmax([synsx,synsy,expsx,expsy])
	oplot, [expsx,expsy], color=255L, psym=4

end

;--------------------- MINIMIZATION FUNCTION --------------------
;           FOR REGISTRATION BETWEEN ACTS AND SUBAPERTURES
;-----------------------------------------------------------------

FUNCTION flao_optogeom_errorfunc, misalignments
	; misalignments is a 3-dimensional vector containing:
	;  [anglerot, shift_x, shift_y]
	shiftval   = misalignments[1:2]	;in percent of pupil size
	anglerot = misalignments[0]

	common syncalib_common
	nsub = long(total(scobj->exp_slmask()))

	;check if matrix has already been created to avoid repetitions (typical of POWELL method)
	synmat_fname = 'synmat' + $
		'_angle'+strtrim(string(anglerot,format='(f6.2)'),2) + $
		'_shiftx'+strtrim(string(shiftval[0], format='(f7.3)'),2) + $
		'_shifty'+strtrim(string(shiftval[1], format='(f7.3)'),2) + $
		'.sav'
	synmat_fname = filepath(root=func_tempdir, synmat_fname)
	if file_test(synmat_fname) then restore, synmat_fname, verbose=funcverbose else begin
		matinter = scobj->syn_intmat(mymodes, anglerot, shiftval, verbose=funcverbose);, visu=funcvisu)
		save, matinter, filename=synmat_fname
	endelse

	IF total(size(matinter) NE size(mintlab)) then message,'The two matrices are not of same size.'
	error = sqrt(total( (double(mintlab)*1e-6 - double(matinter)*1e-6)^2d0)) ;/ trace(transpose(mintlab)##matinter)

	;show one mode at every iteration
	if funcvisu then begin
		mode = n_elements(mymodes)-1
	;	synsx = sqrt(ratio[mode])*reform(matinter[mode,0:nsub-1])
	;	synsy = sqrt(ratio[mode])*reform(matinter[mode,nsub:*])
		synsx = reform(matinter[mode,0:nsub-1])
		synsy = reform(matinter[mode,nsub:*])
		expsx = reform(mintlab[mode,0:nsub-1])
		expsy = reform(mintlab[mode,nsub:*])
		show_sigs, synsx, synsy, expsx, expsy
	endif

	print, 'anglerot is ', anglerot
	print, 'shift is ', shiftval
	print, 'error is ', error

	if funcvisu then wait,0.05

	return, error

END
