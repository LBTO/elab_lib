function AOsyncalib::Init, im_tracknum, basis, syndata_dir=syndata_dir

	if n_params() ne 2 then begin
		message, "SINTAXIS: sc=obj_new('AOsyncalib', im_tracknum, basis)",/info
		return,0
	endif

	if not keyword_set(syndata_dir) then syndata_dir='.'
	self._syndata_dir = syndata_dir


;	Measured IM data:
;======================================================================================
	self._im_tracknum = im_tracknum
	im_fname = 'adsec_calib'+path_sep()+'M2C'+path_sep()+basis+path_sep()+'RECs'+path_sep()+'Intmat_'+im_tracknum+'.fits'
	im_obj = obj_new('AOintmat', im_fname)
	if obj_valid(im_obj) then self._expim_obj = im_obj else return,0

	modeshapes_fname = filepath(root=ao_phasemapdir(), 'KLmatrix_'+basis+'.sav')
	modeShapes = get_modes_shapes(modeShapes_fname)
	if obj_valid(modeShapes) then self._modeShapes = modeShapes else return,0

	slmask = im_obj->s2d_mask()
	slsz = size(slmask)
	slidx = where(slmask)
	slxr = minmax( slidx mod slsz[1])
	slyr = minmax( slidx  /  slsz[1])
	slmask = slmask[slxr[0]:slxr[1],slyr[0]:slyr[1]]
	self._exp_slmask = ptr_new(slmask)


;	synthetic IM data:
;=======================================================================================
	defsysv, '!FFTW_AVAILABLE', 0B
	if path_sep() eq '\' then defsysv, '!FFTW_ARCETRI', 0B else defsysv, '!FFTW_ARCETRI', 1B
	init_parallel,0

	binning = ((im_obj->wfs_status())->camera())->binning()
	print, 'binning mode #',strtrim(binning,2)

	amp_mod = float(round((im_obj->wfs_status())->modulation()))
	print, 'TT modulation radius (lambda/D):  ', strtrim(amp_mod,2)

	pyr = {pyr,					 	 $
	Dpix				: 150L		,$ ; Telescope Pupil diameter (in pixels)
	DpupM				: 8.222		,$ ; Telescope Diameter (m)
	oc					: 0.125		,$ ; This value was enlarged "by eye" from the official of 0.111
									   ;   to remove from syn IM the subapertures close to the OC subject
									   ;   to interpolation errors that caused spikes on the signals.
	n_sspp				: 30		,$ ; number of subapertures across the diameter of the pupil.
	distanza_pup		: 36		,$ ; Separazione richiesta fra i centri di due pupille adiacenti (in pixels)
	fov     			: 2.1		,$ ; WFS Field-of-view (in arcsec)
	steps_mod 			: 16		,$ ; Number of discrete steps to emulate the tip-tilt modulation
	amp_mod 			: amp_mod	,$ ; Modulation radius (in lambda/D units).
	ccd_size			: 90		,$ ; CCD  size
	ccd_dynamic_range	: 120000L	,$ ; CCD dynamic range (e.g. 16 bit).
	binning				: 1			,$ ; CCD binning (1x1, 2x2, 3x3)
	shifts      		: [0,0]	    ,$ ; Shifts of input phase in pixels
	lwfs				: 750.e-9	,$ ; lambda WFS (en m)
	simul_pyrpup		: 'pups_pyrlbt_30x30_1x1.sav' }	;name of pupils used in simulator

	if pyr.binning ne binning then begin
		message, 'Only binning mode #1 supported in this version!',/info
		return,0
	endif

	self._syn_pyr = ptr_new(pyr)

	CASE (im_obj->wfs_status())->wunit() OF
		'W1': begin		;W1 is installed in FLAO#2 (SX): left BACK bent-Gregorian focus!
			pups_shifts = [-0.5,0.]
			self._m3_sig_inv = [1.,-1.]
		 end
		'W2': begin		;W2 is installed in FLAO#1 (DX): right FRONT bent-Gregorian focus!
			pups_shifts = [0.,0.]
			self._m3_sig_inv = [-1.,1.]
		 end
		 else: begin
		 	message, 'Wunit not recognized!', /info
		 	return, 0
		 end
	ENDCASE

	pyrm = init_pyr(pyr.Dpix, pyr.n_sspp, pyr.distanza_pup, pyr.ccd_size, $
		fp_stop_diam = pyr.fov, binning=pyr.binning, DpupM = pyr.DpupM, lambda = pyr.lwfs, $
		shifts = pyr.shifts, pups_shifts=pups_shifts, verbose=1b)

	self._syn_pyrm = ptr_new(pyrm)

	pups_tracknum = ((im_obj->wfs_status())->pupils())->pup_tracknum()
	self._syn_pyrpup_fname = filepath(root=syndata_dir, 'synpups_'+pups_tracknum+'.sav')

	return,1
end


;	Save synthetic pupil data in a format that can be used in the simulator.
;=======================================================================================
pro AOsyncalib::compute_syn_pyrpup
	slmask = self->exp_slmask()
	sz = size(slmask,/dim)
	slidx  = where(slmask,nsub)
	pyrm = self->syn_pyrm()
	simul_pyrpup_fname = (self->syn_pyr()).simul_pyrpup
	simul_pyrpup= pyr_acquire_pupils(filepath(root=self->syndata_dir(), simul_pyrpup_fname),1b)

	new_ind_pup = lonarr(4,nsub)
	for ii=0, 3 do begin
		fr = fltarr(pyrm.real_ccd_side,pyrm.real_ccd_side)
		fr[simul_pyrpup.ind_pup[ii,*]] = 1.
		offx = min(where(fr) mod pyrm.real_ccd_side)
		offy = min(where(fr)  /  pyrm.real_ccd_side)
		fr = fltarr(pyrm.real_ccd_side,pyrm.real_ccd_side)
		fr[offx:offx+sz[0]-1,offy:offy+sz[1]-1] = slmask
		new_ind_pup[ii,*] = where(fr)
	endfor

	pup_data = CREATE_STRUCT("RAD", simul_pyrpup.rad,  $
    	                     "C_COORD", simul_pyrpup.c_coord, $
        	                 "ind_pup", new_ind_pup, $
            	             "n_sspp", (size(new_ind_pup))[2] )
	save, pup_data, filename=self->syn_pyrpup_fname()
end


pro AOsyncalib::get_syn_pyrpup
	if not file_test(self->syn_pyrpup_fname()) then self->compute_syn_pyrpup
	restore, self->syn_pyrpup_fname()
	self._syn_pyrpup = ptr_new(pup_data)
end


function AOsyncalib::syn_pyrpup
	if not ptr_valid(self._syn_pyrpup) then self->get_syn_pyrpup
	return, *(self._syn_pyrpup)
end

pro AOsyncalib::compute_syn_slmask
	pyrm = self->syn_pyrm()
	slmask1 = fltarr(pyrm.real_ccd_side,pyrm.real_ccd_side)
	pyrpup = self->syn_pyrpup()
	slmask1[pyrpup.ind_pup[1,*]] = 1.
	slidx1 = where(slmask1)
	slxr1 = minmax( slidx1 mod pyrm.real_ccd_side)
	slyr1 = minmax( slidx1  /  pyrm.real_ccd_side)
	slmask1 = slmask1[slxr1[0]:slxr1[1],slyr1[0]:slyr1[1]]
	self._syn_slmask = ptr_new(slmask1)
end

function AOsyncalib::syn_slmask
	if not ptr_valid(self._syn_slmask) then self->compute_syn_slmask
	return, *(self._syn_slmask)
end


pro AOsyncalib::visu_pups_on_ccd, amp_mod=amp_mod, steps_mod=steps_mod, pups_shifts=pups_shifts, visu=visu
	if n_elements(amp_mod) eq 0 then amp_mod=10.
	if n_elements(steps_mod) eq 0 then steps_mod=64L

	pyrm   = self->syn_pyrm()
	pyr   = self->syn_pyr()
	mask1 = make_mask(pyr.Dpix, obs=pyr.oc)

	; CCD frame (eventually tuning pups_shifts)
	if n_elements(pups_shifts) eq 2 then pyrm.pups_shifts=pups_shifts
	ccdframe = pyr_ccd_frame_v2( fltarr(pyr.Dpix,pyr.Dpix), pyr=pyrm, maskpup=mask1, /no_show, tt_amp=amp_mod, passi=steps_mod)

	; Simulator original pyrpup
	simul_pyrpup= pyr_acquire_pupils(filepath(root=self->syndata_dir(), pyr.simul_pyrpup),1b)
	simul_pups = fltarr(pyrm.real_ccd_side,pyrm.real_ccd_side)
	simul_pups[simul_pyrpup.ind_pup] = 1.

	if keyword_set(visu) then begin
		window,0, title='simul_pups'
		image_show, ccdframe-ccdframe*simul_pups,/as,/sh
	endif

	; Synthetic pyrpup
	syn_pyrpup = self->syn_pyrpup()
	syn_pups = fltarr(pyrm.real_ccd_side,pyrm.real_ccd_side)
	syn_pups[syn_pyrpup.ind_pup] = 1.

	if keyword_set(visu) then begin
		window,1, title='syn_pups'
		image_show, ccdframe-ccdframe*syn_pups,/as,/sh
	endif

	stop
	;onepupil = ccdframe[0:pyrm.real_ccd_side/2L-1L,0:pyrm.real_ccd_side/2L-1L]
	;onepupil = onepupil[slxr1[0]:slxr1[1],slyr1[0]:slyr1[1]]
end


;	Synthetic IM calibration using a selected measured KL basis
;=======================================================================================
function AOsyncalib::syn_intmat, mymodes, anglerot, shiftval, verbose=verbose, visu=visu $
						, sigminmax=sigminmax, modamp=modamp, sigrms=sigrms
	if not keyword_set(verbose) then verbose = 0b
	if not keyword_set(visu) then visu = 0b
	reflcoeff = 4L	;for measured intmats done with retro-reflector setup

;	mymodes =lindgen(10)
	modemat = self._modeShapes->modemat( mode_idx=mymodes, anglerot=anglerot);, shiftval=shiftval)
	nmodes = n_elements(mymodes)
	idx  = self._modeShapes->idx_mask()
	Dpix = self._modeShapes->Dpix()
	mask = self._modeShapes->mask()

	modamp = mymodes+2
	zern_num, modamp, n=nn
;	modamp = 1.0/sqrt(nn)		;to calibrate KL modes without saturating WFS
	modamp = 1.0/nn

	;syn model and params
	pyrpup = self->syn_pyrpup()
	pyrm   = self->syn_pyrm()
	pyr    = self->syn_pyr()
	mask1 = make_mask(pyr.Dpix, obs=pyr.oc)
	idx1 = where(mask1)

	;Introduce shifts on the CCD frame
	if n_elements(shiftval) eq 0 then shiftval = [0., 0.]
    if n_elements(shiftval) ne 2 then message, 'SHIFTVAL must be of the form: [xshift,yshift]'
	pyrm.pups_shifts += shiftval

	;allocate memory
	klmode  = make_array(size=size(mask))
	im1 = fltarr(nmodes,pyrpup.n_sspp*2L)
	sigminmax = fltarr(nmodes,2)
	sigrms = fltarr(nmodes,2)	;rms of Sx and Sy

	for ii=0, nmodes-1 do begin
		klmode[idx] = modemat[ii,*]
		klmode1 = resample_image(klmode, mask, mask1) ;assumed units: radians of phase @ lwfs.

		slopes_pos = aso_pyr_v2( modamp[ii]*klmode1, pyrm=pyrm, pyrpup=pyrpup, maskPup=mask1, ccd_dynamic_range=pyr.ccd_dynamic_range, $
                    	nphotons=1e6, amp_mod=pyr.amp_mod, steps_mod=pyr.steps_mod, verbose1=verbose, visu2=visu, windows=[0,1], dispfactor=4, ccdframe=framePOS)

    	slopes_neg = aso_pyr_v2(-modamp[ii]*klmode1, pyrm=pyrm, pyrpup=pyrpup, maskPup=mask1, ccd_dynamic_range=pyr.ccd_dynamic_range, $
                    	nphotons=1e6, amp_mod=pyr.amp_mod, steps_mod=pyr.steps_mod, verbose1=verbose, visu2=visu, windows=[0,1], dispfactor=4, ccdframe=frameNEG)

		sl_temp1 = (slopes_pos - slopes_neg)/2.
		sigminmax[ii,*] = minmax(sl_temp1)
		sigrms[ii,*] = [ rms(sl_temp1[0:pyrpup.n_sspp-1]), rms(sl_temp1[pyrpup.n_sspp:*]) ]

		sl_temp = (slopes_pos - slopes_neg)/(2.*modamp[ii])

		;Apply signal inversions
		sl_temp[0:pyrpup.n_sspp-1] = self._m3_sig_inv[0] * sl_temp[0:pyrpup.n_sspp-1]
		sl_temp[pyrpup.n_sspp:*]   = self._m3_sig_inv[1] * sl_temp[pyrpup.n_sspp:*]

	    im1[ii,*] = sl_temp
		if verbose then print, 'act:',ii,'SIGNAL MINMAX:', TRANSPOSE(sigminmax[ii,*])

		IF VISU THEN pyr_pupil_display, framePOS-frameNEG $
			, sl_temp[0:pyrpup.n_sspp-1], sl_temp[pyrpup.n_sspp:*], pyrpup.ind_pup $
			, TARGET = [1], MAGNIFY=4, /NEGATIVE
	endfor

	;scale im to match exp im (metri wf with retro-reflector)
	im1 *= ((2.*!PI / pyr.lwfs)) * reflcoeff

	return, im1
end

function AOsyncalib::find_registration, mode_list, init_pos=init_pos, visu=visu, verbose=verbose, tempdir=tempdir
	common syncalib_common, scobj, mymodes, mintlab, funcvisu, funcverbose, func_tempdir

	mymodes = mode_list
	scobj = self
	if keyword_set(visu) then funcvisu=1B else funcvisu=0b
	if keyword_set(verbose) then funcverbose=1B else funcverbose=0b

	;Create temporary dir to save IMs
	if n_elements(tempdir) eq 0 then func_tempdir = self->syndata_dir() else $
		func_tempdir = filepath(root=self->syndata_dir(), tempdir)
	if not file_test(func_tempdir,/dir) then file_mkdir, func_tempdir

	;exp IM
	nsub = long(total(scobj->exp_slmask()))
	mintlab  = (scobj->imobj())->im()
	mintlab = mintlab[mymodes,*]
	mintlab = mintlab[*,0:nsub*2-1]
	allsx = mintlab[*,0:*:2]
	allsy = mintlab[*,1:*:2]
	mintlab = [[allsx],[allsy]]

	if keyword_set(visu) then begin
		window,0, XSIZE=550, YSIZE=216
		window,1, XSIZE=550, YSIZE=216
		window,2, XSIZE=550, YSIZE=216
		window,3
	endif


    ;positions = [dmrot, xshift, yshift]
;   positions = [-21.,0.,0.]	;FLAO1
;	positions = [48.8,0.,0.]	;FLAO2
	if n_elements(init_pos) eq 0 then positions = [0., 0., 0.] else positions = init_pos
	if n_elements(positions) ne 3 then message, 'init_pos should be [dmrot, xshift, yshift]'


    Ftol = 1.
    initdir = transpose([[3.,0.,0.],[0.,0.1,0.],[0.,0.,0.1]])
    POWELL, positions, initdir, Ftol, Fmin, 'flao_optogeom_errorfunc', ITER=ncalls
    if keyword_set(verbose) then print, 'total number of POWELL iterations: ', strtrim(ncalls,2)

	return, positions
end

function AOsyncalib::scramble_syn2exp, synmat

    if ((self->imobj())->wfs_status())->isSoul() then begin
        n_slopes = 2848
    endif else begin
        n_slopes = 1600
    endelse

	nsub1 = (size(synmat,/dim))[1] / 2L
	nmode = (size(synmat,/dim))[0]
	sx = synmat[*,0:nsub1-1]
	sy = synmat[*,nsub1:*]
	synmat1 = fltarr(672,n_slopes)
	slv = fltarr(nsub1*2)
	for ii=0, nmode-1 do begin
		slv[0:*:2] = sx[ii,*]
		slv[1:*:2] = sy[ii,*]
		synmat1[ii,0] = transpose(slv)
	endfor
	return, synmat1
end

pro AOsyncalib::compare_sigs, mode, anglerot=anglerot, shiftval=shiftval
	nsub = long(total(self->exp_slmask()))
	synsl = reform(self->syn_intmat(mode, anglerot, shiftval))
	synsx = synsl[0:nsub-1]
	synsy = synsl[nsub:*]
	expsx = reform((self->imobj())->sx(mode))
	expsy = reform((self->imobj())->sy(mode))

	pupobj = ((self->imobj())->wfs_status())->pupils()
	indpup = (pupobj->indpup())
	mypup = 0	;use this pupil info to remap signals
	cx  = (pupobj->cx())[mypup]
	cy  = (pupobj->cy())[mypup]
	rad = (pupobj->radius())[mypup]
	xr = [floor(cx-rad),ceil(cx+rad)]
	yr = [floor(cy-rad),ceil(cy+rad)]
	sl2d_w = xr[1]-xr[0]+1
	sl2d_h = yr[1]-yr[0]+1

	fr_sz_x = (((self->imobj())->wfs_status())->camera())->sensorSideX()
	fr_sz_y = (((self->imobj())->wfs_status())->camera())->sensorSideY()
	s2d = fltarr(fr_sz_x,fr_sz_y)
	s2d[indpup[*,mypup]] = expsx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = expsy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	sl_2d = [s2d_tmpA,s2d_tmpB]
	window,0, XSIZE=550, YSIZE=216
	image_show, sl_2d, /as,/sh, title='experimental'

	s2d = fltarr(fr_sz_x,fr_sz_y)
	s2d[indpup[*,mypup]] = synsx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = synsy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	sl_2d = [s2d_tmpA,s2d_tmpB]
	window,1, XSIZE=550, YSIZE=216
	image_show, sl_2d, /as,/sh, title='synthetic'

	s2d = fltarr(fr_sz_x,fr_sz_y)
	s2d[indpup[*,mypup]] = expsx - synsx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = expsy - synsy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	sl_2d = [s2d_tmpA,s2d_tmpB]
	window,2, XSIZE=550, YSIZE=216
	image_show, sl_2d, /as,/sh, title='difference'

	diff = [synsx,synsy] - [expsx,expsy]
	window,3
	plot, [synsx,synsy],yrange=minmax([synsx,synsy,expsx,expsy])
	oplot, [expsx,expsy], color=255L
	oplot, diff, color=255L*250L

	print, 'diff crit.: ', strtrim(total((diff*1e-6)^2.),2)

end



pro AOsyncalib::export_synmat, anglerot, shiftval, export_date=export_date, nmodes=nmodes, verbose=verbose
	if n_params() ne 2 then begin
		message, 'SYNTAXIS: ...->export_synmat, anglerot, shiftval', /info
		return
	endif
	if n_elements(shiftval) ne 2 then begin
		message, 'Shiftval should be a two-dimensional vector [shiftx, shifty]',/info
		return
	endif

	; If generated previously, restore synmat with selected position and max number of modes.
	max_nmodes = self._expim_obj->nmodes()
	synmat_fname = 'synmat_'+self._im_tracknum + $
		'_angle'+strtrim(string(anglerot,format='(f6.2)'),2) + $
		'_shiftx'+strtrim(string(shiftval[0], format='(f7.3)'),2) + $
		'_shifty'+strtrim(string(shiftval[1], format='(f7.3)'),2) + $
		'.sav'
	synmat_fname = filepath(root=self->syndata_dir(), synmat_fname)
	if file_test(synmat_fname) then restore, synmat_fname, verbose=verbose else begin
		mymodes = lindgen(max_nmodes)
		matinter = self->syn_intmat(mymodes, anglerot, shiftval, verbose=verbose, sigminmax=sigminmax $
						, modamp=modamp, sigrms=sigrms )
		pyrpup = self->syn_pyrpup()
		pyrm   = self->syn_pyrm()
		pyr    = self->syn_pyr()
		save, matinter, sigminmax, modamp, sigrms, pyrpup, pyrm, pyr, filename=synmat_fname, /compress
	endelse

	;If requested, export a synthetic IM with a lower number of modes
	if n_elements(nmodes) eq 0 then nmodes = [max_nmodes]

	for jj=0, n_elements(nmodes)-1 do begin
		matinter1 = matinter[0:nmodes[jj]-1,*]
		matinter1 = self->scramble_syn2exp(matinter1)

		;Prepare fits to hold exported IM
		if n_elements(export_date) eq 0 then begin
			today_date = bin_date()
			export_date = 	string(today_date[0],format='(i4)') 	+ $
							string(today_date[1],format='(i02)')	+ $
							string(today_date[2],format='(i02)')
		endif
		export_tracknum = export_date + '_' + string(nmodes[jj], format='(i06)')
		export_fname = 'Intmat_'+export_tracknum+'.fits'
		exp_hdr = self._expim_obj->header()
		mkhdr, hdr, matinter1, /EXTEND
	  	sxaddpar, hdr, 'IM_TYPE', 'SYN', 'IM calib method'
	  	sxaddpar, hdr, 'EXP_IM', self._expim_obj->fname(), 'measured IM matched'
	  	sxaddpar, hdr, 'ANGLEROT', anglerot, 'ASM angle (degrees)'
	  	sxaddpar, hdr, 'X_SHIFT', shiftval[0], 'ASM x-shift (% of pupil size)'
	  	sxaddpar, hdr, 'Y_SHIFT', shiftval[1], 'ASM y-shift (% of pupil size)'
	  	sxaddpar, hdr, 'X_SIGINV', self._m3_sig_inv[0], 'Signal inversion [Sx] caused by M3 reflection'
	  	sxaddpar, hdr, 'Y_SIGINV', self._m3_sig_inv[1], 'Signal inversion [Sy] caused by M3 reflection'
	  	sxaddpar, hdr, 'SYN_SAV', synmat_fname, '.sav file with synthetic IM calib data'
		sxaddpar, hdr, 'FILETYPE', 'intmat'
		sxaddpar, hdr, 'M2C', 		aoget_fits_keyword(exp_hdr, 'M2C')
		sxaddpar, hdr, 'BINNING', 	aoget_fits_keyword(exp_hdr, 'BINNING')
		sxaddpar, hdr, 'IM_MODES', 	nmodes[jj]
		sxaddpar, hdr, 'PUPILS', 	aoget_fits_keyword(exp_hdr, 'PUPILS')
		sxaddpar, hdr, 'INSTR', 	aoget_fits_keyword(exp_hdr, 'INSTR')
		sxaddpar, hdr, 'SENSOR', 	aoget_fits_keyword(exp_hdr, 'SENSOR')
		sxaddpar, hdr, 'W_UNIT', 	aoget_fits_keyword(exp_hdr, 'W_UNIT')
		aoadd_fits_keyword, hdr, 'tt.LAMBDA_D', (self->syn_pyr()).amp_mod
		aoadd_fits_keyword, hdr, 'ccd39.BINNING', aoget_fits_keyword(exp_hdr, 'ccd39.BINNING')
		aoadd_fits_keyword, hdr, 'ocam2.BINNING', aoget_fits_keyword(exp_hdr, 'ocam2.BINNING')
		aoadd_fits_keyword, hdr, 'ocam2.MODE', aoget_fits_keyword(exp_hdr, 'ocam2.MODE')
		aoadd_fits_keyword, hdr, 'sc.PUPILS', aoget_fits_keyword(exp_hdr, 'sc.PUPILS')
		writefits, filepath(root=self->syndata_dir(), export_fname), matinter1, hdr
	endfor
end

function AOsyncalib::modeShapes
    IF (OBJ_VALID(self._modeShapes)) THEN return, self._modeShapes else return, obj_new()
end

function AOsyncalib::imobj
	return, self._expim_obj
end

function AOsyncalib::exp_slmask
	return, *(self._exp_slmask)
end

function AOsyncalib::syn_pyrm
	return, *(self._syn_pyrm)
end

function AOsyncalib::syn_pyrpup_fname
	return, self._syn_pyrpup_fname
end

function AOsyncalib::syn_pyr
	return, *(self._syn_pyr)
end

function AOsyncalib::m3_sig_inv
	return, self._m3_sig_inv
end

function AOsyncalib::syndata_dir
	return, self._syndata_dir
end

pro AOsyncalib::free
	ptr_free, self._exp_slmask
	ptr_free, self._syn_pyr
	ptr_free, self._syn_pyrm
	ptr_free, self._syn_pyrpup
	ptr_free, self._syn_slmask
end

pro AOsyncalib::Cleanup
	obj_destroy, self._modeShapes
	obj_destroy, self._expim_obj
	ptr_free, self._exp_slmask
	ptr_free, self._syn_pyr
	ptr_free, self._syn_pyrm
	ptr_free, self._syn_pyrpup
	ptr_free, self._syn_slmask
end

pro AOsyncalib__define
    struct = { AOsyncalib, $
        _modeShapes		   : obj_new() , $
        _expim_obj		   : obj_new() , $
        _im_tracknum	   : ""		   , $
        _exp_slmask		   : ptr_new() , $
;	synthetic parameters
		_syn_pyr		   : ptr_new() , $
        _syn_pyrm		   : ptr_new() , $
        _syn_pyrpup		   : ptr_new() , $
        _syn_pyrpup_fname  : ""		   , $
        _syn_slmask		   : ptr_new() , $
        _m3_sig_inv		   : [0.,0.]   , $		; Signal inversion [Sx,Sy] required for simulated signals
        										; in order to mimic the reflections introduced by the M3
        _syndata_dir	   : ""		     $
	}
end


