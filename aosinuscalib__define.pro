;+
;
; ANALYSIS OF TRACKNUMS CONTAINING MODULATED SIGNALS FOR SINUSOIDAL IM CALIBRATION
;
;-

function AOsinuscalib::Init, tracknumlist, from_tracknum=from_tracknum, to_tracknum=to_tracknum, recompute=recompute

	if not self->AOdataset::Init(tracknumlist, from=from_tracknum, to=to_tracknum, recompute=recompute, /check) then return,0
	set1 = self->where('disturb.type', 'ne', 'sinusmode')
	if obj_valid(set1) then self->removeTracknum, set1->tracknums()

	nslopes = self->value('wfs_status.pupils.nsub') * 2L
	if n_elements(rem_dup(nslopes)) gt 1 then begin
		message, 'slopes in dataset are not defined on same pupil!'
		return,0
	endif
	self._nslopes = nslopes[0]

	operation_mode = self->value('operation_mode')
	if n_elements(rem_dup(operation_mode)) gt 1 then begin
		message, 'do you really want to mix on-sky and RR data? Dont think so...'
		return,0
	endif
	self._operation_mode = operation_mode[0]

	for ii=0, self->count()-1 do begin
		trn = self->get(pos=ii)
		ao = getaoelab(trn)
		if ii eq 0 then self._meas_imobj = ao->intmat()
		nmodes = ao->ex('disturb.nsinmodes')
		tracknums1 	= replicate(trn,nmodes)
		modes1 		= ao->ex('disturb.sin_mode')
		sin_freq1	= ao->ex('disturb.sin_freq')
		req_amp1	= ao->ex('disturb.mode_amp')

		if ii eq 0 then begin
			tracknums 	= tracknums1
			modes 		= modes1
			sin_freq	= sin_freq1
			req_amp		= req_amp1
		endif else begin
			tracknums 	= [tracknums, tracknums1]
			modes		= [modes, modes1]
			sin_freq	= [sin_freq, sin_freq1]
			req_amp		= [req_amp, req_amp1]
		endelse
	endfor

	self._trns 		= ptr_new(tracknums)
	self._modes		= ptr_new(modes)
	self._req_freq	= ptr_new(sin_freq)
	self._req_amp	= ptr_new(req_amp)
	self._nmeas		= n_elements(tracknums)
	self._global_sign = 1.

;    ; initialize help object and add methods and leafs
    self->addMethodHelp, "summary"	 			 , "summary of parameters (modes, frequencies, amplitudes...)"
    self->addMethodHelp, "sin_intmat([/VISU])"	 , "sinusoidal IM (float)"
    self->addMethodHelp, "imobj()",	"Returns an AOINTMAT object with the sinusoidal IM"
    self->addMethodHelp, "global_sign()"		 , "current value of global sign (float)"
    self->addMethodHelp, "set_global_sign, x"	 , "changes global sign of sinusoidal IM: x={-1.,1.}"
    self->addMethodHelp, "demodulate_im, [/VISU]", "process all signals and produce IM"
    self->addMethodHelp, "compare_sigs, mode, [REFMODE=REFMODE]"	 , "for selected mode, compares demodulated and reference signals"
    self->addMethodHelp, "export [nmodes=nmodes, this_dir=this_dir, /REC]",	"generates fits file with IM"
;    self->addMethodHelp, "verify_disturb", "Checks that disturbance files comply with requirements (PSD analysis)"
    self->addMethodHelp, "visu_specs,idx",	"Visualizes the input and CL disturbance PSD of acquisition #idx"
    self->addMethodHelp, "modes()",                "list of modes actuated in the input disturbances"
    self->addMethodHelp, "req_freq()",             "requested frequency of disturbances"
    self->addMethodHelp, "req_amp()",              "requested amplitude of disturbances"
    self->addMethodHelp, "app_freq()",             "applied frequency of disturbances"
    self->addMethodHelp, "app_amp()",              "applied amplitude of disturbances"

	return,1
end

;+
; SUMMARY OF PARAMETERS OF IM ACQUISITION
;-
pro AOsinuscalib::summary
	hdr = "| *TrackNo* | *mode* | *freq (Hz)* | *amp (nm)* |"
    print, hdr

	for ii=0, self._nmeas-1 do begin
		str = string(format='(%"| %s | %d | %9.2f | %8.1f |")', self->trn(ii), self->modes(ii), $
				self->req_freq(ii), self->req_amp(ii)*1e9 )
		print, str
	endfor
end

;+
; VERIFY THE INPUT DISTURBANCE
;-
pro AOsinuscalib::verify_disturb

	amps  = fltarr(self._nmeas)
	freqs = fltarr(self._nmeas)
	amp_rel_err  = fltarr(self._nmeas)
	freq_rel_err = fltarr(self._nmeas)
	dist_ok = replicate(1B,self._nmeas)

	max_amp_rel_err  = 5.	;(%) maximum relative error in amplitude
	max_freq_rel_err = 5.	;(%) maximum relative error in frequency

    init_progress, self._nmeas
	for ii=0, self._nmeas-1 do begin
		ao = getaoelab(self->trn(ii))
		(ao->modaldisturb())->set_threshold, 1e-5
		freq = ao->ex('modaldisturb.findpeaks('+strtrim(self->modes(ii),2)+').spec'+string(self->modes(ii),format='(i04)')+'.fr')
		pw   = ao->ex('modaldisturb.findpeaks('+strtrim(self->modes(ii),2)+').spec'+string(self->modes(ii),format='(i04)')+'.pw')

		; In case that more than one peak is detected....
		if n_elements(freq) gt 1 then begin
			f_idx	= closest(self->req_freq(ii), freq)
			freq	= freq[f_idx]
			pw		= pw[f_idx]
		endif

		;Estimated amplitude, frequency, and relative errors
		amps[ii]  = sqrt(pw)*sqrt(2.)
		freqs[ii] = freq
		amp_rel_err[ii]  = abs( amps[ii] - self->req_amp(ii) )  / self->req_amp(ii) * 100.
		freq_rel_err[ii] = abs(freqs[ii] - self->req_freq(ii))  / self->req_freq(ii) * 100.
		if amp_rel_err[ii]  gt max_amp_rel_err  then dist_ok[ii] = 0B
		if freq_rel_err[ii] gt max_freq_rel_err then dist_ok[ii] = 0B
        progress, ii, ao->tracknum()
	endfor

	self._dist_ok = ptr_new(dist_ok)
	self._app_freq = ptr_new(freqs)
	self._app_amp = ptr_new(amps)
	self._amp_rel_err = ptr_new(amp_rel_err)
	self._freq_rel_err = ptr_new(freq_rel_err)
end


;+
; AMPLITUDE OF APPLIED SINUSOIDALS IN CLOSED LOOP ESTIMATED FROM PSD (NOT USED FOR IM NORMALIZATION)
;-
pro AOsinuscalib::compute_cl_amps
	cl_amps  = fltarr(self._nmeas)

	for ii=0, self._nmeas-1 do begin
		ao = getaoelab(self->trn(ii))
		mpfreq = (ao->modalpositions())->freq()
		df = mpfreq[1] - mpfreq[0]
		cl_pw  = (ao->modalpositions())->power(self->modes(ii), from_freq=self->req_freq(ii)-df/2., to_freq=self->req_freq(ii)+df/2.)
		cl_amps[ii] = sqrt(cl_pw)*sqrt(2.)	;m surf
	endfor
	self._cl_amp_psd = ptr_new(cl_amps)
end


;+
; DEMODULATE THE SIGNALS!
;-
pro AOsinuscalib::demodulate_im, VISU=VISU, slowly=slowly

	AA_mat = fltarr(self._nmeas)
	BB_mat = fltarr(self._nslopes, self._nmeas)
	delta_mat = fltarr(self._nslopes, self._nmeas)
    nmodes = max(self->modes())
    saturation = fltarr(nmodes, 3)

	if keyword_set(VISU) then window,10
    ; Save saturation data

	; Find the CL modal amplitudes, the signal amplitudes, and estimate delay between c(t) and s(t)
	;-----------------------------------------------------------------------------------------------
    trk= self->tracknums()
    meas=0
    init_progress, n_elements(trk)
    for t=0,n_elements(trk)-1 do begin
        ao = getaoelab(trk[t])
        (ao->sinusacq())->demodulate, VISU = VISU, slowly =slowly
        progress, t, ao->tracknum()
        idx = lindgen((ao->sinusacq())->nmodes())+meas
        AA_mat[idx] = (ao->sinusacq())->AA()
        BB_mat[*,idx]= (ao->sinusacq())->BB()
        delta_mat[*,idx] = (ao->sinusacq())->delta()
        saturation[ (ao->sinusacq())->modes(), *] = (ao->sinusacq())->saturation()
        meas += (ao->sinusacq())->nmodes()
        ao->free
    endfor

	; Find the sign of demodulated signals (modes with same sin frequency analyzed together)
	;------------------------------------------------------------------------------------------
	sin_freqs = self->req_freq(rem_dup(self->req_freq()))
	nfreqs = n_elements(sin_freqs)
	delays = fltarr(self._nmeas)
	for ii=0, nfreqs-1 do begin
		idx_freq = where(self->req_freq() eq sin_freqs[ii], nf)
		idx_neg = sgn_sin_modes(delta_mat[*,idx_freq], VISU=VISU, WIN_ID=10, delay=delay $
					, title='modes @ '+ strtrim(string(sin_freqs[ii],format='(f7.2)'),2)+'Hz' )
                if (n_elements(idx_neg) eq 1) and (idx_neg[0] eq -1) then continue
		delays[idx_freq] = delay
		BB_temp = BB_mat[*,idx_freq]
		BB_temp[idx_neg] *= -1.
		BB_temp *= delay/abs(delay) ;required if phase is >!PI (high-frequency sinusoidals).
		BB_mat[*,idx_freq] = BB_temp
	endfor

	; Normalize BB_mat
	;---------------------------------------------------------------------------
	BB_mat /= rebin(transpose(AA_mat), self._nslopes, self._nmeas, /SAMPLE)
	BB_mat *= self->global_sign()
	if self._operation_mode eq 'ONSKY' then BB_mat *= 2. ; To comply with IMs calibrated with RR (otherwise autogain range is different).


	; Average different measurements of same mode (to increase SNR).
	;---------------------------------------------------------------------------
	sin_modes = self->modes(rem_dup(self->modes()))
	nmodes = n_elements(sin_modes)
	IM = fltarr(max(sin_modes)+1,self._nslopes)

	if nmodes lt self._nmeas then begin
		for ii=0, nmodes-1 do begin
			idx_mode = where(self->modes() eq sin_modes[ii], nm)
			if nm gt 1 then IM[sin_modes[ii],*] = total(BB_mat[*,idx_mode],2) / float(nm) else $
							IM[sin_modes[ii],*] = BB_mat[*,idx_mode]
		endfor
	endif else IM[self->modes(),*] = transpose(BB_mat)

	;Save data
	self._cl_amp_dem = ptr_new(AA_mat)
	self._sin_intmat = ptr_new(IM)
	self._delay		 = ptr_new(delays)
	self._saturation = ptr_new(saturation)

end


;-----------------------------------
; GET PROPERTIES


function AOsinuscalib::trn, idx
	if n_elements(idx) eq 0 then return, *self._trns else $
		return, (*self._trns)[idx]
end

function AOsinuscalib::modes, idx
	if n_elements(idx) eq 0 then return, *self._modes else $
		return, (*self._modes)[idx]
end

function AOsinuscalib::req_freq, idx
	if n_elements(idx) eq 0 then return, *self._req_freq else $
		return, (*self._req_freq)[idx]
end

function AOsinuscalib::req_amp, idx
	if n_elements(idx) eq 0 then return, *self._req_amp else $
		return, (*self._req_amp)[idx]
end

function AOsinuscalib::app_freq, idx
	if not ptr_valid(self._app_freq) then self->verify_disturb
	if n_elements(idx) eq 0 then return, *self._app_freq else $
		return, (*self._app_freq)[idx]
end

function AOsinuscalib::app_amp, idx
	if not ptr_valid(self._app_amp) then self->verify_disturb
	if n_elements(idx) eq 0 then return, *self._app_amp else $
		return, (*self._app_amp)[idx]
end

function AOsinuscalib::amp_rel_err, idx
	if not ptr_valid(self._amp_rel_err) then self->verify_disturb
	if n_elements(idx) eq 0 then return, *self._amp_rel_err else $
		return, (*self._amp_rel_err)[idx]
end

function AOsinuscalib::freq_rel_err, idx
	if not ptr_valid(self._freq_rel_err) then self->verify_disturb
	if n_elements(idx) eq 0 then return, *self._freq_rel_err else $
		return, (*self._freq_rel_err)[idx]
end

function AOsinuscalib::dist_ok, idx;, VERBOSE=VERBOSE
	if not ptr_valid(self._dist_ok) then self->verify_disturb
	if n_elements(idx) eq 0 then return, *self._dist_ok else $
		return, (*self._dist_ok)[idx]
end

function AOsinuscalib::cl_amp_psd, idx
	if not ptr_valid(self._cl_amp_psd) then self->compute_cl_amps
	if n_elements(idx) eq 0 then return, *self._cl_amp_psd else $
		return, (*self._cl_amp_psd)[idx]
end

function AOsinuscalib::cl_amp_dem, idx
	if not ptr_valid(self._cl_amp_dem) then self->demodulate_im
	if n_elements(idx) eq 0 then return, *self._cl_amp_dem else $
		return, (*self._cl_amp_dem)[idx]
end

function AOsinuscalib::saturation
    if not ptr_valid(self._saturation) then self->demodulate_im
	return, *(self._saturation)
end

function AOsinuscalib::global_sign
	return, self._global_sign
end

pro AOsinuscalib::set_global_sign, value
	self._global_sign = value
end

function AOsinuscalib::sin_intmat, VISU=VISU
	if not ptr_valid(self._sin_intmat) then self->demodulate_im, VISU=VISU
	return, *self._sin_intmat
end

; returns Sx in sinusoidal matrix
function AOsinuscalib::sx, mode_num_idx
	im = self->sin_intmat()
	sz = size(im,/dim)
	if n_params() eq 0 then mode_num_idx = lindgen(sz[0])
	sx = im[mode_num_idx,*]
	sx = sx[*,0:*:2]
	return, sx
end

; returns Sy in sinusoidal matrix
function AOsinuscalib::sy, mode_num_idx
	im = self->sin_intmat()
	sz = size(im,/dim)
	if n_params() eq 0 then mode_num_idx = lindgen(sz[0])
	sy = im[mode_num_idx,*]
	sy = sy[*,1:*:2]
	return, sy
end

function AOsinuscalib::meas_imobj
	return, self._meas_imobj
end

function AOsinuscalib::imobj
        fname = filepath(root=getenv('IDL_TMPDIR'), 'im.fits')
        if file_test(fname) then file_delete, fname
        self->export, fname = fname
        return, obj_new('aointmat', fname)
end

;-------------------------------------
; VISUALIZATION ROUTINES

pro AOsinuscalib::visu_specs, idx
	if n_elements(idx) ne 1 then begin
		message, 'SINTAXIS: ee->visu_specs, idx.  (One idx at a time)',/info
		return
	endif
	ao = getaoelab(self->trn(idx))
	window,0
	(ao->modaldisturb())->specplot,self->modes(idx), title='input disturbance, mode'+strtrim(self->modes(idx),2)
	elab_legend, ['f='+strtrim(string(self->req_freq(idx), format='(f6.2)'),2)+'Hz, Amp='+strtrim(string(self->req_amp(idx)*1e9, format='(f6.2)'),2)+' nm surf'], charsize=1.2
	if not self->dist_ok(idx) then elab_legend, ['DISTURBANCE ERROR!'], /bottom, charsize=2
	window,1
	(ao->modalpositions())->specplot,self->modes(idx),title='modal pos CL, mode '+strtrim(self->modes(idx),2)
	elab_legend, ['f='+strtrim(string(self->req_freq(idx), format='(f6.2)'),2)+'Hz, Amp='+strtrim(string(self->cl_amp_psd(idx)*1e9, format='(f6.2)'),2)+' nm surf'], charsize=1.2, /bottom
	;window,2
	;plot, sc->cl_amp_psd()*1e9, sc->cl_amp_dem()*1e9, psym=1, charsize=1.2, xtitle='CL amps from PSD', ytitle='CL amps from demodulation'
	;oplot, [0,1e4], [0,1e4], linestyle=1
end


pro AOsinuscalib::compare_sigs, mode, REFMODE=REFMODE, slo_out=slo_out, compIM_tracknum= compIM_tracknum, data_out = data_out
	if n_elements(mode) ne 1 then begin
		message, 'SINTAXIS: ee->compare_sigs, mode.  (One mode at a time)',/info
		return
	endif
	if not keyword_set(REFMODE) then REFMODE = mode
	if not keyword_set(compIM_tracknum) then imobj = self->meas_imobj() else begin
		imfname = filepath(root=file_dirname((self->meas_imobj())->full_fname()), 'Intmat_'+compIM_tracknum+'.fits')
		if not file_test(imfname) then begin
			message, 'Requested IM not found: '+imfname,/info
			return
		endif
		imobj = obj_new('AOintmat', imfname)
	endelse
	sinsx = reform(self->sx(mode))
	sinsy = reform(self->sy(mode))

	pupobj  = (imobj->wfs_status())->pupils()
	puptrn  = pupobj->pup_tracknum()
	indpup = (pupobj->indpup())
	mypup = 0	;use this pupil info to remap signals
	cx  = (pupobj->cx())[mypup]
	cy  = (pupobj->cy())[mypup]
	rad = (pupobj->radius())[mypup]
	xr = [floor(cx-rad),ceil(cx+rad)]
	yr = [floor(cy-rad),ceil(cy+rad)]
	sl2d_w = xr[1]-xr[0]+1
	sl2d_h = yr[1]-yr[0]+1

    ; sinus
	fr_sz_x = ((imobj->wfs_status())->camera())->sensorSideX()
	fr_sz_y = ((imobj->wfs_status())->camera())->sensorSideY()
	s2d = fltarr(fr_sz_x,fr_sz_y)
	s2d[indpup[*,mypup]] = sinsx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = sinsy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	sl_2d = [s2d_tmpA,s2d_tmpB]
	window,1, XSIZE=550, YSIZE=216
	image_show, sl_2d, /as,/sh, title='sinusoidal IM'
    slo_out=sl_2d

    ; ref
    if REFMODE ge max(imobj->modes_idx()) then begin
        message, 'Cannot compare with mode '+strtrim(REFMODE,2)+': reference matrix stops at mode '+strtrim(max(imobj->modes_idx()),2), /info
        return
    endif
	refsx = reform(imobj->sx(REFMODE))
	refsy = reform(imobj->sy(REFMODE))

	s2d = fltarr(fr_sz_x,fr_sz_y)
	s2d[indpup[*,mypup]] = refsx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = refsy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	sl_2d = [s2d_tmpA,s2d_tmpB]
	window,0, XSIZE=550, YSIZE=216
	image_show, sl_2d, /as,/sh, title='reference IM'

	s2d = fltarr(fr_sz_x,fr_sz_y)
	s2d[indpup[*,mypup]] = refsx - sinsx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = refsy - sinsy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	sl_2d = [s2d_tmpA,s2d_tmpB]
	window,2, XSIZE=550, YSIZE=216
	image_show, sl_2d, /as,/sh, title='difference'

	diff = [sinsx,sinsy] - [refsx,refsy]
	window,3
	plot, [sinsx,sinsy],yrange=minmax([sinsx,sinsy,refsx,refsy])
	oplot, [refsx,refsy], color=255L
	oplot, diff, color=255L*250L
	elab_legend, ['sin','ref','diff'], linestyle=[0,0,0], color=[0,255L,255L*250L]
	print, 'diff crit.: ', strtrim(total((diff*1e-6)^2.),2)
    data_out =[[[sinsx,sinsy]], [[refsx,refsy]], [diff]]
end


;EXPORT INTMAT AS FITS FILE (with optional reconstructor if /REC is set)
;--------------------------------------------------------------------------------------------------------------
pro AOsinuscalib::export, nmodes=nmodes, this_dir=this_dir, fname = fname, REC = REC

    matinter = self->sin_intmat()
    max_nmodes = max(self->modes())+1

    if (ao->wfs_status())->isSoul() then begin 
        n_slopes = 2848
    endif else begin
        n_slopes = 1600
    endelse

    ;If requested, export an IM with a lower number of modes
    if n_elements(nmodes) eq 0 then nmodes = [max_nmodes]

    ; Generate tracknum
    caldat, systime(/julian), m,d,y,hh,mm,ss

	for jj=0, n_elements(nmodes)-1 do begin

        export_tracknum = string(format='(%"%04d%02d%02d_%02d%02d%02d")', y,m,d,hh,mm,ss)

		matinter1 = matinter[0:nmodes[jj]-1,*]

		export_fname = 'Intmat_'+export_tracknum+'.fits'
        rec_fname = 'Rec_'+export_tracknum+'.fits'

		;Setup fits header
		ao = getaoelab(self->get(pos=0))
		hdr = (ao->wfs_status())->header()	;Include this WFS status in IM fits header.
		exp_hdr = self._meas_imobj->header()

	  	sxaddpar, hdr, 'IM_TYPE'	, 'SINUS'				, 'IM calib method'
	  	sxaddpar, hdr, 'CL_IM'		, self._meas_imobj->fname()	, 'IM used in CL during acquisition'
		sxaddpar, hdr, 'FILETYPE'	, 'intmat'
		sxaddpar, hdr, 'N_TRN'		, self->count()			, 'Total number of tracknums associated to this IM calib'
		for ii=0, self->count()-1 do $
			sxaddpar, hdr, 'TRN'+string(ii,format='(i05)'), self->get(pos=ii), 'sinus acq tracknums'
		sxaddpar, hdr, 'M2C', 		aoget_fits_keyword(exp_hdr, 'M2C')
		sxaddpar, hdr, 'BINNING', 	aoget_fits_keyword(exp_hdr, 'BINNING')
		sxaddpar, hdr, 'IM_MODES', 	nmodes[jj]
		sxaddpar, hdr, 'PUPILS', 	aoget_fits_keyword(exp_hdr, 'PUPILS')

		if not keyword_set(this_dir) then this_dir=file_dirname(self._meas_imobj->full_fname()) else $
			if not file_test(this_dir,/dir) then file_mkdir, this_dir
		if not keyword_set(fname) then begin
            intmat_fname = filepath(root=this_dir, export_fname)
            rec_fname = filepath(root=this_dir, rec_fname)
        endif else begin
            intmat_fname = fname
            rec_fname = intmat_fname+'_rec.fits'
        endelse

		if file_test(intmat_fname) then message, 'File already exists! '+ intmat_fname
		writefits, intmat_fname, float(matinter1), hdr
		print, 'sinus-calib IM saved in: '+ intmat_fname

        if keyword_set(REC) then begin
		    if file_test(rec_fname) then message, 'File already exists! '+ rec_fname
            gen_reconstructor, INPUT_FILE=intmat_fname, OUTPUT_FILE=rec_fname, final_dim=[n_slopes,672], CUT=0
		    print, 'reconstructor saved in: '+ rec_fname
        endif

        ;; Prepare next tracknum
        ss +=1
        if ss gt 59 then begin
            ss=0
            mm+=1
        endif
	endfor
end


pro AOsinuscalib::free
	ptr_free, self._trns
	ptr_free, self._modes
	ptr_free, self._req_freq
	ptr_free, self._req_amp
	ptr_free, self._app_freq
	ptr_free, self._app_amp
	ptr_free, self._cl_amp_psd
	ptr_free, self._cl_amp_dem
	ptr_free, self._dist_ok
	ptr_free, self._amp_rel_err
	ptr_free, self._freq_rel_err
	ptr_free, self._delay
	ptr_free, self._sin_intmat
	ptr_free, self._saturation
end


pro AOsinuscalib::Cleanup
    self->free
	self->AOdataset::Cleanup
end


pro AOsinuscalib__define
    struct = { AOsinuscalib	, $
    	_meas_imobj				: obj_new() , $
    	_trns				: ptr_new() , $
		_modes				: ptr_new()	, $
		_req_freq			: ptr_new() , $
		_req_amp			: ptr_new() , $
		_app_freq			: ptr_new() , $
		_app_amp			: ptr_new() , $
		_cl_amp_psd			: ptr_new() , $
		_cl_amp_dem			: ptr_new() , $
		_dist_ok			: ptr_new() , $
		_amp_rel_err		: ptr_new() , $
		_freq_rel_err		: ptr_new() , $
        _saturation         : ptr_new() , $
		_nmeas				: 0L		, $
		_nslopes			: 0L		, $
		_global_sign		: 0.		, $
		_delay				: ptr_new() , $
		_sin_intmat			: ptr_new() , $
		_operation_mode		: ""		, $
		INHERITS AOdataset				$;, $
	}
end

