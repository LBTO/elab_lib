;+
;
;-


function AOelab::Init, tracknum, $
            recompute = recompute, $
            modal_reconstructor_file = modal_reconstructor_file, $	; this is used in case of kalman filter
            dark_fname = dark_fname								    ; override IRTC dark filename

    self._recompute = keyword_set(recompute)

    date = strmid(tracknum, 0, 8)
    self._datadir = filepath(root=ao_datadir(), sub=['adsec_data', date, 'Data_'+tracknum], '')
    self._elabdir = filepath(root=ao_elabdir(), sub=[date, 'Data_'+tracknum], '')


	;verify that the datadir exists before anything.
	if not FILE_TEST(self._datadir, /DIR) then begin
		print, 'data directory does not exist: Data_'+tracknum
		return, 0
	endif

    ; create elab dir and set permission / owner
    tmp_dir = filepath(root=ao_elabdir(), sub=[date], '')
    if file_test(tmp_dir, /dir) eq 0 then begin
        file_mkdir, tmp_dir
        file_chmod,  tmp_dir, /a_read, /a_write, /g_read, /g_write
    endif
    if file_test(self._elabdir, /dir) eq 0 then begin
        file_mkdir, self._elabdir
        file_chmod,  self._elabdir, /a_read, /a_write
    endif

    self._obj_tracknum = obj_new('AOtracknum', tracknum)

    ; create adsec_status leaf
    tmp = filepath(root=self._datadir, 'adsec.sav')
    if file_test(tmp) eq 0 then begin
    	message, 'Cannot find adsec_status file: '+tmp, /info
    	return,0
    endif
    restore, tmp ; restore status
    self._adsec_status = obj_new('AOadsec_status', self, status)
    if not obj_valid(self._adsec_status) then return, 0

    ; create wfs_status leaf
    wfs_status_file = filepath(root=self._datadir, 'wfs.fits')
    self._wfs_status = obj_new('AOwfs_status', self, wfs_status_file)
    if not obj_valid(self._wfs_status) then return, 0

	; create telescope leaf
	self._tel = obj_new('AOtel', wfs_status_file)

    ; create sanity check leaf
    crcerrors_fname = filepath(root=self._datadir,  'CrcErrors_'+tracknum+'.fits')
    fltimeout_fname = filepath(root=self._datadir,  'FlTimeout_'+tracknum+'.fits')
    loopclosed_fname = filepath(root=self._datadir,  'LoopClosed_'+tracknum+'.fits')
    pendingcounter_fname = filepath(root=self._datadir,  'PendingCounter_'+tracknum+'.fits')
    skipcounter_fname = filepath(root=self._datadir,  'SkipCounter_'+tracknum+'.fits')
    timestamp_fname = filepath(root=self._datadir,  'Timestamp_'+tracknum+'.fits')
    wfsglobaltimeout_fname = filepath(root=self._datadir,  'WFSGlobalTimeout_'+tracknum+'.fits')
    self._sanitycheck = obj_new('AOSanityCheck', $
        crcerrors_fname, $
        fltimeout_fname, $
        loopclosed_fname, $
        pendingcounter_fname, $
        skipcounter_fname, $
        timestamp_fname, $
        wfsglobaltimeout_fname $
     )


    ; create control filter leaf
    self._control = obj_new('AOcontrol', self, $
        self._adsec_status->b0_a_file(), $
        self._adsec_status->a_delay_file(), $
        self._adsec_status->b_delay_a_file(), $
        self._adsec_status->c_file(), $
        self._adsec_status->g_gain_a_file() $
        )

    ; create frames counter leaf
    frames_counter_file = filepath(root=self._datadir,  'FramesCounter_'+tracknum+'.fits')
    self._frames_counter = obj_new('AOframes_counter', frames_counter_file, self._wfs_status)

    ; modal_rec
    if keyword_set(modal_reconstructor_file) then begin
        ;self._modal_rec= obj_new('AOrecmatrix', modal_reconstructor_file)
        self._modal_rec = getrecmatrix( modal_reconstructor_file )
    endif else begin
        ;self._modal_rec= obj_new('AOrecmatrix', (self->control())->b0_a_file())
        if (self->control())->isKalman() then begin
            intmat_fname = (self->control())->intmat_fname()
            pos = strpos(intmat_fname, 'Intmat_', /REVERSE_SEARCH)
            if pos eq -1 then message, 'Missing info on interaction matrix', BLOCK='elab', name='ao_oaa_elab'
            rec_fname = strmid(intmat_fname, 0, pos) +'Rec_'+strmid(intmat_fname, pos+7)
        endif else begin
            rec_fname = (self->control())->b0_a_fname()
        endelse
        self._modal_rec = getrecmatrix( rec_fname )
    endelse

	; (modal) interaction matrix
    intmat_fname = (self->control())->intmat_fname()
	self._intmat = getintmat( intmat_fname )

    ; disturb & modaldisturb
    if (self->adsec_status())->disturb_status() eq 1 then begin
        ;self._disturb = getdisturb( (self->adsec_status())->disturb_file(), recompute=self._recompute )
        self._disturb = obj_new('AOdisturb', self, (self->adsec_status())->disturb_file(), recompute=self._recompute )
        self._modaldisturb = obj_new('AOmodaldisturb', self)
    endif

	; effective number of independent realizations in the real-time data acquired.
	if obj_valid(self._disturb) then $
		self._n_periods = self._frames_counter->nframes() / self._disturb->ind_realizations() else $
		self._n_periods = 1L
	if self._n_periods eq 0 then self._n_periods = 1L

    ; slopes
    slopes_file = filepath(root=self._datadir,  'Slopes_'+tracknum+'.fits')
    self._slopes = obj_new('AOslopes', self, slopes_file, self._frames_counter)

    ; residual modes
    self._residual_modes = obj_new('AOresidual_modes', self, self._slopes, self._modal_rec)

    ; modes
    modes_fname = filepath(root=self._datadir,  'Modes_'+tracknum+'.fits')
    self._modes = obj_new('AOmodes', self, modes_fname, self._frames_counter)

    ; commands
    commands_fname = filepath(root=self._datadir,  'Commands_'+tracknum+'.fits')
    self._commands = obj_new('AOcommands', self, commands_fname, self._frames_counter)

    ; positions
    positions_fname = filepath(root=self._datadir,  'Positions_'+tracknum+'.fits')
    self._positions = obj_new('AOpositions', self, positions_fname, self._frames_counter)

    ; modalpositions
    self._modalpositions = obj_new('AOmodalpositions', self)

    ; ccd39 frames
    frames_fname = filepath(root=self._datadir,  'Frames_'+tracknum+'.fits')
    self._frames = obj_new('AOframes', self, frames_fname)

    ; TV
 ;   psf_fnames=file_search(filepath(root=self._datadir, 'psf*.fits'))
 ;   self._psf = obj_new('AOpsf', self, psf_fnames, 13./1800)

    ; IRTC
    irtc_fname = file_search(filepath(root=self._datadir, 'irtc.fits'))
    if (n_elements(dark_fname) eq 0) then begin
    	full_dark_fname = find_irtc_dark(self, irtc_fname)
    endif else begin
        dark_subdir = ['wfs_calib_'+(self->wfs_status())->wunit(),'irtc','backgrounds','bin1'] ;always bin1???
		full_dark_fname = filepath(root=ao_datadir(), sub=dark_subdir,  dark_fname)
	endelse
    self._irtc = obj_new('AOpsf', self, irtc_fname, full_dark_fname);, pixelscale =0.010)

    ; offload modes
    pos2mod_fname = filepath(root=ao_datadir(),  'matrix_proiezione_per_lorenzo.sav') ; TODO fix this name
    self._offloadmodes = obj_new('AOoffloadmodes', self, pos2mod_fname)

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOElab', 'Represents an AO measure') then return, 0
    if obj_valid(self._obj_tracknum) then self->addleaf, self._obj_tracknum, 'obj_tracknum'
    if obj_valid(self._adsec_status) then self->addleaf, self._adsec_status, 'adsec_status'
    if obj_valid(self._wfs_status) then self->addleaf, self._wfs_status, 'wfs_status'
    if obj_valid(self._tel) then self->addleaf, self._tel, 'tel'
    if obj_valid(self._sanitycheck) then self->addleaf, self._sanitycheck, 'sanity_check'
    if obj_valid(self._control) then self->addleaf, self._control, 'control'
    if obj_valid(self._frames_counter) then self->addleaf, self._frames_counter, 'frames_counter'
    if obj_valid(self._slopes) then self->addleaf, self._slopes, 'slopes'
    if obj_valid(self._modal_rec) then self->addleaf, self._modal_rec, 'modal_rec'
    if obj_valid(self._residual_modes) then self->addleaf, self._residual_modes, 'residual_modes'
    if obj_valid(self._modes) then self->addleaf, self._modes, 'modes'
    if obj_valid(self._commands) then self->addleaf, self._commands, 'commands'
    if obj_valid(self._positions) then self->addleaf, self._positions, 'positions'
    if obj_valid(self._modalpositions) then self->addleaf, self._modalpositions, 'modalpositions'
    if obj_valid(self._psf) then self->addleaf, self._psf, 'tv'
    if obj_valid(self._frames) then self->addleaf, self._frames, 'frames'
    if obj_valid(self._disturb) then self->addleaf, self._disturb, 'disturb'
    if obj_valid(self._modaldisturb) then self->addleaf, self._modaldisturb, 'modaldisturb'
    if obj_valid(self._irtc) then self->addleaf, self._irtc, 'irtc'
    if obj_valid(self._offloadmodes) then self->addleaf, self._offloadmodes, 'offloadmodes'
    self->addMethodHelp, "tracknum()", "Tracknum (string)"
    self->addMethodHelp, "obj_tracknum()", "reference to tracknum object (AOtracknum)"
    self->addMethodHelp, "adsec_status()", "reference to adsec status object (AOadsec_status)"
    self->addMethodHelp, "wfs_status()", "reference to wfs status object (AOwfs_status)"
    self->addMethodHelp, "tel()", "reference to telescope object (AOtel_status)"
    self->addMethodHelp, "sanity_check()", "reference to loop sanity check (AOsanitycheck)"
    self->addMethodHelp, "control()", "reference to control filter object (AOcontrol)"
    self->addMethodHelp, "frames_counter()", "reference to frames counter object (AOframes_counter)"
    self->addMethodHelp, "slopes()", "reference to slopes object (AOslopes)"
    self->addMethodHelp, "residual_modes()", "reference to residual modes object (AOresidual_modes)"
    self->addMethodHelp, "modes()", "reference to integrated modes object (AOmodes)"
    self->addMethodHelp, "commands()", "reference to commands object (AOcommands)"
    self->addMethodHelp, "positions()", "reference to mirror positions object (AOpositions)"
    self->addMethodHelp, "modalpositions()", "reference to mirror modal positions object (AOmodalpositions)"
    self->addMethodHelp, "irtc()", "reference to IRTC object (AOpsf)"
    self->addMethodHelp, "tv()", "reference to TV ccd47 object (AOpsf)"
    self->addMethodHelp, "modal_rec()", "reference to modal reconstructor object (AOrecmatrix)"
    self->addMethodHelp, "frames()", "reference to WFS frame object (AOframes)"
    self->addMethodHelp, "disturb()", "reference to disturb object (AOdisturb)"
    self->addMethodHelp, "modaldisturb()", "reference to modal disturb object (AOmodaldisturb)"
    self->addMethodHelp, "offloadmodes()", "reference to offload modes object (AOoffloadmodes)"
    self->addMethodHelp, "mag()", "equivalent star magnitude (R)"
    self->addMethodHelp, "sr_from_positions()", "Strehl Ratio estimate (default H band)"

    ; free memory
    self->free

    return, 1
end

function AOelab::datadir
    return, self._datadir
end

function AOelab::elabdir
    return, self._elabdir
end

function AOelab::recompute
    return, self._recompute
end

function AOelab::reflcoef
	return, ao_reflcoef()
end

function AOelab::n_periods
	return, self._n_periods
end

;;;;;;;;;;;;; Shortcut to most important functions/macro ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function AOelab::tracknum
    return, (self._obj_tracknum)->Tracknum()
end

function AOelab::isOK, cause=cause
    imok=1B
    cause = ""
    imok *= (self->sanitycheck())->isOK(cause=cause)
    imok *= (self->frames_counter())->isok(cause=cause)
    if OBJ_VALID(self->disturb()) then imok *= (self->disturb())->isok(cause=cause)
    if obj_valid(self->intmat()) then $
        if obj_valid(self->wfs_status()) and obj_valid( (self->intmat())->wfs_status() )  then $
    	    if round((self->wfs_status())->modulation()) ne round(((self->intmat())->wfs_status())->modulation()) then begin
    		    imok*=0B
    		    cause += ' - Pyramid modulation mismatch'
    	    endif
    return, imok
end

function AOelab::errorDescription
    ok=self->isOK(cause=cause)
    return, cause
end

function AOelab::closedloop
    return, (self->sanitycheck())->closedloop()
end

function AOelab::mag
    if obj_valid(self->frames()) then $
        if obj_valid(self->wfs_status()) then $
            if obj_valid( (self->wfs_status())->ccd39() ) then $
	            return, tell_me_the_mag((self->frames())->nph_per_int_av(), $
							((self->wfs_status())->ccd39())->framerate() )
    message, 'impossible to compute the magnitude', /info 
    return, !values.f_nan
end

function AOelab::sr_from_positions, lambda_perf=lambda_perf
	if not keyword_set(lambda_perf) then lambda_perf = 1.65e-6 	; Default: H band
	pos_coef_var = (self->modalpositions())->time_variance() * (2*!PI*self->reflcoef()/lambda_perf)^2. ;in rad^2 @ lambda_perf
	return, exp(-total(pos_coef_var))
end



pro AOelab::summary, PARAMS_ONLY=PARAMS_ONLY
    print, string(format='(%"| %-30s | %s |")','Tracknum',self->tracknum() )
    print, string(format='(%"| %-30s | %s |")','Closed Loop', self->closedloop() ? 'Yes' : 'No' )
    print, string(format='(%"| %-30s | %s %s |")','Is OK?', ( self->isOK(cause=cause) eq 1L) ? "OK" :  "No", cause  )
    if obj_valid(self->disturb()) then begin
      if strmatch((self->disturb())->type(),'*atm*') then begin
        print, string(format='(%"| %-30s | %f |")','seeing [arcsec]',(self->disturb())->seeing() )
	    print, string(format='(%"| %-30s | %f |")','Vwind [m/s]',(self->disturb())->vwind() )
	    if (self->disturb())->cor_nmodes() ne 0 then $
	      print, 'Modal pre-correction: '+string((self->disturb())->cor_nmodes())+' modes, starting from mode number '+strtrim((self->disturb())->cor_first_mode(),2)
	  endif
	  if strmatch((self->disturb())->type(),'*vib*') then begin
		print, string(format='(%"| %-30s | %f |")','number of vibrations',(self->disturb())->totnvib() )
		print, string(format='(%"| %-30s | %f |")','type of vibration',(self->disturb())->casevib() )
	  endif
    endif
    if obj_valid(self->frames()) then begin
        print, string(format='(%"| %-30s | %f |")','nphotons/sub/fr', (self->frames())->nphsub_per_int_av())
    endif
    print, string(format='(%"| %-30s | %f |")','Magnitude', self->mag())
    if obj_valid(self->modal_rec()) then begin 
        print, string(format='(%"| %-30s | %d |")','# Modes', (self->modal_rec())->nmodes())
        print, string(format='(%"| %-30s | %s |")','Modal rec', file_basename( (self->modal_rec())->fname() ) )
    endif
    if obj_valid(self->wfs_status()) and obj_valid((self->wfs_status())->ccd39())  then begin 
        print, string(format='(%"| %-30s | %d |")','Binning', ((self->wfs_status())->ccd39())->binning())
        print, string(format='(%"| %-30s | %d |")','Frequency [Hz]', ((self->wfs_status())->ccd39())->framerate())
        print, string(format='(%"| %-30s | %f |")','Modulation', (self->wfs_status())->modulation() )
        ;print, string(format='(%"%-30s %s")','B0_a matrix', (self->control())->b0_a_fname())
        print, string(format='(%"| %-30s | %s |")','FW1', ((self->wfs_status())->filtw1())->name() )
        print, string(format='(%"| %-30s | %s |")','FW2', ((self->wfs_status())->filtw2())->name() )
    endif
    if obj_valid(self->control()) and obj_valid(self->modal_rec()) then begin 
        gaintemp = minmax( ((self->control())->gain())[(self->modal_rec())->modes_idx()] )
        if gaintemp[0] eq -1 then print, 'Gain: AUTO' else $
        print, string(format='(%"| %-30s | %f %f |")','Gain minmax', gaintemp)
    endif
    if obj_valid(self->tel()) then begin 
        print, string(format='(%"| %-30s | %f |")','Telescope elevation', (self->tel())->el()/3600. )
        print, string(format='(%"| %-30s | %f |")','Wind speed', (self->tel())->wind_speed() )
    endif
    if not keyword_set(PARAMS_ONLY) then begin
    	;print, string(format='(%"%-30s %f")','SR@H  FQP',self->sr_from_positions())
    	if obj_valid(self->irtc()) then begin
    		print, string(format='(%"| %-30s | %f |")','lambda [um]',(self->irtc())->lambda()*1e6)
    		print, string(format='(%"| %-30s | %f |")','exptime [s]',(self->irtc())->exptime())
    		print, string(format='(%"| %-30s | %f |")','framerate [Hz]',(self->irtc())->framerate())
    		print, string(format='(%"| %-30s | %d |")','no. frames',(self->irtc())->nframes())
    		print, string(format='(%"| %-30s | %f |")','SR SE' ,(self->irtc())->sr_se())
    		print, string(format='(%"| %-30s | %s |")','IRTC dark', file_basename( (self->irtc())->dark_fname()))
    	endif
    endif
end



pro AOelab::modalplot

	nmodes = (self->modalpositions())->nmodes()
	clvar  = (self->modalpositions())->time_variance() * (1e9*self->reflcoef())^2.
	yrange = sqrt(minmax(clvar))
    if obj_valid(self._disturb) then begin
    	olvar  = (self->modaldisturb())->time_variance() * (1e9*self->reflcoef())^2.
    	yrange = sqrt(minmax([clvar,olvar]))
    endif

	loadct,39, /silent
	plot_oo, lindgen(nmodes)+1, sqrt(clvar), psym=-1, symsize=0.8, charsize=1.2, ytitle='nm rms wf', xtitle='mode number', title=self._obj_tracknum->tracknum(), yrange=yrange
	if obj_valid(self._disturb) then oplot, lindgen(nmodes)+1, sqrt(olvar), psym=-2, symsize=0.8, color=250
	if obj_valid(self._disturb) then legend, ['disturbance','closed-loop'], color=[250,!P.color], psym=-[2,1], /right

end

pro AOelab::estimate_r0, lambda=lambda
	if n_elements(lambda) eq 0 then lambda=500e-9	;nm
	nmodes = (self->modal_rec())->nmodes()
	clvar  = (self->modalpositions())->time_variance() * (10*self->reflcoef()*2.*!PI/lambda)^2.

	; Compare with Zernike variance:
	;Zernike number (1->piston, 2->tip, ...)
	zern_number = indgen(nmodes)+1
	DpupM = 8.22	;m
	r0aso = 0.020	;m
	sec2rad = 4.85*1.e-6

	armando			= (4.*!pi^2)*(DpupM/r0aso)^(5./3.)*diag_matrix(kolm_mcovar(nmodes+1))
;	zern_num, indgen(nmodes)+2, n=nn
;	varNoll         = varzern_turb(nn, DRO = DpupM/r0aso)
	loadct,39, /silent
	plot_oo, lindgen(nmodes)+1, clvar, psym=-1, symsize=0.8, charsize=1.2, ytitle=textoidl('rad^2'), xtitle='mode number', title=self._obj_tracknum->tracknum(), yrange=yrange
	oplot, lindgen(nmodes)+1, armando, psym=-2, color=250

end

pro AOelab::modalSpecPlot, modenum

	if n_params() ne 1 then begin
		message, 'Missing parameter. Usage: ...->modalSpecPlot, modenum', /info
		return
	endif

	nmodes = (self->modalpositions())->nmodes()
	if modenum ge nmodes then begin
		message, 'Mode number requested not available. The last mode available is '+strtrim(nmodes-1,2), /info
		return
	endif

	; corrected PSD
	freq   = (self->modalpositions())->freq()
	psd    = (self->modalpositions())->psd() * (1e9*self->reflcoef())^2.

	; disturbance PSD
	if obj_valid(self._disturb) then $
	  if (self->disturb())->dist_freq() ne -1 then begin
		olfreq = (self->modaldisturb())->freq()
		olpsd  = (self->modaldisturb())->psd() * (1e9*self->reflcoef())^2.
		yrange=sqrt(minmax([olpsd[1:*,modenum],psd[1:*,modenum]]))
	  endif else begin
	  	message, 'disturbance frequency data not available', /info
	  	yrange=sqrt(minmax(psd[1:*,modenum]))
	  endelse

	loadct,39,/silent
	!X.MARGIN = [12, 3]
	plot_oo, freq[1:*], sqrt(psd[1:*,modenum]), charsize=1.2, xtitle='frequency [Hz]', ytitle=textoidl('[nm Hz^{-1/2}]') $
		, title=self._obj_tracknum->tracknum()+', mode '+strtrim(modenum,2), yrange=yrange, ytickformat='(e9.1)'
	if n_elements(olfreq) ne 0 then begin
		oplot, olfreq[1:*], sqrt(olpsd[1:*,modenum]), color=250
		legend, ['disturbance','closed-loop'], color=[250,!P.color], linestyle=[0,0], /bottom
	endif


end

;;;;;;;;;;;;;;;;;;;;;;;;; access to leafs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

function AOelab::obj_tracknum
    return, (self._obj_tracknum)
end

function AOelab::adsec_status
    IF (OBJ_VALID(self._adsec_status)) THEN return, self._adsec_status else return, obj_new()
end

function AOelab::wfs_status
    IF (OBJ_VALID(self._wfs_status)) THEN return, self._wfs_status else return, obj_new()
end

function AOelab::tel
    IF (OBJ_VALID(self._tel)) THEN return, self._tel else return, obj_new()
end

function AOelab::sanitycheck
    IF (OBJ_VALID(self._sanitycheck)) THEN return, self._sanitycheck else return, obj_new()
end

function AOelab::control
    IF (OBJ_VALID(self._control)) THEN return, self._control else return, obj_new()
end

function AOelab::frames_counter
    IF (OBJ_VALID(self._frames_counter)) THEN return, self._frames_counter else return, obj_new()
end

function AOelab::frames
	IF (OBJ_VALID(self._frames)) THEN return, self._frames else return, obj_new()
end

function AOelab::slopes
    IF (OBJ_VALID(self._slopes)) THEN return, self._slopes else return, obj_new()
end

function AOelab::residual_modes
    IF (OBJ_VALID(self._residual_modes)) THEN return, self._residual_modes else return, obj_new()
end

function AOelab::modes
    IF (OBJ_VALID(self._modes)) THEN return, self._modes else return, obj_new()
end

function AOelab::commands
    IF (OBJ_VALID(self._commands)) THEN return, self._commands else return, obj_new()
end

function AOelab::positions
    IF (OBJ_VALID(self._positions)) THEN return, self._positions else return, obj_new()
end

function AOelab::modalpositions
    IF (OBJ_VALID(self._modalpositions)) THEN return, self._modalpositions else return, obj_new()
end

function AOelab::tv
    IF (OBJ_VALID(self._psf)) THEN return, self._psf else return, obj_new()
end

function AOelab::irtc
    IF (OBJ_VALID(self._irtc)) THEN return, self._irtc else return, obj_new()
end

function AOelab::modal_rec
    IF (OBJ_VALID(self._modal_rec)) THEN return, self._modal_rec else return, obj_new()
end

function AOelab::intmat
    IF (OBJ_VALID(self._intmat)) THEN return, self._intmat else return, obj_new()
end

function AOelab::disturb
    IF (OBJ_VALID(self._disturb)) THEN return, self._disturb else return, obj_new()
end

function AOelab::modaldisturb
    IF (OBJ_VALID(self._modaldisturb)) THEN return, self._modaldisturb else return, obj_new()
end

function AOelab::offloadmodes
    IF (OBJ_VALID(self._offloadmodes)) THEN return, self._offloadmodes else return, obj_new()
end

pro AOelab::free
    IF (OBJ_VALID(self._slopes)) THEN  self._slopes->free
    IF (OBJ_VALID(self._residual_modes)) THEN  self._residual_modes->free
    IF (OBJ_VALID(self._modes)) THEN  self._modes->free
    IF (OBJ_VALID(self._commands)) THEN  self._commands->free
    IF (OBJ_VALID(self._positions)) THEN  self._positions->free
    IF (OBJ_VALID(self._modalpositions)) THEN  self._modalpositions->free
    IF (OBJ_VALID(self._offloadmodes)) THEN  self._offloadmodes->free
    IF (OBJ_VALID(self._irtc)) THEN  self._irtc->free
end

pro AOelab::Cleanup
    obj_destroy, self._obj_tracknum
    obj_destroy, self._adsec_status
    obj_destroy, self._wfs_status
    obj_destroy, self._tel
    obj_destroy, self._sanitycheck
    obj_destroy, self._control
    obj_destroy, self._frames_counter
    obj_destroy, self._slopes
    obj_destroy, self._residual_modes
    obj_destroy, self._modes
    obj_destroy, self._commands
    obj_destroy, self._positions
    obj_destroy, self._modalpositions
    obj_destroy, self._psf
    obj_destroy, self._irtc
;    obj_destroy, self._modal_rec
;    obj_destroy, self._intmat
    obj_destroy, self._frames
    obj_destroy, self._disturb
    obj_destroy, self._modaldisturb
    obj_destroy, self._offloadmodes
    self->AOhelp::Cleanup
end

pro AOelab__define
    struct = { AOelab, $
        _datadir           : "",        $
        _elabdir           : "",        $
        _recompute         : 0B,        $
        _n_periods		   : 0L,		$
        _obj_tracknum      : obj_new(), $
        _adsec_status      : obj_new(), $
        _wfs_status        : obj_new(), $
        _tel			   : obj_new(), $
        _sanitycheck       : obj_new(), $
        _control           : obj_new(), $
        _frames_counter    : obj_new(), $
        _slopes            : obj_new(), $
        _residual_modes    : obj_new(), $
        _modes             : obj_new(), $
        _commands          : obj_new(), $
        _positions         : obj_new(), $
        _modalpositions    : obj_new(), $
        _psf               : obj_new(), $
        _irtc              : obj_new(), $
        _modal_rec         : obj_new(), $
        _intmat			   : obj_new(), $
        _frames            : obj_new(), $
        _disturb           : obj_new(), $
        _modaldisturb      : obj_new(), $
        _offloadmodes      : obj_new(), $
        INHERITS AOhelp $
    }
end



