
;+
;
;-


function AOelab::Init, tracknum, $
            recompute = recompute, $
            modal_reconstructor_file = modal_reconstructor_file, $	; this is used in case of kalman filter
            dark_fname = dark_fname								    ; override IRTC/PISCES dark filename

    self._recompute = keyword_set(recompute)

    date = strmid(tracknum, 0, 8)
    self._datadir = filepath(root=ao_datadir(), sub=['adsec_data', date, 'Data_'+tracknum], '')
    self._elabdir = filepath(root=ao_elabdir(), sub=[date, 'Data_'+tracknum], '')


	;verify that the datadir exists before anything.
	if not FILE_TEST(self._datadir, /DIR) then begin
		print, 'data directory does not exist: Data_'+tracknum
		return, 0
	endif

	temp = FILE_SEARCH(FILEPATH(root=self._datadir, 'gains_step*.fits'), COUNT=ngainfiles)
	if ngainfiles ne 0 then begin
		print, 'gain optimization data: Data_'+tracknum
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
	message, 'Warning: adsec_status object not available!', /info
    	;return,0
    endif else begin
	restore, tmp ; restore status
	self._adsec_status = obj_new('AOadsec_status', self, status)
	if not obj_valid(self._adsec_status) then message, 'Warning: adsec_status object not available!', /info ;return, 0
    endelse

    ; create wfs_status leaf
    wfs_status_file = filepath(root=self._datadir, 'wfs.fits')
    self._wfs_status = obj_new('AOwfs_status', self, wfs_status_file)
    if not obj_valid(self._wfs_status) then message, 'Warning: wfs object not available!', /info ;return, 0

	; create telescope leaf
	self._tel = obj_new('AOtel', self, wfs_status_file)

	; Operation mode: "RR"    : @SolarTower, or @Telescope with RR.
	;				  "ONSKY" : @Telescope on-sky!
	if obj_valid(self._tel) then begin
		;if (self->tel())->el() lt 89. then self._operation_mode = "ONSKY" $
		if not (finite( (self->tel())->rot_angle())) then self._operation_mode = "RR" else begin

			if ((self->wfs_status())->cube_stage() lt -40.) then self._operation_mode = "ONSKY" else $
			if (self->wfs_status())->lamp_intensity() gt .001 then self._operation_mode = "RR" else begin
				self._operation_mode = "ONSKY"
				message, 'Warning: Cube stage inside with lamp off.... Better move it out!',/info
			endelse
		endelse
	endif else self._operation_mode = "RR"	;in Solar Tower


	;Single or double reflection
	if self->operation_mode() eq "RR" then self._reflcoef=4. else self._reflcoef=2.

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
    if not obj_valid(self._adsec_status) then begin
	message, 'Warning: control object not available!' , /info
    endif else begin
	self._control = obj_new('AOcontrol', self, $
	    self._adsec_status->b0_a_file(), $
	    self._adsec_status->a_delay_file(), $
	    self._adsec_status->b_delay_a_file(), $
	    self._adsec_status->c_file(), $
	    self._adsec_status->g_gain_a_file() $
        )
    endelse

    ; create frames counter leaf
    frames_counter_file = filepath(root=self._datadir,  'FramesCounter_'+tracknum+'.fits')
    self._frames_counter = obj_new('AOframes_counter', frames_counter_file, self._wfs_status)
	if not obj_valid(self._frames_counter) then message, 'Warning: FramesCounter object not initialized!', /info ;return,0

    ; modal_rec
    if keyword_set(modal_reconstructor_file) then begin
        self._modal_rec = getrecmatrix( modal_reconstructor_file )
    endif else begin
        if obj_valid(self._control) then begin
	    if (self->control())->isKalman() then begin
		intmat_fname = (self->control())->intmat_fname()
		pos = strpos(intmat_fname, 'Intmat_', /REVERSE_SEARCH)
		if pos eq -1 then message, 'Missing info on interaction matrix', BLOCK='elab', name='ao_oaa_elab'
		rec_fname = strmid(intmat_fname, 0, pos) +'Rec_'+strmid(intmat_fname, pos+7)
	    endif else begin
		rec_fname = (self->control())->b0_a_fname()
	    endelse
	    self._modal_rec = getrecmatrix( rec_fname )
	endif else message, 'Control info not available: Reconstructor object not initialized!', /info
    endelse

    ; (modal) interaction matrix
    if obj_valid(self._control) then begin
	intmat_fname = (self->control())->intmat_fname()
	self._intmat = getintmat( intmat_fname )
    endif else message, 'Control info not available: Interaction Matrix object not initialized!', /info


	; Modes Shapes
	if obj_valid(self->intmat()) then begin
		basis = (self->intmat())->basis()
		modeshapes_fname = filepath(root=ao_phasemapdir(), 'KLmatrix_'+basis+'.sav')
		if file_test(modeshapes_fname) then self._modeShapes = get_modes_shapes(modeShapes_fname) else $
			message, 'Mode Shapes file not found',/info
	endif else message, 'Unknown modal basis: mode shapes not initialized...', /info


    ; disturb & modaldisturb
    if obj_valid(self->wfs_status()) then begin
    disturb_sync = long(aoget_fits_keyword((self->wfs_status())->header(), "sc.DISTURBANCE"))
    if disturb_sync gt 0 then begin
        ;self._disturb = getdisturb( (self->adsec_status())->disturb_file(), recompute=self._recompute )
        self._disturb = obj_new('AOdisturb', self, (self->adsec_status())->disturb_file(), recompute=self._recompute )
        self._modaldisturb = obj_new('AOmodaldisturb', self)
    endif
    endif else message, 'Wfs object not available: disturb object not initialized!', /info

	; effective number of independent realizations in the real-time data acquired.
	if obj_valid(self._disturb) then $
		self._n_periods = self._frames_counter->nframes() / self._disturb->ind_realizations() else $
		self._n_periods = 1L
	if self._n_periods eq 0 then self._n_periods = 1L

    ; slopes
    slopes_file = filepath(root=self._datadir,  'Slopes_'+tracknum+'.fits')
    self._slopes = obj_new('AOslopes', self, slopes_file, self._frames_counter, store_label='slopes')

    ; residual modes
    if obj_valid(self._slopes) and obj_valid(self._modal_rec) then $
        self._residual_modes = obj_new('AOresidual_modes', self, self._slopes, self._modal_rec, store_label='residual_modes')

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

    ; open loop modes
    self._olmodes = obj_new('AOolmodes', self)

    ; ccd39 frames
    frames_fname = filepath(root=self._datadir,  'Frames_'+tracknum+'.fits')
    antidrift_fname = filepath(root=self._datadir, 'AntiDrift_'+tracknum+'.fits')
    self._frames = obj_new('AOframes', self, frames_fname, antidrift_fname)

    ; TV
    tv_fnames=file_search(filepath(root=self._datadir, 'psf*.fits'))
    self._tv  = obj_new('AOTV', self, tv_fnames)

    ; IRTC
    irtc_fname = file_search(filepath(root=self._datadir, 'irtc.fits'))
    self._irtc = obj_new('AOIRTC', self, irtc_fname, dark_fname)

    ; PISCES
    ;pisces_fname = file_search(filepath(root=self._datadir, 'pisces.fits'))
    ;self._piscesold = obj_new('aopiscesold', self, pisces_fname, dark_fname)

    ; PISCES
    pisces_fname = file_search(filepath(root=self._datadir, 'pisces.fits.cq'))
    if file_test(pisces_fname) eq 0 then pisces_fname = file_search(filepath(root=self._datadir, 'pisces.fits'))
    self._pisces = obj_new('aopisces', self, pisces_fname)

    ; offload modes
    pos2mod_fname = filepath(root=ao_datadir(),  'matrix_proiezione_per_lorenzo.sav') ; TODO fix this name
    self._offloadmodes = obj_new('AOoffloadmodes', self, pos2mod_fname)

    ; accelerometers
    dir = ao_datadir()+path_sep()+'accel'
    proj = ao_elabdir()+path_sep()+'accelerometers'+path_sep()+'projection_matrix.fits'
    trnm_acc = tracknum
    temp0 = strmid(trnm_acc, 0, 8)
    temp1 = strmid(trnm_acc, 9)
    if temp0 ge 20101124 then begin
      self._accel = obj_new('AOaccel', self, proj, filepath(root=self._datadir, 'adsec.sav'))
    endif else begin
      flag_acc = 0
      if FILE_TEST(dir+path_sep()+trnm_acc) eq 0 then begin
        t = 'temp1='+temp1+'+1'
        f = execute(t)
        if temp1 lt 95959 then trnm_acc = temp0+'_0'+strtrim(temp1,2) $
          else trnm_acc = temp0+'_'+strtrim(temp1,2)
        if FILE_TEST(dir+path_sep()+trnm_acc) eq 0 then begin
          flag_acc = 1
        endif
      endif
      if flag_acc eq 0 then begin
        acc_file = dir+path_sep()+trnm_acc+path_sep()+'acc.sav'
        self._accel = obj_new('AOaccel', self, proj, acc_file)
      endif
    endelse

    ; slopes null
    if obj_valid(self->wfs_status()) then begin
	    slopes_null_basename 	= (self->wfs_status())->slopes_null_fname()
        if slopes_null_basename ne "" then begin
            wunit  = (self->wfs_status())->wunit()
            binning = ((self->wfs_status())->ccd39())->binning()
	        slopes_null_subdir 		= ['wfs_calib_'+wunit,'slopenulls','bin'+strtrim(binning,2)]
	        slopes_null_fname = filepath(root=ao_datadir(), sub=slopes_null_subdir,  slopes_null_basename)
            self._slopes_null = obj_new('AOslopes', self, slopes_null_fname, self._frames_counter, store_label='slopes_null')
            if obj_valid(self._slopes_null) and obj_valid(self._modal_rec) then $
                self._modes_null = obj_new('AOresidual_modes', self, self._slopes_null, self._modal_rec, store_label='modes_null')
        endif
    endif else message, 'Wfs object not available: slopesnull object not initialized!', /info


    ;
    ; measurement type
    ; if slope null file ==nc.fits then 'SlopeNullMeas'
    ; if file exists 'gains_step1.fits' then 'GainMeas'
    ; else 'LoopMeas'
    self._meas_type = 'LOOP'
    if  obj_valid(self->wfs_status()) then $
        if strtrim(file_basename( (self->wfs_status())->slopes_null_fname()),2) eq 'nc.fits' then self._meas_type = 'NCPA'
    if file_test(filepath(root=self._datadir, 'gains_step1.fits')) then self._meas_type = 'AG'

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
    if obj_valid(self._intmat) then self->addleaf, self._intmat, 'intmat'
    if obj_valid(self._modeShapes) then self->addleaf, self._modeShapes, 'modeShapes'
    if obj_valid(self._residual_modes) then self->addleaf, self._residual_modes, 'residual_modes'
    if obj_valid(self._modes) then self->addleaf, self._modes, 'modes'
    if obj_valid(self._olmodes) then self->addleaf, self._olmodes, 'olmodes'
    if obj_valid(self._commands) then self->addleaf, self._commands, 'commands'
    if obj_valid(self._positions) then self->addleaf, self._positions, 'positions'
    if obj_valid(self._modalpositions) then self->addleaf, self._modalpositions, 'modalpositions'
    if obj_valid(self._tv ) then self->addleaf, self._tv , 'tv'
    if obj_valid(self._frames) then self->addleaf, self._frames, 'frames'
    if obj_valid(self._disturb) then self->addleaf, self._disturb, 'disturb'
    if obj_valid(self._modaldisturb) then self->addleaf, self._modaldisturb, 'modaldisturb'
    if obj_valid(self._irtc) then self->addleaf, self._irtc, 'irtc'
    if obj_valid(self._pisces) then self->addleaf, self._pisces, 'pisces'
    if obj_valid(self._piscesold) then self->addleaf, self._piscesold, 'piscesold'
    if obj_valid(self._offloadmodes) then self->addleaf, self._offloadmodes, 'offloadmodes'
    if obj_valid(self._accel) then self->addleaf, self._accel, 'accel'
    if obj_valid(self._slopes_null) then self->addleaf, self._slopes_null, 'slopesnull'
    if obj_valid(self._modes_null) then self->addleaf, self._modes_null, 'modesnull'

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
    self->addMethodHelp, "olmodes()", "reference to open loop modes object (AOolmodes)"
    self->addMethodHelp, "commands()", "reference to commands object (AOcommands)"
    self->addMethodHelp, "positions()", "reference to mirror positions object (AOpositions)"
    self->addMethodHelp, "modalpositions()", "reference to mirror modal positions object (AOmodalpositions)"
    self->addMethodHelp, "pisces()", "reference to PISCES object (AOscientificimage)"
    self->addMethodHelp, "piscesold()", "reference to old PISCES object (AOpsfabstract)"
    self->addMethodHelp, "irtc()", "reference to IRTC object (AOpsf)"
    self->addMethodHelp, "tv()", "reference to TV ccd47 object (AOpsf)"
    self->addMethodHelp, "modal_rec()", "reference to modal reconstructor object (AOrecmatrix)"
    self->addMethodHelp, "intmat()", "reference to interaction matrix object (AOintmat)"
    self->addMethodHelp, "modeShapes()", "reference to modal shapes object (AOmodeShapes)"
    self->addMethodHelp, "frames()", "reference to WFS frame object (AOframes)"
    self->addMethodHelp, "disturb()", "reference to disturb object (AOdisturb)"
    self->addMethodHelp, "modaldisturb()", "reference to modal disturb object (AOmodaldisturb)"
    self->addMethodHelp, "offloadmodes()", "reference to offload modes object (AOoffloadmodes)"
    self->addMethodHelp, "accel()", "reference to adsec accelerometer data object (0:1 centroid, 2 x, 3 y, 4 z, 5 Rx, 6 Ry, 7 Rz) (AOaccel)"
    self->addMethodHelp, "slopesnull()", "reference to a slopesnull object (AOslopes)"
    self->addMethodHelp, "mag()", "equivalent star magnitude (R)"
    self->addMethodHelp, "sr_from_positions()", "Strehl Ratio estimate (default H band)"
    self->addMethodHelp, "modalplot", "Plot the modal performance evaluation"
    self->addMethodHelp, "operation_mode()", "Return ONSKY or RR (retroreflector)"
    self->addMethodHelp, "meas_type()", "Return the type of measurement: LOOP, NCPA (non-common path calibration), AG (autogain)"
    self->addMethodHelp, "psf", "quick psf display"
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
	return, self._reflcoef
end

function AOelab::n_periods
	return, self._n_periods
end

function AOelab::operation_mode
	return, self._operation_mode
end

function AOelab::meas_type
	return, self._meas_type
end

;;;;;;;;;;;;; Shortcut to most important functions/macro ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function AOelab::tracknum
    return, (self._obj_tracknum)->Tracknum()
end

function AOelab::isOK, cause=cause
    imok=1B
    cause = ""
    if obj_valid(self->sanitycheck()) then imok *= (self->sanitycheck())->isOK(cause=cause)
    if obj_valid(self->frames_counter()) then imok *= (self->frames_counter())->isok(cause=cause)
    if OBJ_VALID(self->disturb()) then imok *= (self->disturb())->isok(cause=cause)
    if OBJ_VALID(self->intmat()) then $
        if OBJ_VALID(self->wfs_status()) and OBJ_VALID( (self->intmat())->wfs_status() )  then begin
    	    if round((self->wfs_status())->modulation()) ne round(((self->intmat())->wfs_status())->modulation()) then begin
    		    imok*=0B
    		    cause += ' - Pyramid modulation mismatch'
    	    endif
    	    if ((self->wfs_status())->pupils())->pup_tracknum() ne (((self->intmat())->wfs_status())->pupils())->pup_tracknum() then begin
    		    ; TODO UGLY PATCH BECAUSE OF DIGITAL SHIFT OF THE PUPILS DONE IN June 2011 RUN
                ; Here we should have families of pupils that belongs to the same set and are just shifted to optimize
                ; camera lens pupil recentering
                if not ( ((self->wfs_status())->pupils())->pup_tracknum() eq '20110601-224135' and $
                   (((self->intmat())->wfs_status())->pupils())->pup_tracknum() eq '20100525-171742' ) then begin
                    imok*=0B
    		        cause += ' - Pupils mismatch'
                endif
			endif
    	endif
    if OBJ_VALID(self->wfs_status()) then imok *= (self->wfs_status())->isok(cause=cause)
    if OBJ_VALID(self->irtc()) then imok *= (self->irtc())->isok(cause=cause)
    if OBJ_VALID(self->pisces()) then imok *= (self->pisces())->isok(cause=cause)
    return, imok
end

function AOelab::errorDescription
    ok=self->isOK(cause=cause)
    return, cause
end

function AOelab::closedloop
    return, (self->sanitycheck())->closedloop()
end


;function AOelab::mag_v2
;    if obj_valid(self->frames()) then $
;        if obj_valid(self->wfs_status()) then $
;            if obj_valid( (self->wfs_status())->ccd39() ) then $
;				if obj_valid( (self->wfs_status())->filtw1() ) then begin
;					nphpup = (self->frames())->nph_per_int_av()
;					framerate = ((self->wfs_status())->ccd39())->framerate()
;					fw1_trans = ((self->wfs_status())->filtw1())->transmissivity()
;					fw1_lower_bound = ((self->wfs_status())->filtw1())->cw() - $
;									  ((self->wfs_status())->filtw1())->bw()/2.
;					fw1_upper_bound = ((self->wfs_status())->filtw1())->cw() + $
;									  ((self->wfs_status())->filtw1())->bw()/2.
;
;	            	return, tell_me_the_mag_v2(nphpup, framerate, fw1_trans, fw1_lower_bound, fw1_upper_bound)
;				endif
;    message, 'impossible to compute the magnitude', /info
;    return, !values.f_nan
;end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                   SUMMARY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    	print, string(format='(%"| %-30s | %s |")','AntiDrift', (self->frames())->antidrift_status() ? 'ON':'OFF' )
        print, string(format='(%"| %-30s | %f |")','nphotons/sub/fr', (self->frames())->nphsub_per_int_av())
    endif
    print, string(format='(%"| %-30s | %f |")','Magnitude', self->mag())
    if obj_valid(self->modal_rec()) then begin
        print, string(format='(%"| %-30s | %d |")','# Modes', (self->modal_rec())->nmodes())
        print, string(format='(%"| %-30s | %s |")','Modal rec', file_basename( (self->modal_rec())->fname() ) )
    endif
    if obj_valid(self->wfs_status()) then if obj_valid((self->wfs_status())->ccd39())  then begin
        print, string(format='(%"| %-30s | %d |")','Binning', ((self->wfs_status())->ccd39())->binning())
        print, string(format='(%"| %-30s | %d |")','Frequency [Hz]', ((self->wfs_status())->ccd39())->framerate())
        print, string(format='(%"| %-30s | %f |")','Modulation', (self->wfs_status())->modulation() )
        ;print, string(format='(%"%-30s %s")','B0_a matrix', (self->control())->b0_a_fname())
        if obj_valid((self->wfs_status())->filtw1()) then $
        	print, string(format='(%"| %-30s | %s |")','FW1', ((self->wfs_status())->filtw1())->name() )
		if obj_valid((self->wfs_status())->filtw2()) then $
        	print, string(format='(%"| %-30s | %s |")','FW2', ((self->wfs_status())->filtw2())->name() )
    endif
    if obj_valid(self->control()) then begin
        gaintemp = minmax( (self->control())->gain() )
        if gaintemp[0] eq -1 then print, 'Gain: AUTO' else $
        print, string(format='(%"| %-30s | %f %f |")','Gain minmax', gaintemp)
    endif
    if obj_valid(self->tel()) then begin
        print, string(format='(%"| %-30s | %f |")','Telescope elevation', (self->tel())->el() )
        print, string(format='(%"| %-30s | %f |")','Wind speed', (self->tel())->wind_speed() )
    endif
    if obj_valid(self->olmodes()) then begin
    	print, string(format='(%"| %-30s | %f |")','seeing from OL modes', (self->olmodes())->seeing() )
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
    	if obj_valid(self->pisces()) then begin
    		print, string(format='(%"| %-30s | %s |")','filter',(self->pisces())->filter_name())
    		print, string(format='(%"| %-30s | %f |")','lambda [um]',(self->pisces())->lambda()*1e6)
    		print, string(format='(%"| %-30s | %f |")','exptime [s]',(self->pisces())->exptime())
    		print, string(format='(%"| %-30s | %f |")','framerate [Hz]',(self->pisces())->framerate())
    		print, string(format='(%"| %-30s | %d |")','no. frames',(self->pisces())->nframes())
    		print, string(format='(%"| %-30s | %f |")','SR SE' ,(self->pisces())->sr_se())
    		print, string(format='(%"| %-30s | %s |")','pisces dark', file_basename( (self->pisces())->dark_fname()))
    	endif
       	if obj_valid(self->piscesold()) then begin
    		print, string(format='(%"| %-30s | %f |")','piscesold lambda [um]',(self->piscesold())->lambda()*1e6)
    		print, string(format='(%"| %-30s | %f |")','piscesold exptime [s]',(self->piscesold())->exptime())
    		print, string(format='(%"| %-30s | %f |")','piscesold framerate [Hz]',(self->piscesold())->framerate())
    		print, string(format='(%"| %-30s | %d |")','piscesold no. frames',(self->piscesold())->nframes())
    		print, string(format='(%"| %-30s | %f |")','piscesold SR SE' ,(self->piscesold())->sr_se())
    		print, string(format='(%"| %-30s | %s |")','piscesold dark', file_basename( (self->piscesold())->dark_fname()))
    	endif

    endif
end

pro AOelab::fullsummary
    if obj_valid(self._obj_tracknum) then if obj_hasmethod(self._obj_tracknum, 'summary') then self._obj_tracknum->summary
    if obj_valid(self._adsec_status) then if obj_hasmethod(self._adsec_status, 'summary') then self._adsec_status->summary
    if obj_valid(self._wfs_status) then if obj_hasmethod(self._wfs_status, 'summary') then self._wfs_status->summary
    if obj_valid(self._tel) then if obj_hasmethod(self._tel, 'summary') then self._tel->summary
    if obj_valid(self._sanitycheck) then if obj_hasmethod(self._sanitycheck, 'summary') then self._sanitycheck->summary
    if obj_valid(self._control) then if obj_hasmethod(self._control, 'summary') then self._control->summary
    if obj_valid(self._frames_counter) then if obj_hasmethod(self._frames_counter, 'summary') then self._frames_counter->summary
    if obj_valid(self._slopes) then if obj_hasmethod(self._slopes, 'summary') then self._slopes->summary
    if obj_valid(self._modal_rec) then if obj_hasmethod(self._modal_rec, 'summary') then self._modal_rec->summary
    if obj_valid(self._intmat) then if obj_hasmethod(self._intmat, 'summary') then self._intmat->summary
    if obj_valid(self._modeShapes) then if obj_hasmethod(self._modeShapes, 'summary') then self._modeShapes->summary
    if obj_valid(self._residual_modes) then if obj_hasmethod(self._residual_modes, 'summary') then self._residual_modes->summary
    if obj_valid(self._modes) then if obj_hasmethod(self._modes, 'summary') then self._modes->summary
    if obj_valid(self._olmodes) then if obj_hasmethod(self._olmodes, 'summary') then self._olmodes->summary
    if obj_valid(self._commands) then if obj_hasmethod(self._commands, 'summary') then self._commands->summary
    if obj_valid(self._positions) then if obj_hasmethod(self._positions, 'summary') then self._positions->summary
    if obj_valid(self._modalpositions) then if obj_hasmethod(self._modalpositions, 'summary') then self._modalpositions->summary
    if obj_valid(self._tv) then if obj_hasmethod(self._tv, 'summary') then self._tv->summary
    if obj_valid(self._frames) then if obj_hasmethod(self._frames, 'summary') then self._frames->summary
    if obj_valid(self._disturb) then if obj_hasmethod(self._disturb, 'summary') then self._disturb->summary
    if obj_valid(self._modaldisturb) then if obj_hasmethod(self._modaldisturb, 'summary') then self._modaldisturb->summary
    if obj_valid(self._irtc) then if obj_hasmethod(self._irtc, 'summary') then self._irtc->summary
    if obj_valid(self._pisces) then if obj_hasmethod(self._pisces, 'summary') then self._pisces->summary
    ;if obj_valid(self._piscesold) then if obj_hasmethod(self._piscesold, 'summary') then self._piscesold->summary
    if obj_valid(self._offloadmodes) then if obj_hasmethod(self._offloadmodes, 'summary') then self._offloadmodes->summary
    if obj_valid(self._accel) then if obj_hasmethod(self._accel, 'summary') then self._accel->summary
    if obj_valid(self._slopes_null) then if obj_hasmethod(self._slopes_null, 'summary') then self._slopes_null->summary
    if obj_valid(self._modes_null) then if obj_hasmethod(self._modes_null, 'summary') then self._modes_null->summary
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  PLOTS and SHORTCUTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro AOelab::modalplot, OVERPLOT = OVERPLOT, COLOR=COLOR, _extra=ex
    if self->operation_mode() eq "RR" then begin
		nmodes = (self->modalpositions())->nmodes()
		clvar  = (self->modalpositions())->time_variance() * (1e9*self->reflcoef())^2.
		yrange = sqrt(minmax(clvar))
    	if obj_valid(self._disturb) then begin
    		olvar  = (self->modaldisturb())->time_variance() * (1e9*self->reflcoef())^2.
    		yrange = sqrt(minmax([clvar,olvar]))
    	endif
        if not keyword_set(OVERPLOT) then  begin
		    plot_oo, lindgen(nmodes)+1, sqrt(clvar), psym=-1, symsize=0.8, charsize=1.2, ytitle='nm rms wf', xtitle='mode number', title=self._obj_tracknum->tracknum(), yrange=yrange, _extra=ex
        endif else begin
		    oplot, lindgen(nmodes)+1, sqrt(clvar), psym=-1, symsize=0.8,COLOR=COLOR
        endelse
		if obj_valid(self._disturb) then oplot, lindgen(nmodes)+1, sqrt(olvar), psym=-2, symsize=0.8, color='0000ff'x
		if obj_valid(self._disturb) then legend, ['disturbance','closed-loop'], color=['0000ff'x,!P.color], psym=-[2,1], /right
	endif else begin
;		nmodes = (self->residual_modes())->nmodes()
		clvar  = (self->residual_modes())->time_variance() * (1e9*self->reflcoef())^2.
		olvar  = (self->olmodes())->time_variance() * (1e9*self->reflcoef())^2.
		modes_idx = (self->modal_rec())->modes_idx()
		clvar  = clvar[modes_idx]
		olvar  = olvar[modes_idx]
   		yrange = sqrt(minmax([clvar,olvar]))
        if not keyword_set(OVERPLOT) then  begin
			plot_oo, modes_idx+1, sqrt(clvar), psym=-1, symsize=0.8, charsize=1.2, ytitle='nm rms wf', xtitle='mode number', title=self._obj_tracknum->tracknum(), yrange=yrange, _extra=ex
        endif else begin
		    oplot, modes_idx+1, sqrt(clvar), psym=-1, symsize=0.8,COLOR=COLOR
		endelse
		oplot, modes_idx+1, sqrt(olvar), psym=-2, symsize=0.8, color='0000ff'x
	endelse
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

function AOelab::sr_from_positions, lambda_perf=lambda_perf
	if not keyword_set(lambda_perf) then lambda_perf = 1.65e-6 	; Default: H band
	pos_coef_var = (self->modalpositions())->time_variance() * (2*!PI*self->reflcoef()/lambda_perf)^2. ;in rad^2 @ lambda_perf
	return, exp(-total(pos_coef_var))
end

pro AOelab::psf, WINDOW = WINDOW, fullframe=fullframe, sr=sr
    loadct,3
    if keyword_set(sr) then begin
        obj = OBJ_VALID(self->irtc()) ? self->irtc() : self->pisces()
        image_show, /lab, /as, /sh, /log, title=self->tracknum(), pos=pos, obj->longexposure(/fullframe)>0.1
        nstars = obj->nstars()
        starpos = obj->star_position_px()
        sr = 100.*obj->star_sr()
        for i=0,nstars-1 do begin
            starposdev = starpos[*,i]/1024 * [pos[2]-pos[0], pos[3]-pos[1]] + [pos[0],pos[1]]
            xyouts, starposdev[0], starposdev[1], string(format='(%"%d - %4.1f")', i, sr[i]), charsi=1.5, col='ffffff'x, /dev, alig=0.5
        endfor
    endif else begin
        psf = OBJ_VALID(self->irtc()) ?  (self->irtc())->longexposure() : (self->pisces())->longexposure(fullframe=fullframe)
        xshow, /lab, /as, /sh, /log, title=self->tracknum(), pos=pos,   psf>0.1
    endelse

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           MEMBERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

function AOelab::olmodes
    IF (OBJ_VALID(self._olmodes)) THEN return, self._olmodes else return, obj_new()
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
    IF (OBJ_VALID(self._tv )) THEN return, self._tv  else return, obj_new()
end

function AOelab::irtc
    IF (OBJ_VALID(self._irtc)) THEN return, self._irtc else return, obj_new()
end

function AOelab::pisces
    IF (OBJ_VALID(self._pisces)) THEN return, self._pisces else return, obj_new()
end

function AOelab::piscesold
    IF (OBJ_VALID(self._piscesold)) THEN return, self._piscesold else return, obj_new()
end

function AOelab::modal_rec
    IF (OBJ_VALID(self._modal_rec)) THEN return, self._modal_rec else return, obj_new()
end

function AOelab::intmat
    IF (OBJ_VALID(self._intmat)) THEN return, self._intmat else return, obj_new()
end

function AOelab::modeShapes
    IF (OBJ_VALID(self._modeShapes)) THEN return, self._modeShapes else return, obj_new()
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

function AOelab::accel
  IF (OBJ_VALID(self._accel)) THEN return, self._accel else return, obj_new()
end

function AOelab::slopes_null
    IF (OBJ_VALID(self._slopes_null)) THEN return, self._slopes_null else return, obj_new()
end

function AOelab::modes_null
    IF (OBJ_VALID(self._modes_null)) THEN return, self._modes_null else return, obj_new()
end

function AOelab::ex, cmd,  isvalid=isvalid
  	apex = string(39B)

	;Split cmd string
	level=0
	pos=0
	for i=0,strlen(cmd)-1 do begin
		c = strmid(cmd, i, 1)
		if c eq '(' then level +=1
		if c eq ')' then level -=1
		if c eq '.' AND level eq 0 then begin
			s = strmid(cmd,pos,i-pos)
			if n_elements(cmds) eq 0 then cmds = s else cmds = [cmds,s]
			pos=i+1
		endif
	endfor
	s = strmid(cmd,pos,i-pos)
	if n_elements(cmds) eq 0 then cmds = s else cmds = [cmds,s]

	;Start processing each substring
    isvalid=0
	tmpobj=self
    for j=0L, n_elements(cmds)-2 do begin
        method_name = (strsplit(cmds[j], '(', /extr))[0]
        add_brackets = strpos(cmds[j], '(') eq -1 ? '()' : ''
        r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+method_name+apex+')')
        r=execute('if (hasmethod) then tmpobj= tmpobj->'+cmds[j]+add_brackets)
        if not obj_valid(tmpobj) then break ;
    endfor

    if test_type(tmpobj, /struct) eq 0 then begin
    	tmpstruct = tmpobj
    	for k=j+1, n_elements(cmds)-1 do begin
    		tag_name = cmds[k]
		    r=execute('hastag = tag_exist(tmpstruct, '+apex+tag_name+apex+')')
		    r=execute('if (hastag) then begin & tmpstruct = tmpstruct.'+tag_name+' & isvalid=1 & endif else isvalid=0')
		endfor
		value = tmpstruct
    endif else begin
    	if j eq n_elements(cmds)-1 then begin
        	method_name = (strsplit(cmds[j], '(', /extr))[0]
        	add_brackets = strpos(cmds[j], '(') eq -1 ? '()' : ''
        	r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+method_name+apex+')')
        	r=execute('if (hasmethod) then begin & value = tmpobj->'+cmds[j]+add_brackets+'  & isvalid=1 & endif')
        	if test_type(value, /obj_ref) eq 0 then if obj_valid(value) eq 0 then isvalid=0
    	endif
    endelse

    if isvalid eq 1 then begin
        return, value
    endif else begin
        message, '...INVALID FUNCTION: '+cmd
    endelse

end

pro AOelab::test
    IF (OBJ_VALID(self._obj_tracknum)) THEN  self._obj_tracknum->test
    IF (OBJ_VALID(self._adsec_status)) THEN  self._adsec_status->test
    IF (OBJ_VALID(self._wfs_status)) THEN  self._wfs_status->test
    IF (OBJ_VALID(self._tel)) THEN  self._tel->test
    IF (OBJ_VALID(self._sanitycheck)) THEN  self._sanitycheck->test
end

pro AOelab::free
    IF (OBJ_VALID(self._wfs_status )) THEN  self._wfs_status->free
    IF (OBJ_VALID(self._control )) THEN  self._control->free
    IF (OBJ_VALID(self._frames_counter)) THEN  self._frames_counter->free
    IF (OBJ_VALID(self._slopes)) THEN  self._slopes->free
    IF (OBJ_VALID(self._residual_modes)) THEN  self._residual_modes->free
    IF (OBJ_VALID(self._modes)) THEN  self._modes->free
    IF (OBJ_VALID(self._olmodes)) THEN  self._olmodes->free
    IF (OBJ_VALID(self._commands)) THEN  self._commands->free
    IF (OBJ_VALID(self._positions)) THEN  self._positions->free
    IF (OBJ_VALID(self._modalpositions)) THEN  self._modalpositions->free
    IF (OBJ_VALID(self._tv)) THEN  self._tv->free
    IF (OBJ_VALID(self._irtc)) THEN  self._irtc->free
    IF (OBJ_VALID(self._pisces)) THEN  self._pisces->free
    IF (OBJ_VALID(self._piscesold)) THEN  self._piscesold->free
    IF (OBJ_VALID(self._modal_rec)) THEN  self._modal_rec->free
    IF (OBJ_VALID(self._intmat)) THEN  self._intmat->free
    IF (OBJ_VALID(self._modal_rec)) THEN  self._modal_rec->free
    IF (OBJ_VALID(self._modeShapes)) THEN  self._modeShapes->free
    IF (OBJ_VALID(self._frames)) THEN  self._frames->free
    IF (OBJ_VALID(self._disturb)) THEN self._disturb->free
    IF (OBJ_VALID(self._modaldisturb)) THEN self._modaldisturb->free
    IF (OBJ_VALID(self._offloadmodes)) THEN  self._offloadmodes->free
    IF (OBJ_VALID(self._accel )) THEN  self._accel->free
    IF (OBJ_VALID(self._slopes_null)) THEN  self._slopes_null->free
    IF (OBJ_VALID(self._modes_null)) THEN  self._modes_null->free
end

pro AOelab::Cleanup
    obj_destroy, self._obj_tracknum
    ;obj_destroy, self._adsec_status
    obj_destroy, self._wfs_status
    obj_destroy, self._tel
    obj_destroy, self._sanitycheck
    obj_destroy, self._control
    obj_destroy, self._frames_counter
    obj_destroy, self._slopes
    obj_destroy, self._residual_modes
    obj_destroy, self._modes
    obj_destroy, self._olmodes
    obj_destroy, self._commands
    obj_destroy, self._positions
    obj_destroy, self._modalpositions
    obj_destroy, self._tv
    obj_destroy, self._irtc
    obj_destroy, self._pisces
    obj_destroy, self._piscesold
;    obj_destroy, self._modal_rec
;    obj_destroy, self._intmat
;	 obj_destroy, self._modeShapes
    obj_destroy, self._frames
    obj_destroy, self._disturb
    obj_destroy, self._modaldisturb
    obj_destroy, self._offloadmodes
    obj_destroy, self._accel
    obj_destroy, self._slopes_null
    obj_destroy, self._modes_null
    self->AOhelp::Cleanup
end

pro AOelab__define
    struct = { AOelab, $
        _datadir           : "",        $
        _elabdir           : "",        $
        _recompute         : 0B,        $
        _n_periods		   : 0L,		$
        _operation_mode    : "",		$	; "RR": retroreflector, "ONSKY", idem.
        _meas_type         : "",		$	; "LOOP", "NCPA", "AG"
        _reflcoef		   : 0.,		$
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
        _olmodes           : obj_new(), $
        _commands          : obj_new(), $
        _positions         : obj_new(), $
        _modalpositions    : obj_new(), $
        _tv                : obj_new(), $
        _irtc              : obj_new(), $
        _pisces            : obj_new(), $
        _piscesold         : obj_new(), $
        _modal_rec         : obj_new(), $
        _intmat			   : obj_new(), $
        _modeShapes		   : obj_new(), $
        _frames            : obj_new(), $
        _disturb           : obj_new(), $
        _modaldisturb      : obj_new(), $
        _offloadmodes      : obj_new(), $
        _accel             : obj_new(), $
        _slopes_null       : obj_new(), $
        _modes_null        : obj_new(), $
        INHERITS AOhelp $
    }
end



