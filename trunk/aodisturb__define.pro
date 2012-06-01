
;+
;
;-

function AOdisturb::Init, root_obj, fname, recompute=recompute
    self._disturb_fname  = ao_datadir()+path_sep()+fname
    if not file_test(self._disturb_fname) then begin
        message, self._disturb_fname + ' not found ', /info
        return, 0
    endif
	if strmatch(self._disturb_fname,'*intmatAcq*') then begin
		message, 'disturbance for intmat acquisition ignored.',/info
		return,0
	endif

    self._store_fname       = filepath(root=root_obj->elabdir(), 'disturb.sav')
    self._store_psd_fname   = filepath(root=root_obj->elabdir(), 'disturb_psd.sav')
    self._store_peaks_fname   = filepath(root=root_obj->elabdir(), 'disturb_peaks.sav')
    if keyword_set(recompute) then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._store_peaks_fname, /allow_nonexistent
    endif

	; Read disturbance file header and retrieve general disturbance parameters:
	header = headfits(self._disturb_fname, /SILENT)
	self._type 	    = strlowcase(strtrim(aoget_fits_keyword(header, 'type'),2))
    self._reflcoef	= float(aoget_fits_keyword(header, 'reflcoef'))
    n_realizations  = long(aoget_fits_keyword(header, 'naxis1'))


    ; Determine the actual frequency of the disturbance.
	;-----------------------------------------------------------------

	; Nominal frequency: This is the frequency for which the disturbance command history was created for.
    nominal_freq = float(aoget_fits_keyword(header, 'overfreq'))

	; Oversampling frequency:
    ovs_time = (root_obj->adsec_status())->ovsamp_time()
    if ovs_time gt 0. then ovs_freq = 1./ovs_time

	; Loop frequency (the frame rate):
	ao_freq = ((root_obj->wfs_status())->ccd39())->framerate()

	; Synchronization mode:
    ; 	disturb_sync eq 0 ; disturb not applied (according to the WFS).
    ;	disturb_sync eq 1 ; disturb applied synchronously with the frame rate.
    ;	disturb_sync eq 3 ; disturb applied at the oversampling frequency.
	disturb_sync = long(aoget_fits_keyword((root_obj->wfs_status())->header(), "sc.DISTURBANCE"))

	self._dist_freq = -1.
	self._ind_realizations = n_realizations
	CASE disturb_sync OF
		0:	return, 0
		1:	BEGIN
			  self._dist_freq = ao_freq
			END
		3:  BEGIN
			  if n_elements(ovs_freq) eq 0 then begin
			  	message, 'oversampling frequency not known', /INFO
			  endif else begin
				if ovs_freq lt ao_freq then self._dist_freq = ao_freq else self._dist_freq = ovs_freq
				;NOTE: ovs_freq should be a multiple of ao_freq: Allow for a 5% of error in this verification
				if (abs(self._dist_freq - round(self._dist_freq/ao_freq)*ao_freq)/self._dist_freq*100. gt 5.) then self._ovs_not_multiple = 1B
				self._ind_realizations = n_realizations / round(self._dist_freq/ao_freq)
			  endelse
			END
	ENDCASE


    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOdisturb', 'Represent a disturbance ') then return, 0
    self->addMethodHelp, "fname()",   "fitsfile name (string)"
    self->addMethodHelp, "header()",  "header of fitsfile (strarr)"
    self->addMethodHelp, "command()", "disturb commands matrix (real, [iter,n_act], [m])"
    self->addMethodHelp, "type()", "type of disturbance (string) (atm, vib, atmo_vib, etc...)"
    self->addMethodHelp, "dist_freq()", "frequency at which disturbance was generated (float) [Hz]"
    self->addMethodHelp, "reflcoef()", "reflection coefficient (float) [adim]"
    self->addMethodHelp, "ind_realizations()", "Number of independent disturbance realizations in data set"
    self->AOtime_series::addHelp, self

    self._disturb_fname_fitsheader = ptr_new(header, /no_copy)


	;Check what type of disturbance and inherit additional data/methods
	;------------------------------------------------------------------------
	CASE self._type OF
		'atm': BEGIN
			if not self->AOdisturbatm::Init(root_obj) then return,0
			self->AOdisturbatm::addHelp, self
			END

		'atm+vib': BEGIN
			if not self->AOdisturbatm::Init(root_obj) then return,0
			self->AOdisturbatm::addHelp, self
			if not self->AOdisturbvib::Init(root_obj) then return,0
			self->AOdisturbvib::addHelp, self
			END

		'vib':	BEGIN
			if not self->AOdisturbvib::Init(root_obj) then return,0
			self->AOdisturbvib::addHelp, self
			END
        'sinusmode': BEGIN
            if not self->AOdisturb_sinmode::Init(root_obj) then return,0
            self->AOdisturb_sinmode::addHelp, self
            END
        'autogain': BEGIN
            END
		'':	message, 'Unknown type of disturbance', /info
	ENDCASE

	;Inherit time-series parameters:
	if self._dist_freq ne -1. then dt=1./self._dist_freq else dt=1.
    if not self->AOtime_series::Init(dt, fftwindow="hamming") then return,0
   	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('[nm-wf Hz^{-1/2}]')
	self._plots_title = root_obj->tracknum()


    return, 1
end

pro AOdisturb::datiProducer
    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        commands = readfits(self._disturb_fname, /SILENT)
        ;commands = transpose(temporary(commands))
        save, commands, file=self._store_fname
    endelse
    self._commands = ptr_new(commands, /no_copy)
end

function AOdisturb::fname
    return, self._disturb_fname
end

function AOdisturb::store_fname
    return, self._store_fname
end

function AOdisturb::command, _extra=ex
    return, self->dati(_extra=ex)
    ;disturb = readfits(ao_datadir()+path_sep()+self->fname(), /SILENT)
    ;return, disturb
end

; to be implemented in AOtime_series subclasses
function AOdisturb::GetDati
    if not ptr_valid(self._commands) then self->datiProducer
    return, self._commands
end

function AOdisturb::header
    if ptr_valid(self._disturb_fname_fitsheader) then return, *(self._disturb_fname_fitsheader) else return, ""
end

function AOdisturb::type
	return, self._type
end

function AOdisturb::dist_freq
	return, self._dist_freq
end

function AOdisturb::reflcoef
	return, self._reflcoef
end

function AOdisturb::ind_realizations
	return, self._ind_realizations
end

function AOdisturb::isok, cause=cause
	dist_ok = 1B

	if self._ovs_not_multiple eq 1b then begin
		cause += " - oversampling freq not a multiple of loop freq"
		dist_ok = 0B
	endif
	if (self._dist_freq eq -1.) then begin
		cause += "- disturbance frequency not known"
		dist_ok = 0B
	endif
	if strmatch(self._type, '*vib*') then $
	  if  self._m2c_mismatch eq 1b then begin
	  	cause += " - M2C is not the same as the M2C used in vibration disturb"
		dist_ok = 0B
	  endif
	if strmatch(self._type, '*atm*') then $
	  if  self._cor_m2c_mismatch eq 1b then begin
		cause += " - M2C is not the same as the M2C used to pre-correct the atm disturb"
		dist_ok = 0B
	  endif
	if strmatch(self._type, 'sinusmode') then begin
	  if self._sin_m2c_mismatch eq 1b then begin
		cause += " - M2C used to generate sinusmode disturbance is NOT the same as current M2C"
		dist_ok = 0B
	  endif
;	  if self._sin_freq_mismatch eq 1b then begin
;		cause += " - sinusmode disturbance framerate mistmach"
;		dist_ok = 0B
;	  endif
	endif

	return,dist_ok
end

pro AOdisturb::free
	if ptr_valid(self._commands) then ptr_free, self._commands
	self->AOtime_series::free
	self->AOdisturb_sinmode::free
end

pro AOdisturb::Cleanup
    if ptr_valid(self._disturb_fname_fitsheader) then ptr_free, self._disturb_fname_fitsheader
	if ptr_valid(self._commands) then ptr_free, self._commands
    if strmatch(self._type, '*vib*') then self->AOdisturbvib::Cleanup
    if strmatch(self._type, '*atm*') then self->AOdisturbatm::Cleanup
    if strmatch(self._type, 'sinusmode') then self->AOdisturb_sinmode::Cleanup
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end


pro AOdisturb__define
    struct = { AOdisturb, $
        _disturb_fname				: ""		, $
        _disturb_fname_fitsheader	: ptr_new()	, $
        _store_fname                : ""        , $
        _type						: ""		, $
        _dist_freq					: 0.		, $		; [Hz]
        _reflcoef					: 0.		, $
        _commands                   : ptr_new()	, $
        _ovs_not_multiple			: 0B		, $
        _ind_realizations			: 0L		, $
        INHERITS AOdisturbatm                   , $
		INHERITS AOdisturbvib                   , $
		INHERITS AOdisturb_sinmode				, $
        INHERITS AOtime_series                  , $
        INHERITS AOhelp $
    }
end


