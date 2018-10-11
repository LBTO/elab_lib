
;+
;
;-
function AOdisturb_sinmode::Init, root_obj

    ;Retrieve all disturbance info from header:
    hdr = self->header()
    self._nmodulatedmodes = long(aoget_fits_keyword(hdr, 'NMODES'))
    if self._nmodulatedmodes eq 0 then begin ; old format with just one frequency
    	self._nmodulatedmodes = 1
	    self._sin_freq = ptr_new(float(aoget_fits_keyword(hdr, 'FREQEFF')))
	    self._sin_mode = ptr_new(long(aoget_fits_keyword(hdr, 'MODE')))
	    self._mode_amp = ptr_new(float(aoget_fits_keyword(hdr, 'AMP')))
    endif else begin
        sinfreq = fltarr(self._nmodulatedmodes)
        sinmode = lonarr(self._nmodulatedmodes)
        modeamp = fltarr(self._nmodulatedmodes)
        for i=0, self._nmodulatedmodes-1 do begin
            sinfreq[i] = float(aoget_fits_keyword(hdr, 'FREQEFF'+strtrim(i,2)))
            sinmode[i] = long(aoget_fits_keyword(hdr, 'MODE'+strtrim(i,2)))
            modeamp[i] = float(aoget_fits_keyword(hdr, 'AMP'+strtrim(i,2)))
        endfor
	    self._sin_freq = ptr_new(sinfreq)
	    self._sin_mode = ptr_new(sinmode)
	    self._mode_amp = ptr_new(modeamp)
    endelse

	basis = aoget_fits_keyword(hdr, 'M2C')
	if obj_valid(root_obj->intmat()) then $
      if strtrim(basis,2) ne (root_obj->intmat())->basis() then begin
    	message, 'Current M2C is not equal to M2C used to create disturbance!', /INFO
    	self._sin_m2c_mismatch = 1B
      endif

	loopfreq = float(aoget_fits_keyword(hdr, 'LOOPFREQ'))
	if round(loopfreq) ne round(self->dist_freq()) then begin
    	message, 'Framerate mistmach: disturbance designed to operate @ '+strtrim(string(loopfreq,format='(f7.1)'),2), /INFO
    	self._sin_freq_mismatch = 1B
    endif

    return, 1
end

function AOdisturb_sinmode::nsinmodes
    return, self._nmodulatedmodes
end

function AOdisturb_sinmode::sin_mode, idx
    if n_elements(idx) ne 0 then return, (*(self._sin_mode))[idx] else return, *(self._sin_mode)
end

function AOdisturb_sinmode::sin_freq, idx
    if n_elements(idx) ne 0 then return, (*(self._sin_freq))[idx] else return, *(self._sin_freq)
end

function AOdisturb_sinmode::mode_amp, idx
    if n_elements(idx) ne 0 then return, (*(self._mode_amp))[idx] else return, *(self._mode_amp)
end

pro AOdisturb_sinmode::free
end

pro AOdisturb_sinmode::Cleanup
    ;self->AOhelp::Cleanup
    if ptr_valid(self._sin_mode)   then ptr_free, self._sin_mode
    if ptr_valid(self._mode_amp)   then ptr_free, self._mode_amp
    if ptr_valid(self._sin_freq)   then ptr_free, self._sin_freq
end

pro AOdisturb_sinmode::addHelp, obj
    obj->addMethodHelp, "nsinmodes()", "Number of sinusoidally modulated modes"
    obj->addMethodHelp, "sin_mode()", "index of the mode introduced in the sinusoidal disturbance"
    obj->addMethodHelp, "mode_amp()", "amplitude of mode [m surf]"
    obj->addMethodHelp, "sin_freq()", "frequency of the sinusoidal disturbance"
end

pro AOdisturb_sinmode__define
    struct = { AOdisturb_sinmode, $
        _nmodulatedmodes		: 0L		, $		; [Hz]
        _sin_mode               : ptr_new() , $
        _mode_amp               : ptr_new() , $     ; [m]
        _sin_freq               : ptr_new() , $     ; [Hz]
        ;_sin_mode				: 0L		, $		;
		;_mode_amp				: 0.		, $		; [m]
       ; _sin_freq				: 0.		, $		; [Hz]
		_sin_m2c_mismatch		: 0B		, $
		_sin_freq_mismatch		: 0B		  $
    }
end

