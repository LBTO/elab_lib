
;+
;
;-
function AOdisturb_sinmode::Init, root_obj

    ;Retrieve all disturbance info from header:
    hdr = self->header()
	self._sin_freq = float(get_fits_keyword(hdr, 'FREQEFF'))
	self._sin_mode = long(get_fits_keyword(hdr, 'MODE'))
	self._mode_amp = float(get_fits_keyword(hdr, 'AMP'))

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

function AOdisturb_sinmode::sin_mode
	return, self._sin_mode
end

function AOdisturb_sinmode::sin_freq
	return, self._sin_freq
end

function AOdisturb_sinmode::mode_amp
	return, self._mode_amp
end

pro AOdisturb_sinmode::Cleanup
    ;self->AOhelp::Cleanup
end

pro AOdisturb_sinmode::addHelp, obj
    obj->addMethodHelp, "sin_freq()", "frequency of the sinusoidal disturbance"
    obj->addMethodHelp, "sin_mode()", "index of the mode introduced in the sinusoidal disturbance"
    obj->addMethodHelp, "mode_amp()", "amplitude of mode [m surf]"
end

pro AOdisturb_sinmode__define
    struct = { AOdisturb_sinmode, $
        _sin_freq				: 0.		, $		; [Hz]
        _sin_mode				: 0L		, $		;
		_mode_amp				: 0.		, $		; [m]
		_sin_m2c_mismatch		: 0B		, $
		_sin_freq_mismatch		: 0B		  $
    }
end

