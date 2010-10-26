
;+
;
;-

function AOdisturbvib::Init, root_obj

    ;Retrieve all disturbance info from header:
    header = self->header()
    nominal_freq = float(aoget_fits_keyword(header, 'overfreq'))

    m2c = aoget_fits_keyword(header, 'm2c')
    if m2c ne (root_obj->control())->m2c_fname() then begin
    	message, 'Control M2C is not equal to Vibration disturb M2C!', /INFO
    	self._m2c_mismatch = 1B
    endif

    self._totnvib = float(aoget_fits_keyword(header, 'totnvib'))
    self._casevib = float(aoget_fits_keyword(header, 'casevib'))
    if self._casevib eq 0 then begin
	for i=1, self._totnvib do begin
		mode = float(aoget_fits_keyword(header, 'mode'+strtrim(i,2)))
		fr 	 = float(aoget_fits_keyword(header, 'fr'+strtrim(i,2)))
		if self._dist_freq ne -1. then fr *= (self._dist_freq/nominal_freq)
		sd   = float(aoget_fits_keyword(header, 'stddev'+strtrim(i,2)))
		damp = float(aoget_fits_keyword(header, 'damp'+strtrim(i,2)))
		if i eq 1 then begin
			vib = {mode: mode, fr: fr, sd: sd, damp: damp}
			vibration = replicate(vib, self._totnvib)
		endif
		vibration[i-1] = {mode: mode, fr: fr, sd: sd, damp: damp}
   	 endfor
    endif
    if self._casevib eq 1 then begin
	vibration = float(aoget_fits_keyword(header, 'datavib'))
    endif
    self._vibrations = ptr_new(vibration, /no_copy)
    return, 1
end

function AOdisturbatm::totnvib
	return, self._totnvib
end

function AOdisturbatm::casevib
	return, self._casevib
end

function AOdisturbatm::vibrations
	if ptr_valid(self._vibrations) then return, *(self._vibrations) else return, ''
end

pro AOdisturbatm::Cleanup
    ptr_free, self._vibrations
    ;self->AOhelp::Cleanup
end

pro AOdisturbvib::addHelp, obj
    obj->addMethodHelp, 'totnvib', 'total number of vibrations on all modes'
    obj->addMethodHelp, 'casevib', '0 vibration coefficents, 1 temporal data'
    obj->addMethodHelp, 'vibrations', $
    'array of structures: {mode: no. of mode, fr: frequency, sd: stddev [m], damp: damping coefficent}, or path of fits file'
end

pro AOdisturbvib__define
    struct = { AOdisturbvib, $
        _totnvib			: 0.		, $
        _casevib			: 0.		, $
        _vibrations			: ptr_new()	, $
        _m2c_mismatch		: 0B		  $
}
end

