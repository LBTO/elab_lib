
;+
;
;-

function AOdisturbatm::Init, root_obj

    ;Retrieve all disturbance info from header:
    header = self->header()
    nominal_freq = float(aoget_fits_keyword(header, 'overfreq'))
    dist_freq 	 = self->dist_freq()

    self._seeing = float(aoget_fits_keyword(header, 'seeing'))						; [arcsec]
    self._r0	 = float(aoget_fits_keyword(header, 'r0'))							; @ 500nm [m]
    self._L0	 = float(aoget_fits_keyword(header, 'L0'))							; [m]
    self._phase_screen_fname	= strtrim(aoget_fits_keyword(header, 'ph_fname'),2)
    self._angle_ph_translation	= float(aoget_fits_keyword(header, 'angle_w'))		; [degrees]

    self._Vwind	 = float(aoget_fits_keyword(header, 'Vwind'))						; [m/s]
    if dist_freq ne -1. then self._Vwind = self._Vwind*(dist_freq/nominal_freq) else $
    	message, 'Actual disturbance frequency not known. Wind speed assumed to be the nominal one', /INFO

	;Pre-correction of modes:
	self._cor_nmodes = long(aoget_fits_keyword(header, 'nmodes_c'))		; number of precorrected modes
	self._cor_first_mode = long(aoget_fits_keyword(header, 'first_mo'))	; first precorrected mode
	if self._cor_nmodes ne 0 then begin
		cor_m2c = aoget_fits_keyword(header, 'm2c_cor')
    	if cor_m2c ne (root_obj->control())->m2c_fname() then begin
    		message, 'Control M2C is not equal to pre-correction M2C!', /INFO
    		self._cor_m2c_mismatch = 1B
    	endif
	endif

    return, 1
end

function AOdisturbatm::seeing
	return, self._seeing
end

function AOdisturbatm::r0, lambda=lambda
	if keyword_set(lambda) then return, self._r0 * (lambda/0.5e-6)^(6./5.) else return, self._r0
end

function AOdisturbatm::L0
	return, self._L0
end

function AOdisturbatm::Vwind
	return, self._Vwind
end

function AOdisturbatm::phase_screen_fname
	return, self._phase_screen_fname
end

function AOdisturbatm::angle_ph_translation
	return, self._angle_ph_translation
end

; To fully activate the function below we need to decide where to store the phase screens!!!
function AOdisturbatm::phase_screen
    return, 0L
end

function AOdisturbatm::cor_nmodes
	return, self._cor_nmodes
end

function AOdisturbatm::cor_first_mode
	return, self._cor_first_mode
end


; To fully activate the function below we need to decide where to store the phase screens!!!
;function AOdisturbatm::phase_screen_par
;	par_fname = ao_datadir()+path_sep()+self._phase_screen_fname+'.sav'
;	if file_test(par_fname) then restore, par_fname
;	return, par
;end

pro AOdisturbatm::Cleanup
    ;self->AOhelp::Cleanup
end

pro AOdisturbatm::addHelp, obj
    obj->addMethodHelp, "seeing()", "seeing value (float) [arcsec]"
    obj->addMethodHelp, "r0(lambda=lambda)", "Fried parameter (default @ 500nm) (float) [m]"
    obj->addMethodHelp, "L0()", "outer scale (float) [m]"
    obj->addMethodHelp, "Vwind()", "nominal wind speed (float) [m/s]"
    obj->addMethodHelp, "phase_screen_fname()", "Name of phase screen file (string)"
    obj->addMethodHelp, "angle_ph_translation()", "angle of phase screen translation (float) [degrees]"
    obj->addMethodHelp, "cor_nmodes()", "number of pre-corrected modes (long)"
    obj->addMethodHelp, "cor_first_mode()", "number of first pre-corrected mode (long)"
end

pro AOdisturbatm__define
    struct = { AOdisturbatm, $
        _seeing						: 0.		, $		; [arcsec]
        _r0							: 0.		, $		; @ 500nm [m]
        _L0							: 0.		, $		; [m]
        _Vwind						: 0.		, $		; [m/s]
        _phase_screen_fname			: ""		, $
        _angle_ph_translation		: 0.		, $		; [degrees]
        _cor_nmodes 				: 0L		, $		; number of pre-corrected modes
        _cor_first_mode				: 0L		, $		; first mode to be pre-corrected
        _cor_m2c_mismatch			: 0B		  $
    }
end

