;+
; This function is invoked by psf_fwhm_to_seeing function.
;-
FUNCTION fwhm_toko_func, r0in
	COMMON fwhm_toko_params, lambda, L0val, FWHMval
	return, 0.9759*lambda*1e6/(r0in*4.8481) * sqrt(1.-2.183*(r0in/L0val)^0.356) - FWHMval
END


;+
; This function estimates the seeing value (@ 500nm) from the FWHM measured on a PSF acquired at lim wavelength.
;
; INPUTS:
; fwhm: PSF fwhm [arcsec]
; lim:  PSF wavelength [m]
; L0:   vector of outer scale values [m]
;
;-
FUNCTION psf_fwhm_to_seeing, fwhm, lim, L0=L0

	if n_elements(L0) eq 0 then L0 = [20.,50.,80.]
	nL0 = n_elements(L0)

	COMMON fwhm_toko_params, lambda, L0val, FWHMval
	lambda  = lim
	FWHMval = fwhm

	;Estimates r0 @ imaging wavelength
	r0guess = 0.9759*lim*1e6/([0.5*fwhm,fwhm,1.5*fwhm]*4.8481)
	r0im = fltarr(nL0)
	for ii=0, nL0-1 do begin
		L0val = L0[ii]
		r0im[ii] = fx_root(r0guess,'fwhm_toko_func')
	endfor

	;Scales r0 to 500 nm (official lambda for seeing values)
	r0 = r0im*(0.5e-6/lim)^(6./5.)

	;Computes the seeing value
	seeing = 0.9759* 0.5/(r0*4.8481)

	return, seeing
end