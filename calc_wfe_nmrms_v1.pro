;+
;
; Computes WFE (nm rms) from loops controlling mirror modes
;
;-

function calc_wfe_nmrms_v1, trackn, VISU=VISU, precalc_wf_nmrms_ho = precalc_wf_nmrms_ho, sr_out=sr_out, lambda_im_nm = lambda_im_nm

IF KEYWORD_SET(VISU) THEN loadct,3

; imaging wavelength (for sr_out estimation)
if not keyword_set(lambda_im_nm) then lim = 1650. else lim=lambda_im_nm	;[nm] in IRTC
									  ;lim = 850.	;[nm] in CCD47

; Get lab data:
print, '---------------------------------'
print, '...reading loop data: ', trackn
loop1 = getaoelab(trackn)

if OBJ_VALID(loop1->frames()) then print, 'Number of photons/subap/frame: ', (loop1->frames())->nphsub_per_int_av()
; Scale factor from mirror modes coeffs to opd in meters.
Refl_coeff = 4.
mirmodes_rms = 1./sqrt(653.)
mircoeff2pos  = Refl_coeff ; * mirmodes_rms	;

;restore optical interaction matrix:
restore, !ao_env.root+path_sep()+'phase_maps'+path_sep()+'MMmatrix_before_optical_calib.sav'
nacts = (size(PHmatrix,/dim))[0]
print, 'number of active actuators: ', strtrim(nacts,2)


nmodes = (loop1->modal_rec())->nmodes()
print, 'number of controlled modes: ', strtrim(nmodes,2)

if n_elements(precalc_wf_nmrms_ho) eq 0 then begin

	; Retrieve disturbance:
	;-------------------------------------------------------
	print, '...analyzing disturbance file'

	; Retrieve disturbance command history
	disturb_file = (loop1->adsec_status())->disturb_file()
	dist_com_hist = readfits(!ao_env.datadir+path_sep()+disturb_file, /SILENT)
	niter1 = (size(dist_com_hist,/dim))[0]

	; Compute the disturbance history in mirror-modes coefficients
	m2c_file = (loop1->adsec_status())->m2c_file()
	m2c = readfits(!ao_env.datadir+path_sep()+m2c_file, /SILENT)
	dist_mcoeff_hist = transpose(m2c) ## dist_com_hist

	dist_mcoeff_hist = dist_mcoeff_hist * mircoeff2pos


	; Compute wf_ho statistics:
	;-------------------------------------------------------
	wfvar_m_ho = fltarr(niter1)
	for ii=0L, niter1-1 do begin
		wf_ho = PHmatrix[nmodes:*,*] ## dist_mcoeff_hist[ii,nmodes:nacts-1]
		wfvar_m_ho[ii] = variance(wf_ho)
		if keyword_set(visu) then begin
			if ii eq 0 then wftemp = fltarr(Dpix,Dpix)
			wftemp[idx_gmask] = wf_ho
			image_show, wftemp, /as,/sh
		endif
	;	attente
	endfor

	wf_nmrms_ho = mean(sqrt(wfvar_m_ho)*1e9)

endif else wf_nmrms_ho = precalc_wf_nmrms_ho

print, 'Disturbance file: ', (loop1->adsec_status())->disturb_file()
print, 'high-order wf residuals (nm rms): ', strtrim(wf_nmrms_ho,2)

; Compute wf residuals statistics:
;--------------------------------------------------------
print, '...analyzing loop data'
modi   = (loop1->residual_modes())->modes()
modi   = modi * mircoeff2pos
niter  = (size(modi,/dim))[0]

wfvar_m_res = fltarr(niter)
for ii=0L, niter-1 do begin
	wf_res = PHmatrix[0:nmodes-1,*] ## modi[ii,*]
	wfvar_m_res[ii] = variance(wf_res)
	if keyword_set(visu) then begin
		if ii eq 0 then wftemp = fltarr(Dpix,Dpix)
		wftemp[idx_gmask] = wf_res
		image_show, wftemp, /as,/sh
	endif
endfor

wf_nmrms_res = mean(sqrt(wfvar_m_res)*1e9)
print, 'corrected wf residuals (nm rms): ', wf_nmrms_res


; Estimate the SR:
;---------------------------------------------------------

;total WFE (nm rms)
wf_nmrms_tot = sqrt(wf_nmrms_res^2. + wf_nmrms_ho^2.)
print, 'Final wfe (nm rms) :', strtrim(wf_nmrms_tot,2)

;Total WFE (rad rms @ lwfs)
lwfs = 750.	;nm
wf_radrms_tot = (2.*!PI/lwfs) * wf_nmrms_tot
print, 'Final wfe (rad rms @ 750nm): ', strtrim(wf_radrms_tot,2)

SR_out = exp( -(lwfs/lim)^2. * wf_radrms_tot^2.)
print, 'SR estimated @ '+strtrim(lim,2)+'nm :', strtrim(sr_out,2)

elabdir = loop1->elabdir()
save, lwfs, lim, sr_out, wf_nmrms_res, wf_nmrms_ho, wfvar_m_res, wfvar_m_ho $
	, filename=elabdir+'wfe_data.sav'

return, [wf_nmrms_res, wf_nmrms_ho]
end