
;+
;
; LAMBDA_INIT:	Central wavelength (nm) at which the PSFs were taken.
;				NOTE: 	This information is usually retrieved from the filter wheel #2 object in
;						case the CCD47 has been used. However, in the case the status of the FW#2
;						has not been saved properly, this keyword can be used to force LAMBDA_INIT
;						to a given value (most probably taken from the logbook).
; LAMBDA_FINAL:	Wavelength (nm) at which the SR will be estimated.
;-

FUNCTION calc_sr_v1, ref_trackn, trackn, lambda_init = lambda_init, lambda_final = lambda_final


; Get Reference PSF (closed-loop without disturbance)
;---------------------------------------------------------------
ref_obj  = getaoelab(ref_trackn)

if not OBJ_VALID(ref_obj->tv()) then begin
	message, 'Reference PSF not found.', /INFO
	return, -1
endif

if not OBJ_VALID(ref_obj->frames()) then begin
	message, 'Reference CCD39 frames not found.', /INFO
	return, -1
endif

if not OBJ_VALID((ref_obj->wfs_status())->filtw1()) then begin
	message, 'WARNING on Reference data: Position of filter wheel 1 not known.', /INFO
	ref_fw1_pos = -1
endif else ref_fw1_pos = ((ref_obj->wfs_status())->filtw1())->fw_pos()

if not OBJ_VALID((ref_obj->wfs_status())->filtw2()) then begin
	message, 'WARNING on Reference data: Position of filter wheel 2 not known.', /INFO
	ref_fw2_pos = -1
endif else begin
	ref_fw2_pos  = ((ref_obj->wfs_status())->filtw2())->fw_pos()
	ref_fw2_data = ((ref_obj->wfs_status())->filtw2())->fw_data()
	ref_fw2_cw   = ref_fw2_data.cw
endelse

ref_peak = ((ref_obj->tv())->gaussfit())->ampl()
ref_nph  = (ref_obj->frames())->nph_per_sec_av()



; Get AO-corrected PSFs (closed-loop with disturbance)
;-----------------------------------------------------------------
loop_obj = getaoelab(trackn)

if not OBJ_VALID(loop_obj->tv()) then begin
	message, 'AO-corrected PSF not found:'+strtrim(trackn,2), /INFO
	return, -1
endif

if not OBJ_VALID(loop_obj->frames()) then begin
	message, 'AO-corrected CCD39 frames not found:'+strtrim(trackn,2), /INFO
	return, -1
endif

if not OBJ_VALID((loop_obj->wfs_status())->filtw1()) then begin
	message, 'WARNING on Loop data: Position of filter wheel 1 not known.', /INFO
	loop_fw1_pos = -1
endif else loop_fw1_pos = ((loop_obj->wfs_status())->filtw1())->fw_pos()

if not OBJ_VALID((loop_obj->wfs_status())->filtw2()) then begin
	message, 'WARNING on Loop data: Position of filter wheel 2 not known.', /INFO
	loop_fw2_pos = -1
endif else begin
	loop_fw2_pos = ((loop_obj->wfs_status())->filtw2())->fw_pos()
	loop_fw2_data = ((loop_obj->wfs_status())->filtw2())->fw_data()
	loop_fw2_cw   = loop_fw2_data.cw
endelse


loop_peak = ((loop_obj->tv())->gaussfit())->ampl()
loop_nph  = (loop_obj->frames())->nph_per_sec_av()



; Compute Strehl ratio:
;------------------------------------------------------------------

; Verify that filter wheels were at the same position:
if ref_fw1_pos ne loop_fw1_pos then message, 'WARNING: mismatch on filter wheel #1 position!', /INFO
if ref_fw2_pos ne loop_fw2_pos then message, 'WARNING: mismatch on filter wheel #2 position!', /INFO

sr = (loop_peak/loop_nph) / (ref_peak/ref_nph)

print, '-------------------------------------'
print, 'Reference data:'
print, 'PSF peak: ', strtrim(ref_peak,2)
print, 'Flux level (nph/s):', strtrim(ref_nph,2)
print, 'Loop data:'
print, 'PSF peak: ', strtrim(loop_peak,2)
print, 'Flux level (nph/s):', strtrim(loop_nph,2)
print, 'Estimated SR @ imaging wavelength: ', strtrim(sr,2)

IF keyword_set(lambda_final) THEN BEGIN

	IF keyword_set(lambda_init) THEN lin = lambda_init ELSE $
		IF NOT loop_fw2_pos eq -1  THEN	lin = loop_fw2_cw ELSE BEGIN
			message, 'Error: SR conversion not possible. Please specify the imaging wavelength.', /INFO
			return, sr
		ENDELSE

	SR_conversion, sr, lin, lambda_final, sr_final
;	print, 'Estimated SR @ '+strtrim(lambda_final,2)+' ->', sr_final
	sr_out = sr_final

ENDIF ELSE sr_out = sr

print, '-------------------------------------'

return, sr_out

end
