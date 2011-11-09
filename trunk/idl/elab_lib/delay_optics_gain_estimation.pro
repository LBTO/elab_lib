;+
; NAME:
;   delay_optics_gain_estimation
; PURPOSE:
;   Calculate the delay and the wfs gains from the data
;   taken from a tracking number.
; CATEGORY:
;   AO simulation.
; CALLING SEQUENCE:
;
;pro delay_optics_gain_estimation, trckn, mode_idx=mode_idx, optvars=optvars
;
; INPUTS:
;   trckn	tracking number
; KEYWORD
;   mode_idx	index of modes, if not set all the modes will be considered
;   smw		smooth width of the module of the TF determined from the data
; OUTPUTS:
;   optavars	[delay, wfs_gains]
; COMMON BLOCKS:
;   param
; SIDE EFFECTS:
;   None.
; RESTRICTIONS:
;   None
; MODIFICATION HISTORY:
;   Created 02-NOV-2011 by Guido Agapito agapito@arcetri.astro.it
;-

function quadratic_diff, variables
common param_blk, param
; increase the param.step
param.step+=1
; determine the time
etime = systime(1)
; if 5 seconds are passed print, the call number and the time
if etime gt param.old_time+5 then begin
	print, 'call number: '+strtrim(param.step,2)
	print, 'elapsed time: '+strtrim(etime-param.time_init,2)
	param.old_time = etime
endif
; checks that the delay is between 0 and 3 otherwise it return a very big res
if variables[0] lt 0 then return, 1e6*abs(1-variables[0])
if variables[0] gt 3 then return, 1e6*abs(variables[0]-2)
; initialize the res to 0
res=0.
for i=0, n_elements(variables[1:*])-1 do begin
	; check that the wfs gain is between 0 and 2 otherwise it return a very big res 
	if variables[i+1] lt 0 then return, 1e6*abs(1-variables[i+1])
	if variables[i+1] gt 2 then return, 1e6*abs(variables[i+1]-1)
	; determine the TF from the theory
	aocltf, param.fs, param.gain[i], 1, variables[0], wgain=variables[i+1], CLC2M=CLC2M, freq=param.freq
	; increase the res with the res of the mode param.idx[i]
	; that is equal to the l2-norm of the difference of the
	; theoric TF module and the module of the TF determined from the data
	res+=total((CLC2M-smooth(param.TF[*,i],param.smw))^2)
endfor
return, res
end

pro delay_optics_gain_estimation, trckn, mode_idx=mode_idx, smw=smw, optvars=optvars
	
	if not keyword_set(smv) then smw=1
	; take the data with elab_lib
	e = getaoelab(trckn)
	freq = (e->modes())->freq()
	fs = ((e->wfs_status())->ccd39())->framerate()
	; check the modes on which it must determines the WFS gains (one for each mode)
	; and the delay (only one!)
	if  keyword_set(mode_idx) then begin
		if n_elements(mode_idx) eq 1 then begin
			gain = ((e->control())->gain())[mode_idx]
			modal_comm = ( (e->modes())->psd() )[*,mode_idx]   
			modal_meas = ( (e->residual_modes())->psd() )[*,mode_idx]
		endif else begin
			temp = ((e->control())->gain())
			gain = temp[mode_idx] 
			temp = ( (e->modes())->psd() )
			modal_comm =  temp[*,mode_idx]
			temp = ( (e->residual_modes())->psd() )
			modal_meas =  temp[*,mode_idx]
		endelse
	endif else begin
		gain = ((e->control())->gain())
		modal_comm = ( (e->modes())->psd() )   
		modal_meas = ( (e->residual_modes())->psd() )
		mode_idx = findgen(n_elements(gain))
	endelse
	; Module of the closed loop transfer function from modal commands
	; to modal measurements determined from the data
	TF = sqrt(modal_meas/modal_comm)
	plot_oo, freq, smooth(TF[*,0],smw)
	; common to pass the data to the function called by amoeba
	delay = max([(fs-300.)/1000., 0])
	; systime to display the elapsed time
	time = systime(1)
	;parameters needed by the quadratic_diff function
	common param_blk, param
  	
  	param =	{ 				$
		step		: 0,		$
		time_init	: time,		$
		old_time	: time,		$
		smw		: smw,		$
		idx		: mode_idx,	$
		fs		: fs,		$
		gain		: gain,		$
		freq		: freq,		$
		TF		: TF		$
		}
	; tolerance of the amoeba function
	Ftol = 1e-4
	; initial variables guess [delay, wfs gains]
  	P0 = [delay, fltarr(n_elements(gain))+0.5] 
	; characteristic scale
  	scale = fltarr(n_elements(gain)+1)+0.1
	; call of the amoeba function
	nmax=1000L*n_elements(gain)
  	optvars = amoeba(Ftol, FUNCTION_NAME='quadratic_diff', NCALLS=ncalls, NMAX=nmax, P0=P0, SCALE=scale, FUNCTION_VALUE=res)
	; print, delay and wfs gains
  	print, 'delay ', optvars[0]
	if n_elements(gain) gt 1 then begin
		if n_elements(mode_idx) gt 8 then begin
			print, 'min wfs gain', min(optvars[1:*])
			print, 'max wfs gain', max(optvars[1:*])
			print, 'mean wfs gain', mean(optvars[1:*])
		endif else begin
			print, 'wfs gains', optvars[1:*]
		endelse
	endif else begin
		print, 'wfs gain', optvars[1]
	endelse
  	print, 'ncalls ', ncalls
  	print, 'error', res
	; plot the first mode TF vs the one determined from the amoeba optimization
	aocltf, param.fs, param.gain[0], 1, optvars[0], wgain=optvars[1], CLC2M=CLC2M, freq=param.freq
	oplot, freq, CLC2M, col=255
end


