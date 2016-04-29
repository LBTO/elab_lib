;+
;
; scales_est function 
;
; calling sequence:
; res=scales_est(init_variables=init_variables, verbose=verbose, plot_psd=plot_psd)
;
; init_variables	:	(keyword) 
;		init_variables[*,0]		->	amoeba function initial values	
;		init_variables[*,1]		->	amoeba function characteristic scale
; verbose			:	(keyword) if set print some info
; doplot			:	(keyword) ATTENTION: the function becomes very slow if this keyword is set
;						if set makes plot of the true and estimated variance
; NOTE: a common block is needed
;
;    common param_blk, param
;    param = {$
;        mode_idx:   mode_idx, $
;        olvar:      olvar, $
;        D:          ao_pupil_diameter(), $
;        doplot:     doplot $
;        }
;
; Created by Guido Agapito agapito@arcetri.astro.it
;
;-

function res_scales_est, variables

	common param_blk, param

	; checks on the variables
	; r0 < 0.01
	if variables[0] lt 0.01 then variables[0] = 0.01
	; L0 < 1
	if variables[1] lt 0.1 then variables[1] = 0.1
	
	r0 = variables[0]
	L0 = variables[1]
	
	; build the candidate varaince
    turb_var = dblarr(n_elements(param.mode_idx))
    rad2arcsec = 3600.d*180.d/!dpi
    L0norm = L0 / param.D
    scale = (param.D/r0)^(5.d/3.d)
    for i=0,n_elements(param.mode_idx)-1 do turb_var[i] = 4* !pi^2 * scale * von_covar(param.mode_idx(i), param.mode_idx(i), L0norm, /double)

	; make a plot if the keyword plot_psd = 1B
	if param.doplot then begin
		window, 0, xsize=800, ysize=480, ypos=578
		plot_oo, param.mode_idx, param.olvar, xtit='!17mode number', ytit='!17variance !4[!17'+textoidl('rad^2@500nm')+'!4]!17', $
			tit='!17turbulence variance'
		oplot, param.mode_idx, turb_var, col=255l
		wait, 0.5
	endif
	
	; computes the error
	;res = total(abs(param.olvar/turb_var - turb_var/turb_var))
	res = total(abs(param.olvar - turb_var))

	return, res
end

function scales_est, init_variables=init_variables, verbose=verbose

	if not keyword_set(verbose) then verbose=0b
	if not keyword_set(doplot) then doplot=0b
	
	; function initial values and characteristic scale
	if n_elements(init_variables) eq 0 then begin
		P0 = [0.2, 25.]
		scale = [0.05, 5.]
	endif else begin
		P0 = init_variables[*,0]
		scale = init_variables[*,1]
	endelse
	
	; max number of tries and tolerance
	nmax = 1e3
	ftol = 1e-3

	optvars = amoeba(Ftol, FUNCTION_NAME='res_scales_est', NCALLS=ncalls, NMAX=nmax, $
			P0=P0, SCALE=scale, FUNCTION_VALUE=residual)

	if verbose then begin
		print, 'ncalls ', ncalls
		print, optvars
		print, 'minResidual ', residual[0]
	endif

	return, optvars

end
