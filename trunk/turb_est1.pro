;+
;
; turb_est1 function 
;
; calling sequence:
; res=turb_est1(freq, psd, init_variables=init_variables, verbose=verbose, plot_psd=plot_psd)
;
; freq				:	(input) frequency vector
; psd				:	(input) PSD vector
; init_variables	:	(keyword) 
;		init_variables[*,0]		->	amoeba function initial values	
;		init_variables[*,1]		->	amoeba function characteristic scale
; verbose			:	(keyword) if set print some info
; plot_psd			:	(keyword) ATTENTION: the function becomes very slow if this keyword is set
;						if set makes plot of the true and estimated PSD
;
; Created by Guido Agapito agapito@arcetri.astro.it
;
;-

function res_psd_est, variables

	common param_blk, param

	; checks on the variables
	; cut-off frequency > min(param.freq) > 0.3
    min_freq = max([min(param.freq),param.minfreq])
	if variables[0] lt min_freq then begin
        variables[0] = min_freq
        variables[3] = 0.
    endif
	; power > 0
	if variables[1] le 0. then variables[1] = 1.
	; noise frequency > cut-off frequency + 5 > 0
	if variables[2] lt variables[0]+5. then variables[2] = variables[0]+5.
	; noise frequency < max(param.freq)*4./5.
	if variables[2] gt max(param.freq)/5.*4. then variables[2] = max(param.freq)/5.*4.
    ; coeff<0.05 or >2/3
    if variables[3] lt 0. then variables[3] = 0.
    if variables[3] gt 0.67 then variables[3] = 0.67
	
	f_cut = variables[0]
	power = variables[1]
	f_noise = variables[2]
    k = -1*variables[3]
	
	; build the candidate PSD
	est_psd = dblarr(param.n)+0d
    if f_cut lt f_noise then begin
	    idx_f_cut = closest(f_cut,param.freq)
	    idx_f_noise = closest(f_noise,param.freq)
	    est_psd[0:idx_f_cut] = param.freq[0:idx_f_cut]^k
	    est_psd[idx_f_cut+1:idx_f_noise+1] = param.freq[idx_f_cut]^k/param.freq[idx_f_cut]^(-17./3.)*param.freq[idx_f_cut+1:idx_f_noise+1]^(-17./3.)
	    temp_tot = total(est_psd)
	    est_psd[idx_f_noise+1:*] = est_psd[idx_f_noise+1]
	    est_psd = est_psd * power / temp_tot
	endif

	; make a plot if the keyword plot_psd = 1B
	if param.plot_psd then begin
		window, 11, xsize=800, ysize=480, ypos=578
		plot_oo, param.freq, param.psd, xtit='!17frequency !4[!17Hz!4]!17', ytit='!17power', $
			tit='!17power: '+strtrim(power,2)+', fr. cut: '+ $
				strtrim(string(f_cut,format='(f4.1)'),2)+', fr. noise: '+ $
				strtrim(string(f_noise,format='(f5.1)'),2)+', k: '+ $
				strtrim(string(k,format='(f5.2)'),2)
		oplot, param.freq, est_psd, col=255l
		wait, 0.5
	endif
	
	; computes the error
    ;w = (findgen(param.n)+1)/param.n*(max(param.psd)/min(param.psd))^0.25
    ;res = total(abs(param.psd*w - est_psd*w))
    res = total(abs(param.psd - est_psd))

	return, res
end

function turb_est1, freq, psd, init_variables=init_variables, minfreq=minfreq, verbose=verbose, plot_psd=plot_psd

	if not keyword_set(verbose) then verbose=0b
	if not keyword_set(plot_psd) then plot_psd=0b
	if n_elements(minfreq) eq 0 then minfreq=0.3
	
	; function initial values and characteristic scale
	if n_elements(init_variables) eq 0 then begin
		P0 = [10., total(psd), 25., 0.05]
		scale = [5., total(psd)/10., 5., 0.05]
	endif else begin
		P0 = init_variables[*,0]
		scale = init_variables[*,1]
	endelse
	
	; max number of tries and tolerance
	nmax = 1e3
	ftol = total(psd)*1e-1

	n = n_elements(psd)

	; common block to pass parameters to the function res_psd_est
	common param_blk, param

	param = {$
		n:			n, $
		freq:		freq, $
        minfreq:    minfreq, $
		psd:		psd, $
		plot_psd:	plot_psd $
		}
	
	optvars = amoeba(Ftol, FUNCTION_NAME='res_psd_est', NCALLS=ncalls, NMAX=nmax, $
			P0=P0, SCALE=scale, FUNCTION_VALUE=residual)

	if verbose then begin
		print, 'ncalls ', ncalls
		print, optvars
		print, 'minResidual ', residual[0]
	endif

	return, optvars

end
