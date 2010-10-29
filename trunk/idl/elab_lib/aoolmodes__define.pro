;+
;
; Open Loop Modes (reconstructed from closed-loop data).
;
;-

function AOolmodes::Init, root_obj

	if root_obj->operation_mode() ne 'ONSKY' then return,0

	self._totdelay   = 2L	;Assuming a total of 2 frames delay
	self._decimation = (root_obj->frames_counter())->decimation()
	self._nnMin  =  3L
	self._r0	 =  -1.

	if not total(self._decimation eq [0,1,2]) then begin
		message, 'OLmodes cannot be reconstructed: decimation unknown.',/info
		return, 0
	endif

	if not obj_valid(root_obj->residual_modes()) then begin
	 	message, 'OLmodes cannot be reconstructed: residual modes not available.',/info
		return, 0
	endif

	if not obj_valid(root_obj->modes()) then begin
	 	message, 'OLmodes cannot be reconstructed: integrated modes not available.',/info
		return, 0
	end

	if (self._decimation eq 2) and not obj_valid(root_obj->modalpositions()) then begin
	 	message, 'OLmodes cannot be reconstructed: modal positions not available.',/info
		return, 0
	end

    self._store_fname     = filepath(root=root_obj->elabdir(), 'olmodes.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'olmodes_psd.sav')
    self._r0_store_fname  = filepath(root=root_obj->elabdir(), 'olmodes_r0.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._r0_store_fname, /allow_nonexistent
    endif

    if not self->AOtime_series::Init( (root_obj->frames_counter())->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('[nm-wf Hz^{-1/2}]')
	self._plots_title   = root_obj->tracknum()

	;Keep root_obj to easily retrieve residual_modes(), modes() and modalpositions()
    self._root_obj = root_obj

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOolmodes', 'Represent reconstructed open loop modes') then return, 0
    self->addMethodHelp, "modes()", "reconstructed open loop modes matrix [niter,nmodes]"
    self->addMethodHelp, "nmodes()", "number of modes"
    self->addMethodHelp, "r0([lambda=lambda] [,/PLOT])", "Estimates r0 @ lambda (default 500nm)"
    self->addMethodHelp, "seeing()", "Seeing value (@ 500nm)"
    self->addMethodHelp, "ide(mode_idx, visu=visu)", "Identifies turbulence, vibrations and noise model for mode_idx mode"
    self->AOtime_series::addHelp, self

    return, 1
end

pro AOolmodes::datiProducer
    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
		rmodes = (self._root_obj->residual_modes())->modes()
		imodes = (self._root_obj->modes())->modes()
		sz = size(rmodes,/dim)
		nframes = sz[0]
		nmodes  = sz[1]

		olmodes = fltarr(nframes,nmodes)
		if self._decimation eq 2 then begin
			mpos = (self._root_obj->modalpositions())->modalpositions()
			mpos = mpos[*,0:nmodes-1]
			for ii=0, nmodes-1 do mpos[*,ii] = mpos[*,ii] - mean(mpos[*,ii]) + mean(imodes[*,ii])
			olmodes[1:*,*] = rmodes[1:*,*] + 0.5*( mpos[1:*,*] + imodes[0:nframes-2,0:nmodes-1] )
		endif else begin
			dd = self._totdelay - self._decimation
		    olmodes[dd:*,*] = rmodes[dd:*,*] + imodes[0:nframes-1-dd,0:nmodes-1]
		endelse

        save, olmodes, file=self._store_fname
    endelse
    self._modes = ptr_new(olmodes, /no_copy)
end


function AOolmodes::modes, _extra=ex
    return, self->dati(_extra=ex)
end


function AOolmodes::nmodes
    return, self->AOtime_series::nseries()
end


; to be implemented in AOtime_series subclasses
function AOolmodes::GetDati
    if not ptr_valid(self._modes) then self->datiProducer
    return, self._modes
end

; Estimate r0 from reconstructed open loop data
;-----------------------------------------------------
function AOolmodes::r0, lambda=lambda, PLOT=PLOT
	if self._r0 eq -1. then self->retrieve_r0

	if keyword_set(PLOT) then begin
		olrms = sqrt(self->time_variance()) * 1e9 * self._root_obj->reflcoef()	;nm wf rms
		nmodes = self->nmodes()
		DpupM = ao_pupil_diameter()
		theovarfit = (4.*!PI^2) * (DpupM/self._r0)^(5./3.) * diag_matrix(kolm_mcovar(nmodes+1)) ;rad^2 @ 500nm
		theorms = sqrt(theovarfit) * 500./(2.*!PI)	; nm wf rms
		yrange = minmax([olrms,theorms])
		window,/free
		plot_oo, lindgen(nmodes)+1, olrms, psym=-1, symsize=0.8, charsize=1.5, $
            ytitle='nm wf rms', xtitle='mode number', title=self._root_obj->tracknum(), yrange=yrange
		oplot, lindgen(nmodes)+1, theorms, color=255L
		legend, ['r0 = '+string(self._r0*1e2, format='(f4.1)')+'cm @ 500nm'], /right, charsize=1.2
	endif

	if n_elements(lambda) ne 0 then return, self._r0*(lambda/500e-9)^(6./5.) else return, self._r0
end

pro AOolmodes::retrieve_r0
	if file_test(self._r0_store_fname) then begin
        restore, self._r0_store_fname
        self._r0 = r0fit
        self._nnMin = nnMin
    endif else begin
    	self->estimate_r0
	endelse
end

;Estimate r0 @ 500nm
pro AOolmodes::estimate_r0

	COMMON olmodes_data, olvarMean, theovar1

	olvar = self->time_variance() * (self._root_obj->reflcoef()*2.*!PI/500e-9)^2.	;in rad^2 @ 500nm
	nmodes  = self->nmodes()
	nnMax   = long(sqrt(8L*nmodes-7L)-1L)/2L
	nnRange = [self._nnMin,nnMax]
	Zern_number = lindgen(nmodes)+2
	nn = long(sqrt(8L*Zern_number-7L)-1L)/2L
	nnValues = findgen(nnRange[1]-nnRange[0]+1)+nnRange[0]
	norders = n_elements(nnValues)

	;Experimental data: the average per radial order of the coeff variances is computed:
	olvarMean = fltarr(norders)
	FOR j=0, norders-1 DO BEGIN
    	FOR i = 0, nmodes-1 DO IF nn(i) EQ nnValues[j] THEN olvarMean[j] = olvarMean[j] + olvar[i]
 	   	olvarMean[j] = olvarMean[j] / (nnValues(j)+1.)
	ENDFOR

	;Theoretical data: a single variance per radial order:
	FirstZerns = (nnValues*(nnValues+1)/2)+1   ;Fist zernike of each radial order.
	theovar1 = (diag_matrix(kolm_mcovar(nmodes+1)))[FirstZerns-2]	;D/r0=1

	;Find best fit
	r0a=0.01 & r0b=1.   ;range of r0s in m
	minf_bracket, r0a,r0b,r0c, erra,errb, errc, FUNC_NAME='fit_r0_errfunc'
	minf_parabolic, r0a,r0b,r0c, r0fit, errmin, FUNC_NAME='fit_r0_errfunc'

	;Save results
	self._r0 = r0fit
	nnMin = self._nnMin
	save, r0fit, nnMin, filename=self._r0_store_fname
end

function AOolmodes::seeing
	return, 0.5 / (self->r0()*4.85)
end

function AOolmodes::ide, mode_idx, visu=visu

  if not keyword_set(visu) then visu=0
  
  CC = [-1,255.,255.*50,255.*100,255.*150,255.*256,255.*256*50,255.*256*100,255.*256*150,255.*256*256]
  
  fp = self->findpeaks(mode_idx)
  frv = fp.(0).fr
  idx = where(fp.(0).pw100 ge 1. and fp.(0).fr ge 2.)
  nfr = n_elements(idx)
  f1 = max([frv[idx[0]]-5d,2])
  f2 = min([frv[idx[nfr-1]]+20d,(self->freq())[self->nfreqs()-1]])
  
  if visu eq 1 then plot_oo, self->freq(), (self->psd(mode_idx))
  
  idx1 = closest(f1, self->freq())
  idx2 = closest(f2, self->freq())
  nm = mean((self->psd(mode_idx))[idx2:*])
  vnm = variance((self->psd(mode_idx))[idx2:*])
  fc = 1/self._dt
  noise = nm*fc
  
  p1 = (self->psd(mode_idx))[idx1]
  p2 = (self->psd(mode_idx))[idx2]
  
  f1l = alog10(f1)
  f2l = alog10(f2)
  p1l = alog10(p1)
  p2l = alog10(p2)
  frl = alog10((self->freq())[idx1:idx2-2])
  
  pl=interpol([p1l,p2l],[f1l,f2l],frl)
  
  vectemp1 = (self->psd(mode_idx))
  vectemp2 = (self->psd(mode_idx))
  vectemp1[idx1:idx2-2] = 10.^(pl)
  idx3 = where(vectemp2 gt vectemp1+sqrt(vnm))
  if total(idx3) ne -1 then vectemp2[idx3] = vectemp1[idx3]+sqrt(vnm)
  
  kkk1 = 5
  a = 1.0001d - log_array(1d-4, 5d-1, kkk1)
  kkk2 = 5
  b = log_array(1d-8*(self->psd(mode_idx))(0),1d0*(self->psd(mode_idx))(0),kkk2)
  minvectemp2 = min(vectemp2)
  vectemp2 = vectemp2-nm
  vectemp2[where(vectemp2 le 0)] = minvectemp2
  if visu eq 1 then oplot, self->freq(), vectemp2, col=cc[9]
  turbpar = turb_est2(vectemp2, a, b, res=res)
  fturb = -alog(turbpar[1:2])*fc/2/!pi
  kturb = turbpar[0]
  
  CLT=fltarr(self->nfreqs())
  for k=1,self->nfreqs() do begin
    z = exp(complex(0, !pi/self->nfreqs()*k))
    CP = (1-turbpar(1)*z^(-1))*(1-turbpar(2)*z^(-1))
    CLT(k-1) = abs(turbpar(0)/CP)
  endfor
  
  if visu eq 1 then oplot, self->freq(), CLT, col=cc[5]
  
  fv=fltarr(nfr)
  par=fltarr(nfr,2)
  ppp = 50
  for j=0,nfr-1 do fv[j]=closest(fp.(0).fr[idx[j]], self->freq())
  for i=0,nfr-2 do begin
    vecv=fltarr(self->nfreqs())
    if i eq 0 then vecv[fv[0]-10:min([mean(fv[0:1]),fv[0]+ppp])]=(self->psd(mode_idx))[fv[0]-10:min([mean(fv[0:1]),fv[0]+ppp])] else $
      vecv[max([mean(fv[i-1:i]),fv[i]-ppp]):min([mean(fv[i:i+1]),fv[i]+ppp])]=(self->psd(mode_idx))[max([mean(fv[i-1:i]),fv[i]-ppp]):min([mean(fv[i:i+1]),fv[i]+ppp])]
    if visu eq 1 then print, 'vibration #'+strtrim(i,2)+' frequency ='+strtrim(frv[idx[i]],2)  
    par[i,*]=vib_est(vecv-nm, fp.(0).fr[idx[i]], fc)
    CLTv=fltarr(self->nfreqs())
    omega=2*!pi*frv[idx[i]]
    damp1=par[i,1]*omega
    damp2=omega
    va1 = -real(2 * exp(- damp1 / fc) * cos( sqrt(complex( omega^2 - damp1^2 )) / fc ))
    va2 = exp(- 2 * damp1 / fc)
    vb1 = -real(2 * exp(- damp2 / fc) * cos( sqrt(complex( omega^2 - damp2^2 )) / fc ))
    vb2 = exp(- 2* damp2 / fc)
    for k=1,self->nfreqs() do begin
      z = exp(complex(0, !pi/self->nfreqs()*k))
      CP1 = POLY(z^(-1), [1d, vb1, vb2])
      CP2 = POLY(z^(-1), [1d, va1, va2])
      CLTv(k-1) = abs(CP1/CP2)*par[i,0]
    endfor
    if visu eq 1 then oplot, self->freq(), CLTv, col=cc[1]
  endfor
  vecv=fltarr(self->nfreqs())
  vecv[mean(fv[nfr-2:nfr-1]):min([fv[nfr-1]+ppp,self->nfreqs()])]=(self->psd(mode_idx))[mean(fv[nfr-2:nfr-1]):min([fv[nfr-1]+ppp,self->nfreqs()])]
  if visu eq 1 then print, 'vibration #'+strtrim(nfr-1,2)+' frequency ='+strtrim(frv[idx[nfr-1]],2)  
  par[nfr-1,*]=vib_est(vecv-nm, fp.(0).fr[idx[nfr-1]], fc)
  
  CLTv=fltarr(self->nfreqs())
  omega=2*!pi*frv[idx[nfr-1]]
  damp1=par[nfr-1,1]*omega
  damp2=omega
  va1 = -real(2 * exp(- damp1 / fc) * cos( sqrt(complex( omega^2 - damp1^2 )) / fc ))
  va2 = exp(- 2 * damp1 / fc)
  vb1 = -real(2 * exp(- damp2 / fc) * cos( sqrt(complex( omega^2 - damp2^2 )) / fc ))
  vb2 = exp(- 2* damp2 / fc)
  for k=1,self->nfreqs() do begin
    z = exp(complex(0, !pi/self->nfreqs()*k))
    CP1 = POLY(z^(-1), [1d, vb1, vb2])
    CP2 = POLY(z^(-1), [1d, va1, va2])
    CLTv(k-1) = abs(CP1/CP2)*par[i,0]
  endfor
  if visu eq 1 then oplot, self->freq(), CLTv, col=cc[1]
  
  model={ $
    turb_fr: fturb, $
    turb_g: kturb, $
    noise: noise, $
    vib_fr: fp.(0).fr[idx], $
    vib_g: par[*,0], $
    vib_damp: par[*,1] $
  }  
  
  return, model
end

pro AOolmodes::free
    if ptr_valid(self._modes) then ptr_free, self._modes
    self->AOtime_series::free
end


pro AOolmodes::Cleanup
    if ptr_valid(self._modes) then ptr_free, self._modes
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end


pro AOolmodes__define
    struct = { AOolmodes, $
        _root_obj         : obj_new()	, $
    	_totdelay		  : 0L			, $		;total delay of the system in frames.
    	_decimation		  : 0L		  	, $
        _modes            : ptr_new()	, $
        _store_fname      : ""			, $
    	_nnMin			  : 0L			, $		;minimum radial order used in r0 estimation.
        _r0_store_fname   : ""			, $
        _r0				  : 0.			, $
        INHERITS    AOtime_series		, $
        INHERITS    AOhelp 			$
    }

end

