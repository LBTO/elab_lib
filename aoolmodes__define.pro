;+
;
; Open Loop Modes (reconstructed from closed-loop data).
;
;-

function AOolmodes::Init, root_obj

  if root_obj->operation_mode() ne 'ONSKY' then return,0

  if not obj_valid(root_obj->frames_counter()) then begin
    message, 'OLmodes cannot be reconstructed: frames counter not available.',/info
    return, 0
  endif

  delay = (root_obj->delay())->frames_delay()
  self._totdelay   = round(delay)
  ;self._totdelay   = 2L	;Assuming a total of 2 frames delay
  self._decimation = (root_obj->frames_counter())->decimation()
  self._nnMin  =  4L
  self._r0	 =  -1.

  if not total(self._decimation eq [0,1,2,3]) then begin
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
  self._store_peaks_fname = filepath(root=root_obj->elabdir(), 'olmodes_peaks.sav')
  if root_obj->recompute() eq 1B then begin
    file_delete, self._store_fname, /allow_nonexistent
    file_delete, self._store_psd_fname, /allow_nonexistent
    file_delete, self._r0_store_fname, /allow_nonexistent
    file_delete, self._store_peaks_fname, /allow_nonexisten
  endif

  if not self->AOtime_series::Init( (root_obj->frames_counter())->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
  self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
  self._spectra_units = textoidl('nm-wf')
  self._plots_title   = root_obj->tracknum()

  ;Keep root_obj to easily retrieve residual_modes(), modes() and modalpositions()
  self._root_obj = root_obj

  ;Initialize WF
  if not self->AOwf::Init(self._root_obj, self._root_obj->modeShapes()) then message, 'WF object not available', /info

  ; initialize help object and add methods and leafs
  if not self->AOhelp::Init('AOolmodes', 'Represent reconstructed open loop modes') then return, 0
  self->addMethodHelp, "modes()", "reconstructed open loop modes matrix [niter,nmodes]"
  self->addMethodHelp, "nmodes()", "number of modes"
  self->addMethodHelp, "plotJitter(from_freq=from_freq, to_freq=to_freq, _extra=ex)", ""
  self->addMethodHelp, "r0([lambda=lambda] [,/PLOT])", "Estimates r0 @ lambda (default 500nm)"
  self->addMethodHelp, "seeing()", "Seeing value (@ 500nm)"
  self->addMethodHelp, "ide(mode_idx, visu=visu)", "Identifies turbulence, vibrations and noise model for mode_idx mode"
  self->addMethodHelp, "findDirections(from_freq=from, to_freq=to, plot=plot, nfr=nfr, fstep=fstep)", $
                        "return the direction of vibrations (width[Hz]=2*fstep, tip=0°, tilt=90°) between frequencies from_freq and to_freq"
  self->AOwf::addHelp, self
  self->AOtime_series::addHelp, self

  return, 1
end

pro AOolmodes::datiProducer

  if file_test(self._store_fname) then begin
    restore, self._store_fname
  endif else begin
    rmodes = (self._root_obj->residual_modes())->modes()
    if (self._root_obj->wfs_status())->optg() eq 0 then rmodes *= (self._root_obj->residual_modes())->mistmatch_factor()
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

pro AOolmodes::plotJitter, from_freq=from_freq, to_freq=to_freq, _extra=ex, overplot=overplot
  coeff2arcsec = self._root_obj->reflcoef() * 4 / ao_pupil_diameter() / 4.848d-6
  freq = self->freq(from=from_freq, to=to_freq)
  tip  = self->power(0, from=from_freq, to=to_freq, /cum) * coeff2arcsec^2
  tilt = self->power(1, from=from_freq, to=to_freq, /cum) * coeff2arcsec^2
  if not keyword_set(overplot) then begin
  	plot, freq, sqrt(tip + tilt), xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, $
    	title=self._plots_title, xtitle='Freq [Hz]', ytitle='Cumulated PSD [arcsec rms]', _extra=ex
  	oplot, freq, sqrt(tip), col='0000ff'x
  	oplot, freq, sqrt(tilt), col='00ff00'x
  	elab_legend, ['Tilt+Tip', 'Tip', 'Tilt'],linestyle=[0,0,0],colors=[!P.COLOR, '0000ff'x, '00ff00'x]
  endif else begin
   	oplot, freq, sqrt(tip + tilt)
   	oplot, freq, sqrt(tip), col='0000ff'x
   	oplot, freq, sqrt(tilt), col='00ff00'x
  endelse
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
    olrms = sqrt(self->time_variance()) * self->norm_factor()	;nm wf rms
    nmodes = self->nmodes()
    modes_idx = (self._root_obj->modal_rec())->modes_idx()
    olrms = olrms[modes_idx]
    DpupM = ao_pupil_diameter()
    theovarfit = (4.*!PI^2) * (DpupM/self._r0)^(5./3.) * diag_matrix(kolm_mcovar(nmodes+1)) ;rad^2 @ 500nm
    theorms = sqrt(theovarfit) * 500./(2.*!PI)	; nm wf rms
    yrange = minmax([olrms,theorms])
    window,/free
    plot_oo, modes_idx+1, olrms, psym=-1, symsize=0.8, charsize=1.5, $
      ytitle='nm wf rms', xtitle='mode number', title=self._root_obj->tracknum(), yrange=yrange $
      	, xgridstyle=1, ygridstyle=1, xticklen=1, yticklen=1
    oplot, lindgen(nmodes)+1, theorms, color=255L
    elab_legend, ['r0 = '+string(self._r0*1e2, format='(f4.1)')+'cm @ 500nm'], /right, charsize=1.5
  endif

  if n_elements(lambda) ne 0 then return, self._r0*(lambda/500e-9)^(6./5.) else return, self._r0
end

pro AOolmodes::retrieve_r0
  if file_test(self._r0_store_fname) then begin
    restore, self._r0_store_fname
    if n_elements(r0fit) eq 0 then begin
    	file_delete, self._r0_store_fname
    	self->estimate_r0
    endif else begin
    	self._r0 = r0fit
    	self._nnMin = nnMin
    endelse
  endif else begin
    self->estimate_r0
  endelse
end

;Estimate r0 @ 500nm
pro AOolmodes::estimate_r0

  COMMON olmodes_data, olvarMean, theovar1

  olvar = self->time_variance() * (self._root_obj->reflcoef()*2.*!PI/500e-9)^2.	;in rad^2 @ 500nm
  nmodes    = self->nmodes()	;including maybe some non-reconstructed modes
  modes_idx = (self._root_obj->modal_rec())->modes_idx()

  nnMax   = long(sqrt(8L*nmodes-7L)-1L)/2L
  nnRange = [self._nnMin,nnMax-1]
  if nnRange[1]-nnRange[0]+1 le 0 then begin
  	message, 'Not enough OLmodes to perform r0 estimation',/info
    self._r0 = 0.
    return
  endif

  Zern_number = lindgen(nmodes)+2
  nn = long(sqrt(8L*Zern_number-7L)-1L)/2L
  nnValues = findgen(nnRange[1]-nnRange[0]+1)+nnRange[0]
  norders  = n_elements(nnValues)

  ;Experimental data: the average per radial order of the coeff variances is computed:
  olvarMean = fltarr(norders)
  nn_count  = lonarr(norders)
  FOR j=0, norders-1 DO BEGIN
  	FOR i=0, nmodes-1 DO BEGIN
		IF where(modes_idx eq i) eq [-1] then continue
    	IF nn(i) EQ nnValues[j] THEN BEGIN
      		olvarMean[j] = olvarMean[j] + olvar[i]
      		nn_count[j]  = nn_count[j] + 1
    	ENDIF
  	ENDFOR
    ;olvarMean[j] = olvarMean[j] / (nnValues(j)+1.)
    olvarMean[j] = olvarMean[j] / float(nn_count[j])
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

function AOolmodes::ide, mode_idx, visu=visu, only_noise=only_noise

  if not keyword_set(visu) then visu=0
  if not keyword_set(only_noise) then only_noise=0

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
  nm = mean(((self._root_obj->residual_modes())->psd(mode_idx))[idx2:*])
  vnm = variance((self->psd(mode_idx))[idx2:*])
  fc = 1/self._dt
  noise = nm*fc

  if only_noise eq 0 then begin
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
  endif else begin
    model={ $
      turb_fr: -1, $
      turb_g: -1, $
      noise: noise, $
      vib_fr: -1, $
      vib_g: -1, $
      vib_damp: -1 $
      }
  endelse



  return, model
end

function AOolmodes::finddirections, from_freq=from_freq, to_freq=to_freq, plot=plot, nfr=nfr, fstep=fstep, teldir=teldir

  ; teldir return the angle in the telescope coordinates
  IF not keyword_set(plot) THEN plot=0
  IF not keyword_set(fstep) THEN fstep=0.25
  IF not keyword_set(nfr) THEN nfr=5

  if n_elements(from_freq) eq 0 then from_freq = min(self->freq())
  if n_elements(to_freq)   eq 0 then to_freq = max(self->freq())
  if from_freq ge to_freq then message, "from_freq must be less than to_freq"
  if from_freq lt min(self->freq()) then from_freq = min(self->freq())
  if from_freq gt max(self->freq()) then from_freq = max(self->freq())
  if to_freq lt min(self->freq()) then to_freq = min(self->freq())
  if to_freq gt max(self->freq()) then to_freq = max(self->freq())

  idx_from = closest(from_freq, self->freq())
  idx_to   = closest(to_freq, self->freq())
  
  ; return vibrations frequency and power
  peaks=self->findpeaks([0,1], from_freq=from_freq, to_freq=to_freq)

  frtemp=[peaks.(0).fr,peaks.(1).fr]
  pwtemp=[peaks.(0).pw,peaks.(1).pw]
  flag=0
  j=0

  cc = [-1,255.,255.*256,255.*256*256,255.*256*100,255.*100]

  while flag eq 0 do begin
    ; joins vibrations with similar frequency
    idx=where(abs(frtemp - frtemp[j]) lt 0.6)
    if total(idx) ne -1 then begin
      for k=0,n_elements(idx)-1 do begin
        if idx[k] ne j then begin
          if idx[k] lt n_elements(frtemp)-1 then begin
            frtemp=[frtemp[0:idx[k]-1],frtemp[idx[k]+1:*]]
            pwtemp=[pwtemp[0:idx[k]-1],pwtemp[idx[k]+1:*]]
          endif else begin
            frtemp=frtemp[0:idx[k]-1]
            pwtemp=pwtemp[0:idx[k]-1]
          endelse
          idx=idx-1
        endif
      endfor
    endif
    j+=1
    if j gt n_elements(frtemp)-1 then flag=1
  endwhile

  if n_elements(pwtemp) lt nfr then nnn=n_elements(pwtemp) else nnn=nfr
  maxr=dblarr(nnn)
  idxmax=dblarr(nnn)
  fvibmax=dblarr(nnn)
  pow=dblarr(nnn)
  rm1=dblarr(self->niter(),nnn)
  rm2=dblarr(self->niter(),nnn)
  ab=dblarr(2,nnn)
  cor=dblarr(nnn)
  var=dblarr(nnn)
  xy=dblarr(nnn)
  plt=0
  if total(fvibmax gt 0) then begin
    if plot eq 1 then $
      window, /free
    for ijk = 0, nnn-1 do begin
      if ijk eq 0 then pw=pwtemp else pw[idxmax(ijk-1)]=0
      ; sorts the vibrations
      maxr(ijk) = max(pw,idxmaxtemp)
      idxmax(ijk) = idxmaxtemp
      fvibmax(ijk) = frtemp[idxmax(ijk)]
      pow(ijk) = pwtemp[idxmax(ijk)]
      ; isolates the frequency of the ijk-th vibration
      a1t=fft((self->modes())[*,0])
      a2t=fft((self->modes())[*,1])
      p=self->niter()*(self._root_obj->frames_counter())->deltat()

      if p*fstep lt 1 then fstep=1./p
      a1t[0:p*(fvibmax(ijk)-fstep)-1]=0
      a1t[p*(fvibmax(ijk)+fstep):p*(1./(self._root_obj->frames_counter())->deltat()-fvibmax(ijk)-fstep)-1]=0
      a1t[p*(1./(self._root_obj->frames_counter())->deltat()-fvibmax(ijk)+fstep):*]=0
      a2t[0:p*(fvibmax(ijk)-fstep)-1]=0
      a2t[p*(fvibmax(ijk)+fstep):p*(1./(self._root_obj->frames_counter())->deltat()-fvibmax(ijk)-fstep)-1]=0
      a2t[p*(1./(self._root_obj->frames_counter())->deltat()-fvibmax(ijk)+fstep):*]=0
      ; generates the signal with only the frequency of the ijk-th vibration 
      rm1[*,ijk]=fft(a1t,1)
      rm2[*,ijk]=fft(a2t,1)
      ; linear fit
      ab1 = linfit(rm1[*,ijk],rm2[*,ijk])
      ab2 = linfit(rm2[*,ijk],rm1[*,ijk])
      ; errors variance of the linear fit
      cor1 = variance( rm2[*,ijk]-ab1[1]*rm1[*,ijk]-ab1[0] )
      cor2 = variance( rm1[*,ijk]-ab2[1]*rm2[*,ijk]-ab2[0] )
      ; takes the best values
      if cor1 lt cor2 then begin
        cor[ijk]=cor1
        ab[*,ijk]=ab1
        var[ijk] = variance( rm2[*,ijk] )
      endif else begin
        cor[ijk]=cor2
        ab[*,ijk]=ab2
        xy[ijk]=1
        var[ijk] = variance( rm1[*,ijk] )
      endelse
      if ijk eq 0 then begin
        if plot eq 1 then $
          plot, 1.1*minmax([rm1[*,ijk],rm2[*,ijk]]), 1.1*minmax([rm1[*,ijk],rm2[*,ijk]]), $
          xtitle='direction 0', ytitle='direction 1', title='vibrations from '+strtrim(from_freq,2)+'Hz to '+strtrim(to_freq,2)+'Hz', charsize=1.2, /nodata
        if plot eq 1 then $
          oplot, rm1[*,ijk], rm2[*,ijk], psym=3
        frvib= fvibmax(ijk)
        colo=-1
      endif else begin
        if plot eq 1 then $
          oplot, rm1[*,ijk], rm2[*,ijk], psym=3, col=CC[ijk]
        frvib=[frvib, fvibmax(ijk)]
        colo=[colo,CC[ijk]]
      endelse
      if plot eq 1 then begin
        if fvibmax(ijk) lt to_freq and fvibmax(ijk) gt from_freq then begin
          if xy[ijk] eq 0 then oplot, 1.2*minmax(rm1[*,0]), ab[1,ijk]*1.2*minmax(rm1[*,0])+ab[0,ijk], col=CC[ijk] $
          else oplot, ab[1,ijk]*1.2*minmax(rm2[*,0])+ab[0,ijk], 1.2*minmax(rm2[*,0]), col=CC[ijk]
        endif
      endif
    endfor
    if plot eq 1 then $
      elab_legend, strtrim(frvib,2)+'Hz', psym=fltarr(nnn)-1, col=colo
    angle=( (-1)^xy*atan(ab[1,*])+xy*!pi/2 )*!CONST.RtoD
    ; if key then return the angle in the telescope coordinates
    if keyword_set(teldir) then begin
    	ttangle=(self._root_obj->control())->ttdirections()
    	if (ttangle[1] lt ttangle[0]+95) and (ttangle[1] gt ttangle[0]+85) then stt=1 else stt=-1
 	att = mean([ttangle[0],ttangle[1]-stt*90.])
    	angle=att+stt*angle
    endif
  endif else begin
    frvib=-1
    pow=-1
    cor=-1
    var=-1
    angle=-1
  endelse

  directions={$
    freq: frvib, $
    power: pow, $
    error_var: cor, $
    signal_var: var, $
    angle: angle $
    }

  return, directions
end


pro AOolmodes::tf, idx, mtf=mtf, theoric=theoric, gain=gain, mtf_theoric=mtf_theoric, smooth_factor=smooth_factor
    freq = self->freq()
    psd_res = (self._root_obj->residual_modes())->psd() 
    psd_ol = self->psd() 
    mtf = sqrt( psd_res / psd_ol )

    if n_elements(smooth_factor) gt 0 then for i=0,n_elements(mtf[0,*])-1 do mtf[*,i] = smooth(reform(mtf[*,i]),smooth_factor,/edge_tru)

    ;window, /free, xs=640, ys=480
    ;plot_io, findgen(n_elements(mtf[0,*])), mtf[0,*], tit='!3'+(self._root_obj->obj_tracknum())->tracknum()+'!17', xtit='!17mode number!17', ytit='!17amplitude at min. freq.', /xst

    window, /free, xs=640, ys=480
    plot_oo, freq, mtf[*,idx], tit='!3'+(self._root_obj->obj_tracknum())->tracknum()+'!17 TF (mode '+strtrim(round(idx),2)+')', xtit='!17frequency !4[!17Hz!4]!17', ytit='!17amplitude', /xst
    oplot, minmax(freq), [1,1], line=2
    
    if keyword_set(theoric) then begin
        if n_elements(gain) eq 0 then gainTF = ((self._root_obj->control())->gain())[idx] else gainTF = gain
        ff = 1
        fs = ((self._root_obj->wfs_status())->camera())->framerate()
        delay = (self._root_obj->delay())->delay()*fs

        iir = elab_int_iir([((self._root_obj->control())->gain())[idx],gainTF], ff=ff)
        if delay ge 2 then dm_delay = 1. else dm_delay = delay - fix(delay)
        wfs_delay = delay - dm_delay 

        ;; DM tf
        tf_dm = discrete_delay_tf(dm_delay)
        nm = tf_dm[*,0]
        dm = tf_dm[*,1]
        ;; WFS tf
        tf_wfs = discrete_delay_tf(wfs_delay)
        nw = tf_wfs[*,0]
        dw = tf_wfs[*,1]
        plot_iir_tf, iir, fs, [0], nm=nm, dm=dm, nw=nw, dw=dw, /no, freq=freq, ytmtf=mtf_theoric0

        oplot, freq, mtf_theoric0, col=255l, thick=2
        if n_elements(gain) ne 0 then begin
            plot_iir_tf, iir, fs, [1], nm=nm, dm=dm, nw=nw, dw=dw, /no, freq=freq, ytmtf=mtf_theoric1
            oplot, freq, mtf_theoric1, col=255l*256l, thick=2
            mtf_theoric = mtf_theoric1
            elab_legend, ['data MTF','theor. MTF','th. MTF (g='+strtrim(string(gainTF,format='(f9.2)'),2)+')'], $
                              col=[1l,255l,255*256l], linest=[0,0,0], thick=2, /bottom, /right, /clear, charsize=1.5
        endif else begin
            mtf_theoric = mtf_theoric0
            elab_legend, ['data MTF','theor. MTF'], col=[1l,255l], linest=[0,0], thick=2, /bottom, /right, /clear, charsize=1.5
        endelse
    endif
end


pro AOolmodes::free
  if ptr_valid(self._modes) then ptr_free, self._modes
  self->AOwf::free
  self->AOtime_series::free
end


pro AOolmodes::Cleanup
  if ptr_valid(self._modes) then ptr_free, self._modes
  self->AOwf::Cleanup
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
    _wf			   	  : obj_new()   , $
    INHERITS    AOwf                , $
    INHERITS    AOtime_series       , $
    INHERITS    AOhelp 		          $
    }

end

