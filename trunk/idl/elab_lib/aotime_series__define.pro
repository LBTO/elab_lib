
;+
; Dati is a 2D array. Each row of the array contains a time-series of niter time-steps.
; As an example, a 10000 frames loop measure will have slopes in the form [10000,1600],
; commands as [10000,672] and so on.
; dt          : interval between time-steps (s)
;
; variance and average are computed in both directions: timeaverage is obtained
; averaging in the time-direction (so the result is an array of nseries elements), while
; ensembleaverage is obtained by averaging on the series at constant time (so the result is an
; array of niter elements)
;
; obviously, spectra is computed only in the direction of time
;-

function AOtime_series::Init, dt, fftwindow=fftwindow, nwindows=nwindows;, nospectra=nospectra
    self._dt = dt
    self._niter=-1
    self._nseries=-1
    self._nfreqs=-1
    self._nspectra=-1
    if n_elements(fftwindow) ne 0 then self._window = fftwindow else self._window = "hamming"
    if n_elements(nwindows) ne 0 then self._nwindows = nwindows else self._nwindows = 1L
	self._norm_factor = 1.
	self._spectra_units = 'a.u.'
    ;if n_elements(nospectra) ne 0 then self._nospectra = 1B else self._nospectra = 0B

    return, 1
end


pro AOtime_series::Compute
    dati = self->GetDati()
    if test_type(dati, /pointer) ne 0 then message, 'AOtime_series subclass::GetDati must return a pointer to float 1/2D array'

    ; compute all AOtime_series stuff here
    ss = size(*dati)
    self._niter   = ss[1]
    self._nseries = ss[0] eq 1 ? 1 : ss[2]

    ; time average & variance
    timevariance  = fltarr(self._nseries)
    timeaverage   = fltarr(self._nseries)
    for ii=0L, self._nseries-1 do begin
        timevariance[ii] = variance( (*dati)[*,ii])
        timeaverage[ii] = mean( (*dati)[*,ii])
    endfor
    self._time_variance = ptr_new(timevariance, /no_copy)
    self._time_average  = ptr_new(timeaverage, /no_copy)

    ; ensemble average & variance
    ensemblevariance  = fltarr(self._niter)
    ensembleaverage   = fltarr(self._niter)
    for ii=0L, self._niter-1 do begin
        ensemblevariance[ii] = (self._nseries eq 1) ? !values.f_nan :variance( (*dati)[ii,*])
        ensembleaverage[ii] = mean( (*dati)[ii,*])
    endfor
    self._ensemble_variance = ptr_new(ensemblevariance, /no_copy)
    self._ensemble_average  = ptr_new(ensembleaverage, /no_copy)

    ; compute spectra
    ;if self._nospectra eq 0B then self->SpectraCompute
end

pro AOtime_series::SpectraCompute
    if file_test(self._store_psd_fname) then begin
        restore, self._store_psd_fname
    endif else begin
        dati = self->GetDati()
        if test_type(dati, /pointer) ne 0 then message, 'AOtime_series subclass::GetDati must return a pointer to float 1/2D array'

		npoints  = n_elements((*dati)[*,0])
        nfreqs   = npoints/(2*self._nwindows)
        nspectra = n_elements((*dati)[0,*])
        dati_psd = fltarr(nfreqs, nspectra)

        for i=0L,nspectra-1 do begin
          psd1 = fltarr(nfreqs)
          for j=0L,self._nwindows-1 do begin
         	  dati1d_raw = (*dati)[j*nfreqs*2:(j+1)*nfreqs*2-1,i]
              case strlowcase(self._window) of
                  ""         :  begin & dati1d = dati1d_raw & winnorm = 1.0 & end
                  "hanning"  :  begin & dati1d = HANNING(2*nfreqs) * dati1d_raw & winnorm = 1.0/0.375  & end ; winnorm = 1./mean(hanning(10)^2)
                  "hamming"  :  begin & dati1d = HANNING(2*nfreqs, alpha=0.56) * dati1d_raw & winnorm = 1.0/0.4104 & end
              endcase
        	  mv = mean(dati1d)
        	  dati1d = dati1d - mv
              fft1, dati1d, self._dt, fspec=fspec, psd=psd, /noplot
              psd1 += psd
          endfor
          dati_psd[*,i] = psd1 * winnorm/self._nwindows
        endfor

        ; save psd
        save, dati_psd, fspec, nfreqs, nspectra, file=self._store_psd_fname
    endelse

    if (ptr_valid(self._psd))  THEN ptr_free, self._psd
    if (ptr_valid(self._freq)) THEN ptr_free, self._freq
    self._psd      = ptr_new(dati_psd, /no_copy)
    self._freq     = ptr_new(fspec, /no_copy)
    self._nfreqs   = nfreqs
    self._nspectra = nspectra
end

function AOtime_series::GetDati
    message, 'AOtime_series::GetDati is abstract'
end


function AOtime_series::dati, series_idx=series_idx, iter_idx=iter_idx
    dati = self->GetDati()
    if ptr_valid(dati) then begin
        if n_elements(series_idx) ne 0 and n_elements(iter_idx) ne 0 then return, (*dati)[iter_idx, series_idx]
        if n_elements(series_idx) ne 0 then return, (*dati)[*, series_idx]
        if n_elements(iter_idx) ne 0 then return, (*dati)[iter_idx, *]
        return, *dati
   endif
end

function AOtime_series::deltat
    return, self._dt
end

function AOtime_series::niter
    if self._niter eq -1 then self->Compute
    return, self._niter
end

function AOtime_series::nseries
    if self._nseries eq -1 then self->Compute
    return, self._nseries
end

function AOtime_series::time_variance, series_idx
    if not ptr_valid(self._time_variance) then self->Compute

    if n_elements(series_idx) ne 0 then return, (*self._time_variance)[series_idx] $
    else return, *self._time_variance
end

function AOtime_series::ensemble_variance, iter_idx
    if not ptr_valid(self._ensemble_variance) then self->Compute

    if n_elements(iter_idx) ne 0 then return, (*self._ensemble_variance)[iter_idx] $
    else return, *self._ensemble_variance
end

function AOtime_series::time_average, series_idx
    if not ptr_valid(self._time_average) then self->Compute

    if n_elements(series_idx) ne 0 then return, (*self._time_average)[series_idx] $
    else return, *self._time_average
end

function AOtime_series::ensemble_average, iter_idx
    if not ptr_valid(self._ensemble_average) then self->Compute
    if n_elements(iter_idx) ne 0 then return, (*self._ensemble_average)[iter_idx] $
    else return, *self._ensemble_average
end

function AOtime_series::nfreqs
    if self._nfreqs eq -1 then self->SpectraCompute
    return, self._nfreqs
end

function AOtime_series::nspectra
    if self._nspectra eq -1 then self->SpectraCompute
    return, self._nspectra
end

function AOtime_series::psd, spectrum_idx
    IF not (PTR_VALID(self._psd)) THEN self->SpectraCompute
    if n_elements(spectrum_idx) eq 0 then return, *(self._psd) else return,  (*(self._psd))[*,spectrum_idx]
end

function AOtime_series::freq, from_freq=from_freq, to_freq=to_freq
    IF not (PTR_VALID(self._freq)) THEN self->SpectraCompute
    IF (PTR_VALID(self._freq)) THEN begin

        if n_elements(from_freq) eq 0 then from_freq = min(*(self._freq))
        if n_elements(to_freq)   eq 0 then to_freq = max(*(self._freq))
        if from_freq ge to_freq then message, "from_freq must be less than to_freq"
        if from_freq lt min(*(self._freq)) then from_freq = min(*(self._freq))
        if from_freq gt max(*(self._freq)) then from_freq = max(*(self._freq))
        if to_freq lt min(*(self._freq)) then to_freq = min(*(self._freq))
        if to_freq gt max(*(self._freq)) then to_freq = max(*(self._freq))

        idx_from = closest(from_freq, *(self._freq))
        idx_to   = closest(to_freq, *(self._freq))

        return, (*(self._freq))[idx_from:idx_to]

    endif else begin
        return, 0d
    endelse
end

function AOtime_series::fftwindow
	return, self._window
end

pro AOtime_series::set_fftwindow, fftwindow
	if n_params() ne 1 then begin
		message, 'Missing parameter: Usage: (...)->set_fftwindow, fftwindow', /info
		return
	endif
	if fftwindow ne '' and fftwindow ne 'hamming' and fftwindow ne 'hanning' then begin
		message, "Window type not recognized. Valid types: '', 'hamming', 'hanning'", /info
		return
	endif
	if fftwindow eq self._window then return else begin
		self._window = fftwindow
		self->AOtime_series::ForceCompute
	endelse
end

function AOtime_series::nwindows
	return, self._nwindows
end

function AOtime_series::norm_factor
	return, self._norm_factor
end

pro AOtime_series::SpecPlot, elemnum, _extra=ex

	if n_params() ne 1 then begin
		message, 'Missing parameter. Usage: ...->SpecPlot, elemnum', /info
		return
	endif

	nspectra = self->nspectra()
	if elemnum ge nspectra then begin
		message, 'Element number requested not available. The last element available is '+strtrim(nspectra-1,2), /info
		return
	endif

	freq = self->freq()
	if freq[1]-freq[0] eq 1 then xtitle='frequency bin' else xtitle='frequency [Hz]'
	psd = self->psd() * (self._norm_factor)^2.
  	yrange = sqrt(minmax(psd[1:*,elemnum]))

	loadct,39,/silent
	!X.MARGIN = [12, 3]
	title =self._plots_title+', element '+strtrim(elemnum,2)
	plot_oo, freq[1:*], sqrt(psd[1:*,elemnum]), charsize=1.2, xtitle=xtitle, ytitle=textoidl('['+self._spectra_units+'  Hz^{-1/2}]') $
		, title=title, yrange=yrange, ytickformat='(e9.1)', _extra=ex

end

;pro AOtime_series::PowerPlot, elemnum, _extra=ex
;
;	if n_params() ne 1 then begin
;		message, 'Missing parameter. Usage: ...->SpecPlot, elemnum', /info
;		return
;	endif
;
;	nspectra = self->nspectra()
;	if elemnum ge nspectra then begin
;		message, 'Element number requested not available. The last element available is '+strtrim(nspectra-1,2), /info
;		return
;	endif
;
;	freq = self->freq()
;	if freq[1]-freq[0] eq 1 then xtitle='frequency bin' else xtitle='frequency [Hz]'
;	data = self->power(0, /cum) * (self._norm_factor)^2.
;  	yrange = minmax(data)
;
;	;loadct,39,/silent
;	!X.MARGIN = [12, 3]
;	title =self._plots_title+', element '+strtrim(elemnum,2)
;	plot_oo, freq[1:*], data, charsize=1.2, xtitle=xtitle, ytitle=textoidl('['+self._spectra_units+'^2]') $
;		, title=title, yrange=yrange, ytickformat='(e9.1)', _extra=ex
;
;end

;
function AOtime_series::power, spectrum_idx, from_freq=from_freq, to_freq=to_freq, cumulative=cumulative
    IF not (PTR_VALID(self._freq)) THEN self->SpectraCompute
    IF not (PTR_VALID(self._psd)) THEN self->SpectraCompute

    if n_elements(from_freq) eq 0 then from_freq = min(*(self._freq))
    if n_elements(to_freq)   eq 0 then to_freq = max(*(self._freq))
    if from_freq ge to_freq then message, "from_freq must be less than to_freq"
    if from_freq lt min(*(self._freq)) then from_freq = min(*(self._freq))
    if from_freq gt max(*(self._freq)) then from_freq = max(*(self._freq))
    if to_freq lt min(*(self._freq)) then to_freq = min(*(self._freq))
    if to_freq gt max(*(self._freq)) then to_freq = max(*(self._freq))

    idx_from = closest(from_freq, *(self._freq))
    idx_to   = closest(to_freq, *(self._freq))

    df=1./self._dt/(2*self->nfreqs()) ; see fft1.pro for total power computation
    if n_elements(spectrum_idx) eq 0 then begin
        return, total( (*(self._psd))[idx_from:idx_to, *], cumulative=cumulative ) * df
    endif else begin
        return, total( (*(self._psd))[idx_from:idx_to, spectrum_idx],1, cumulative=cumulative ) * df
    endelse
end

function AOtime_series::findpeaks, spectrum_idx, from_freq=from_freq, to_freq=to_freq
	n_el=8

	IF not (PTR_VALID(self._freq)) THEN self->SpectraCompute
	IF not (PTR_VALID(self._psd)) THEN self->SpectraCompute

	if n_elements(from_freq) eq 0 then from_freq = min(*(self._freq))
	if n_elements(to_freq)   eq 0 then to_freq = max(*(self._freq))
	if from_freq ge to_freq then message, "from_freq must be less than to_freq"
	if from_freq lt min(*(self._freq)) then from_freq = min(*(self._freq))
	if from_freq gt max(*(self._freq)) then from_freq = max(*(self._freq))
	if to_freq lt min(*(self._freq)) then to_freq = min(*(self._freq))
	if to_freq gt max(*(self._freq)) then to_freq = max(*(self._freq))

	idx_from = closest(from_freq, *(self._freq))
	idx_to   = closest(to_freq, *(self._freq))

	df=1./self._dt/(2*self->nfreqs()) ; see fft1.pro for total power computation
	fr=*self._freq

	if n_elements(spectrum_idx) eq 0 then vtemp=findgen((size(self->psd(),/dim))[1]) else vtemp=spectrum_idx

	for kkk=0, n_elements(vtemp)-1 do begin
		mode=vtemp[kkk]
		if mode lt 2 then thr=0.005 else thr=0.01
		tmax=( max( self->power(mode,/cum) )-min( self->power(mode,/cum) ) )
		thrs=thr/n_el*tmax
		spsd=smooth((*(self._psd))[idx_from:idx_to, mode],n_el)*df
		if thrs lt mean(spsd) then thrs=mean(spsd)
		idx=where(spsd gt thrs)+idx_from
		if total(idx) ne -1 then begin
			ofr=-1
			opw=-1
			opw100=-1
			j=0
			tempfr=0
			l=0
			f1=0
			for i=1, n_elements(idx)-1 do begin
				if idx[i] eq idx[i-1]+1 then begin
					if j eq 0 then f1=fr[idx[i-1]]
					if i eq 1 then begin
						tempfr=fr[idx[i]]+fr[idx[i-1]]
						j=2
					endif else begin
				    tempfr=tempfr+fr[idx[i]]
						j+=1
					endelse
					if i eq n_elements(idx)-1 then begin
						f2=fr[idx[i]]
						temppw=self->power(from_freq=f1,to_freq=f2,mode)
						if total(ofr) eq -1 then begin
							ofr=tempfr/j
							opw=temppw
						endif else begin
							ofr=[ofr, tempfr/j]
							opw=[opw, temppw]
						endelse
					endif
					l=0
				endif else begin
					l+=1
					if f1 ne 0 then f2=fr[idx[i-1]]
					if tempfr ne 0 then begin
						temppw=self->power(from_freq=f1,to_freq=f2,mode)
						if total(ofr) eq -1 then begin
							ofr=tempfr/j
							opw=temppw
						endif else begin
							ofr=[ofr, tempfr/j]
							opw=[opw, temppw]
						endelse
						l=0
					endif
					if l eq 2 then begin
						if i gt 1 then temppw=$
							(self->power(mode,/cum))[idx[i-1]+idx_from]-(self->power(mode,/cum))[idx[i-1]+idx_from-1] $
							else temppw=0
						if total(ofr) eq -1 then begin
							ofr=fr[idx[i-1]]
							opw=temppw
						endif else begin
							ofr=[ofr, fr[idx[i-1]]]
							opw=[opw, temppw]
						endelse
						l=1
					endif
					j=0
					tempfr=0
					f1=0
					f2=0
				endelse
			endfor
			opw100=opw/tmax*100.
		endif else begin
			ofr=-1
			opw=-1
			opw100=-1
		endelse
		if kkk eq 0 then begin
			res=create_struct('mode'+string(mode,format='(i03)'),{fr: ofr, pw: opw, pw100: opw100})
		endif else begin
			res=create_struct(res,'mode'+string(mode,format='(i03)'),{fr: ofr, pw: opw, pw100: opw100})
		endelse
	endfor
	return, res
end

function AOtime_series::finddirections, from_freq=from_freq, to_freq=to_freq, plot=plot, nfr=nfr, fstep=fstep
  IF not keyword_set(plot) THEN plot=0
  IF not keyword_set(fstep) THEN fstep=0.25
  IF not keyword_set(n) THEN n=5
  IF not (PTR_VALID(self._freq)) THEN self->SpectraCompute
  IF not (PTR_VALID(self._psd)) THEN self->SpectraCompute
  
  if n_elements(from_freq) eq 0 then from_freq = min(*(self._freq))
  if n_elements(to_freq)   eq 0 then to_freq = max(*(self._freq))
  if from_freq ge to_freq then message, "from_freq must be less than to_freq"
  if from_freq lt min(*(self._freq)) then from_freq = min(*(self._freq))
  if from_freq gt max(*(self._freq)) then from_freq = max(*(self._freq))
  if to_freq lt min(*(self._freq)) then to_freq = min(*(self._freq))
  if to_freq gt max(*(self._freq)) then to_freq = max(*(self._freq))
  
  if self._niter eq -1 then self->Compute
  
  idx_from = closest(from_freq, *(self._freq))
  idx_to   = closest(to_freq, *(self._freq))
  
  peaks=self->findpeaks([0,1], from_freq=from_freq, to_freq=to_freq)

  frtemp=[peaks.(0).fr,peaks.(1).fr]
  pwtemp=[peaks.(0).pw,peaks.(1).pw]
  flag=0
  j=0
  
  cc = [-1,255.,255.*256,255.*256*256,255.*256*100,255.*100]
  
  while flag eq 0 do begin
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
  rm1=dblarr(self._niter,nnn)
  rm2=dblarr(self._niter,nnn)
  ab=dblarr(2,nnn)
  cor=dblarr(nnn)
  var=dblarr(nnn)
  plt=0
  if plot eq 1 then $
    window, /free
  for ijk = 0, nnn-1 do begin 
    if ijk eq 0 then pw=pwtemp else pw[idxmax(ijk-1)]=0
    maxr(ijk) = max(pw,idxmaxtemp)
    idxmax(ijk) = idxmaxtemp
    fvibmax(ijk) = frtemp[idxmax(ijk)]
    pow(ijk) = pwtemp[idxmax(ijk)]
    a1t=fft((*(self->GetDati()))[*,0])
    a2t=fft((*(self->GetDati()))[*,1])
    p=self._niter*self._dt
    
    if p*fstep lt 1 then fstep=1./p
    a1t[0:p*(fvibmax(ijk)-fstep)-1]=0
    a1t[p*(fvibmax(ijk)+fstep):p*(1./self._dt-fvibmax(ijk)-fstep)-1]=0
    a1t[p*(1./self._dt-fvibmax(ijk)+fstep):*]=0
    a2t[0:p*(fvibmax(ijk)-fstep)-1]=0
    a2t[p*(fvibmax(ijk)+fstep):p*(1./self._dt-fvibmax(ijk)-fstep)-1]=0
    a2t[p*(1./self._dt-fvibmax(ijk)+fstep):*]=0
    rm1[*,ijk]=fft(a1t,1)
    rm2[*,ijk]=fft(a2t,1)
    ab[*,ijk] = linfit(rm1[*,ijk],rm2[*,ijk])
    cor[ijk] = variance( rm2[*,ijk]-ab[1,ijk]*rm1[*,ijk]-ab[0,ijk] )
    var[ijk] = variance( rm2[*,ijk] )
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
    if plot eq 1 then $
      oplot, minmax(rm1[*,0]), ab[1,ijk]*minmax(rm1[*,0])+ab[0,ijk], col=CC[ijk]
  endfor
  if plot eq 1 then $
    legend, strtrim(frvib,2)+'Hz', psym=fltarr(nnn)-1, col=colo
  angle=transpose(atan(ab[1,*]))*180/!pi

  directions={$
    freq: frvib, $
    power: pow, $
    error_var: cor, $
    signal_var: var, $
    angle: angle $
    }

  return, directions
end

pro AOtime_series::addHelp, obj

    obj->addMethodHelp, "niter()",   "number of time steps"
    obj->addMethodHelp, "time_variance(series_idx)", "time-variance of series series_idx"
    obj->addMethodHelp, "ensemble_variance(iter_idx)", "ensemble-variance of iteration iter_idx"
    obj->addMethodHelp, "time_average(series_idx)", "time-average of series series_idx"
    obj->addMethodHelp, "ensemble_average(iter_idx)", "ensemble-average of iteration iter_idx"
    obj->addMethodHelp, "nfreqs()", "number of elements in the frequency vector"
    obj->addMethodHelp, "freq()",   "frequency vector [nfreqs] (Hz)"
    obj->addMethodHelp, "nspectra()",   "number of residual modes spectra"
    obj->addMethodHelp, "psd(spectrum_idx)",   "return psd of spectra identified by the index vector idx. All spectra if index is not present"
    obj->addMethodHelp, "fftwindow()", "returns the type of apodization window applied in the computation of the PSD."
    obj->addMethodHelp, "set_fftwindow,fftwindow", "sets the apodization window to be used in the computation of the PSD."
    obj->addMethodHelp, "power(idx, from_freq=from, to_freq=to, /cumulative)", "return power of idx-th spectrum between frequencies from_freq and to_freq"
    obj->addMethodHelp, "findPeaks(idx, from_freq=from, to_freq=to)", "return the peaks of idx-th spectrum between frequencies from_freq and to_freq"
    obj->addMethodHelp, "findDirections(from_freq=from, to_freq=to, plot=plot, nfr=nfr, fstep=fstep)", $
                        "return the direction of vibrations (width[Hz]=2*fstep, tip=0°, tilt=90°) between frequencies from_freq and to_freq"
    obj->addMethodHelp, "specPlot(idx)", "plot psd of idx-th spectrum"
end


pro AOtime_series::free
    ptr_free, self._psd
    ptr_free, self._time_variance
    ptr_free, self._time_average
    ptr_free, self._ensemble_variance
    ptr_free, self._ensemble_average
    ptr_free, self._freq
end


pro AOtime_series::forceCompute
    file_delete, self._store_psd_fname, /allow_nonexistent
    self->AOtime_series::free
end


pro AOtime_series::Cleanup
    ;ptr_free, self._dati
    ptr_free, self._time_variance
    ptr_free, self._time_average
    ptr_free, self._ensemble_variance
    ptr_free, self._ensemble_average
    ptr_free, self._psd
    ptr_free, self._freq
end

pro AOtime_series__define
struct = { AOtime_series, $
;    _dati              :  ptr_new(), $  ;ptr to dati area. Belongs to daughter class. Don't free
    _niter             :  0L        , $
    _nseries           :  0L		, $
    _nfreqs            :  0L		, $
    _nspectra          :  0L		, $
;    _nospectra         :  0B       , $
    _time_variance     :  ptr_new()	, $
    _time_average      :  ptr_new()	, $
    _ensemble_variance :  ptr_new()	, $
    _ensemble_average  :  ptr_new()	, $
    _psd               :  ptr_new()	, $
    _freq              :  ptr_new()	, $
    _norm_factor	   :  0.		, $
    _store_psd_fname   :  ""		, $
    _window            :  ""		, $
    _nwindows		   :  0L		, $
    _dt                :  0d  		, $
    _plots_title	   :  ""		, $
    _spectra_units	   :  ""		  $
}
end


