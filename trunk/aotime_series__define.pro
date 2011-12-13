
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
    self._smooth=1
    self._thr_peaks=1e-3
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
        dati_phase = fltarr(nfreqs, nspectra)

        for i=0L,nspectra-1 do begin
          psd1   = fltarr(nfreqs)
          phase1 = fltarr(nfreqs)
          for j=0L,self._nwindows-1 do begin
         	  dati1d_raw = (*dati)[j*nfreqs*2:(j+1)*nfreqs*2-1,i]
              case strlowcase(self._window) of
                  ""         :  begin & dati1d = dati1d_raw & winnorm = 1.0 & end
                  "hanning"  :  begin & dati1d = HANNING(2*nfreqs) * dati1d_raw & winnorm = 1.0/0.375  & end ; winnorm = 1./mean(hanning(10)^2)
                  "hamming"  :  begin & dati1d = HANNING(2*nfreqs, alpha=0.56) * dati1d_raw & winnorm = 1.0/0.4104 & end
              endcase
        	  mv = mean(dati1d)
        	  dati1d = dati1d - mv
              fft1, dati1d, self._dt, fspec=fspec, psd=psd, pspe=pspectrum, /noplot
              phase1 += pspectrum(1,1:n_elements(fspec))
              psd1 += psd
          endfor
          dati_psd[*,i] = psd1 * winnorm/self._nwindows
          dati_phase[*,i] = phase1 /self._nwindows  
        endfor
        ; save psd
        save, dati_psd, fspec, nfreqs, nspectra, dati_phase, file=self._store_psd_fname
    endelse

    if (ptr_valid(self._psd))  THEN ptr_free, self._psd
    if (ptr_valid(self._phase))  THEN ptr_free, self._phase
    if (ptr_valid(self._freq)) THEN ptr_free, self._freq
    self._psd      = ptr_new(dati_psd, /no_copy)
    self._phase    = ptr_new(dati_phase, /no_copy)
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
    if self._niter eq -1 then self->AOtime_series::Compute
    return, self._niter
end

function AOtime_series::nseries
    if self._nseries eq -1 then self->AOtime_series::Compute
    return, self._nseries
end

function AOtime_series::time_variance, series_idx
    if not ptr_valid(self._time_variance) then self->AOtime_series::Compute

    if n_elements(series_idx) ne 0 then return, (*self._time_variance)[series_idx] $
    else return, *self._time_variance
end

function AOtime_series::ensemble_variance, iter_idx
    if not ptr_valid(self._ensemble_variance) then self->AOtime_series::Compute

    if n_elements(iter_idx) ne 0 then return, (*self._ensemble_variance)[iter_idx] $
    else return, *self._ensemble_variance
end

function AOtime_series::time_average, series_idx
    if not ptr_valid(self._time_average) then self->AOtime_series::Compute

    if n_elements(series_idx) ne 0 then return, (*self._time_average)[series_idx] $
    else return, *self._time_average
end

function AOtime_series::ensemble_average, iter_idx
    if not ptr_valid(self._ensemble_average) then self->AOtime_series::Compute
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

; phase of spectrum_idx averaged between from_freq and to to_freq
; if spectrum_idx is not specified it returns a scalar that is the average of the phase on all the spectra and between from an to
; return an array n_elements(spectrum_idx) 
;
function AOtime_series::phase, spectrum_idx, from_freq=from_freq, to_freq=to_freq, average=average
    IF not (PTR_VALID(self._phase)) THEN self->SpectraCompute

	minfreq = min(self->freq())
	maxfreq = max(self->freq())
	if n_elements(from_freq) eq 0 then from_freq = minfreq else $
		from_freq = minfreq > from_freq < maxfreq
  	if n_elements(to_freq) eq 0 then to_freq = maxfreq else $
  		to_freq = minfreq > to_freq < maxfreq
	if from_freq ge to_freq then message, "from_freq must be less than to_freq"

    idx_from = closest(from_freq, self->freq())
    idx_to   = closest(to_freq, self->freq())
    if n_elements(spectrum_idx) eq 0 then begin
        if keyword_set(average) then return, mean( (*(self._phase))[idx_from:idx_to, *] ) $
        else return, (*(self._phase))[idx_from:idx_to, *]
    endif else begin
        if keyword_set(average) then res = total( (*(self._phase))[idx_from:idx_to, spectrum_idx], 1 ) / (idx_to-idx_from+1) $
        else res = (*(self._phase))[idx_from:idx_to, spectrum_idx]
        return, res
    endelse
    
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

pro AOtime_series::SpecPlot, elemnum, OVERPLOT=OVERPLOT, COLOR=COLOR, _extra=ex

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
	psd = self->psd() * self->norm_factor()^2.
  	yrange = sqrt(minmax(psd[1:*,elemnum]))

    if not keyword_set(OVERPLOT) then  begin
    	PREV_MARGIN = !X.MARGIN
		!X.MARGIN = [12, 3]
		title =self._plots_title+', element '+strtrim(elemnum,2)
		plot_oo, freq[1:*], sqrt(psd[1:*,elemnum]), charsize=1.2, xtitle=xtitle, ytitle=textoidl('['+self._spectra_units+'  Hz^{-1/2}]') $
			, title=title, yrange=yrange, ytickformat='(e9.1)', _extra=ex
		!X.MARGIN = PREV_MARGIN
	endif else begin
		oplot, freq[1:*], sqrt(psd[1:*,elemnum]), COLOR=COLOR
	endelse
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
function AOtime_series::power, spectrum_idx, from_freq=from_freq, to_freq=to_freq, cumulative=cumulative, sumspectra=sumspectra

	minfreq = min(self->freq())
	maxfreq = max(self->freq())
	if n_elements(from_freq) eq 0 then from_freq = minfreq else $
		from_freq = minfreq > from_freq < maxfreq
  	if n_elements(to_freq) eq 0 then to_freq = maxfreq else $
  		to_freq = minfreq > to_freq < maxfreq
	if from_freq ge to_freq then message, "from_freq must be less than to_freq"

    idx_from = closest(from_freq, self->freq())
    idx_to   = closest(to_freq, self->freq())

    df=1./self._dt/(2*self->nfreqs()) ; see fft1.pro for total power computation
    if n_elements(spectrum_idx) eq 0 then begin
        return, total( (self->psd())[idx_from:idx_to, *], cumulative=cumulative ) * df
    endif else begin
        res = total( (self->psd())[idx_from:idx_to, spectrum_idx], 1, cumulative=cumulative ) * df ;[nfreqs, n_elements(spectrum_idx)]
        if keyword_set(sumspectra) then res = total(res,size(res, /n_dim))
        return, res
        ;return, total( (*(self._psd))[idx_from:idx_to, spectrum_idx],1, cumulative=cumulative ) * df
    endelse
end


function AOtime_series::findpeaks, spectrum_idx, from_freq=from_freq, to_freq=to_freq, t100=t100

	IF not (PTR_VALID(self._peaks)) THEN self->PeaksCompute

  	; threshold on the minimum power of the returned results
  	if not keyword_set(t100) then t100=0.

	; if from_freq and/or to_freq keywords are set the function find the peaks between these frequencies
	minfreq = min(self->freq())
	maxfreq = max(self->freq())

	if n_elements(from_freq) eq 0 then from_freq = minfreq else $
		from_freq = minfreq > from_freq < maxfreq

  	if n_elements(to_freq) eq 0 then to_freq = maxfreq else $
  		to_freq = minfreq > to_freq < maxfreq

	if from_freq ge to_freq then message, "from_freq must be less than to_freq"


	;Extract Selected SPECs
	;------------------------------
	if n_elements(spectrum_idx) eq 0 then peaks = *self._peaks else $
		for i=0, n_elements(spectrum_idx)-1 do begin
        	tag_spec = 'spec' + string(spectrum_idx[i],format='(i04)')
        	if tag_exist(*self._peaks, tag_spec, index=tidx, /top) then begin
        		tempr = (*self._peaks).(tidx)
        		if n_elements(peaks) eq 0 then peaks = create_struct(tag_spec,tempr) else $
        					   				   peaks = create_struct(peaks, tag_spec, tempr)
        	endif
        endfor
	if n_elements(peaks) eq 0 then message, 'Requested SPECs not found.'


	;Extract selected intervals
	;------------------------------
    if (t100 gt 0) or (to_freq ne maxfreq) or (from_freq ne minfreq) then begin
		nt  = n_tags(peaks)
		tg1 = tag_names(peaks)
		for i=0, nt-1 do begin
			fridx = where_tag(peaks.(i), tag_name='fr', range=[from_freq, to_freq], /NOPRINT )
			t1idx = where_tag(peaks.(i), tag_name='pw100', range=[t100, 100.], /NOPRINT )
			idx = setintersection(fridx,t1idx)
			if idx[0] ne -1 then begin
          		tempr = {fr: peaks.(i).fr[idx], frmax: peaks.(i).frmax[idx], frmin: peaks.(i).frmin[idx] $
          			, pw: peaks.(i).pw[idx], pw100: peaks.(i).pw100[idx]}
				if n_elements(peaks2) eq 0 then peaks2 = create_struct(tg1[i], tempr) else $
												peaks2 = create_struct(peaks2, tg1[i], tempr)
			endif
		endfor
		if n_elements(peaks2) eq 0 then message, 'No SPECs with specified ranges were found.'
	endif else peaks2 = temporary(peaks)

return, peaks2
end

pro AOtime_series::showpeaks, spectrum_idx, from_freq=from_freq, to_freq=to_freq, t100=t100, _extra=ex, WIN=WIN

	if n_params() ne 1 then begin
		message, 'Missing parameter. Usage: ...->SpecPlot, elemnum', /info
		return
	endif

	if n_elements(spectrum_idx) ne 1 then begin
		message, 'Only ONE mode can be specified at a time',  /info
		return
	endif

	df=1./self._dt/(2*self->nfreqs())
	freq = self->freq()
	norm_factor = self->norm_factor()
	psds = self->psd(spectrum_idx) * df * norm_factor^2.
	catch, error
	if error eq 0 then begin
		pk = self->findpeaks(spectrum_idx, from_freq=from_freq, to_freq=to_freq, t100=t100)
		npks = n_elements(pk.(0).fr)
		dpk = (pk.(0).frmax - pk.(0).frmin)/df + 1. ;number of freq bins taken into account in each vib.
		pw = pk.(0).pw * norm_factor^2.
		;show me the peaks!
		IF n_elements(WIN) eq 0 then win=10
		window,win,xsize=700, ysize=400
		plot, freq, psds, psym=-1, ygridstyle=1, xgridstyle=1, xticklen=1, yticklen=1, xtitle='Frequency [Hz]' $
			, ytitle='Power ['+self._spectra_units+textoidl('^2]'), title=self._plots_title+', mode '+strtrim(spectrum_idx,2), charsize=1.2, _extra=ex
		oplot, [pk.(0).fr], [pw], psym=2, color=255L
		for ii=0, npks-1 do oplot, [replicate(pk.(0).frmin[ii],2),pk.(0).fr[ii],replicate(pk.(0).frmax[ii],2)], $
			[0,replicate(pw[ii],3),0], color=255L
		catch, /cancel
	endif else begin
		print, !ERROR_STATE.MSG
		catch, /cancel
	endelse
end

pro AOtime_series::set_smooth, smooth
  if n_params() ne 1 then begin
    message, 'Missing parameter: Usage: (...)->set_smooth, smooth', /info
    return
  endif
  if smooth le 0 then begin
    message, "Smooth must be greater than 0", /info
    return
  endif
  if smooth eq self._smooth then return else begin
    self._smooth = smooth
    file_delete, self._store_peaks_fname, /allow_nonexistent
    ptr_free, self._peaks
  endelse
end

pro AOtime_series::set_threshold, threshold
  if n_params() ne 1 then begin
    message, 'Missing parameter: Usage: (...)->set_threshold, threshold', /info
    return
  endif
  if threshold le 0 or threshold ge 1 then begin
    message, "Threshold must be >0 and <1", /info
    return
  endif
  if threshold eq self._thr_peaks then return else begin
    self._thr_peaks = threshold
    file_delete, self._store_peaks_fname, /allow_nonexistent
    ptr_free, self._peaks
  endelse
end

pro AOtime_series::PeaksCompute
  if file_test(self._store_peaks_fname) then begin
    restore, self._store_peaks_fname
    self._smooth = smooth
    self._thr_peaks=threshold
  endif else begin
  ; this function returns a structure with three vectors:
  ; fr = frequencies where it finds peaks,
  ; pw = power of the peaks found,
  ; pw100 = relative power of the peaks found

;  IF not (PTR_VALID(self._freq)) THEN self->SpectraCompute
;  IF not (PTR_VALID(self._psd)) THEN self->SpectraCompute

  df=1./self._dt/(2*self->nfreqs()) ; see fft1.pro for total power computation
;  fr = self->freq()
;  pw=self->psd()*df
  smooth=self._smooth
  threshold=self._thr_peaks

  modes=findgen((size(self->psd(),/dim))[1])

  for kkk=0, n_elements(modes)-1 do begin
    mode=modes[kkk] ; mode number
    tmax=( max( self->power(mode,/cum) )-min( self->power(mode,/cum) ) ) ; delta power of the measurement
    thrs=threshold*tmax ; the threshold is multiplied by the delta power of the measurement
    if smooth ge 2 then spsd=smooth(self->psd(mode),smooth)*df $ ;smooth of the psd
    	else spsd=self->psd(mode)*df
    idx=where(spsd gt thrs, count1) ; index of the frequencies over the threshold
    if count1 ne 0 then begin ; case of at least one frequency over the threshold
      ; initialize the variables
      ofr=-1
      ofrmax=-1
      ofrmin=-1
      opw=-1
      opw100=-1
      j=0 ; coefficient which measure the number of consecutive frequencies over the threshold
      tempfr=0
      temppw=0
      l=0 ; if it is equal to 1 it gives the information that this frequency and the previous frequency
          ; are not consecutive or that this frequency and the next frequency are not consecutive,
          ; if it is equal to 2 this frequency is an isolated frequency (over the threshold)
      f1=0
      for i=1, count1-1 do begin ; for cycle of each frequencies over the threshold
      if j gt 1 and idx[i] lt n_elements(spsd)-2 then begin ; if there are at least two consecutive frequencies and we are not at the end of the psd
        if spsd[idx[i]] lt spsd[idx[i]-1] and spsd[idx[i]] lt spsd[idx[i]+1] then flag=0 else flag=1 ;it checks if it is in a local minimum, if so flag=0
      endif else begin
        flag=1
      endelse
        if idx[i] eq idx[i-1]+1 and flag then begin ; case of two consecutive frequencies and no local minimum
          if j eq 0 then f1=(self->freq())[idx[i-1]] ; set the starting frequency
          if i eq 1 then j=2 else j+=1
          if i eq count1-1 then begin ; if it is the last step
            f2=(self->freq())[idx[i]] ; set the ending frequency
            temppw=self->power(mode,from=f1,to=f2)
            tempfr=total(self->freq(from=f1,to=f2)*(self->psd(mode))[closest(f1, self->freq()):closest(f2, self->freq())]*df)/temppw
            if total(ofr) eq -1 then begin ; it initializes the vectors if they do not exists
              ofr=tempfr ; weighted mean frequency
              ofrmax=f2
              ofrmin=f1
              opw=temppw ; power
            endif else begin
              ofr=[ofr, tempfr] ; frequency vector
              ofrmax=[ofrmax, f2]
              ofrmin=[ofrmin, f1]
              opw=[opw, temppw] ; power vector
            endelse
          endif
          l=0
        endif else begin ; case of no consecutive frequencies and/or local minimum
          flag=1 ; it exits local minimum condition
          l+=1
          if f1 ne 0 then begin ; set the ending frequency if exists the starting one
            f2=(self->freq())[idx[i-1]]
            temppw=self->power(mode,from=f1,to=f2)
            tempfr=total(self->freq(from=f1,to=f2)*(self->psd(mode))[closest(f1, self->freq()):closest(f2, self->freq())]*df)/temppw
            if total(ofr) eq -1 then begin ; it initializes the vectors if they do not exists
              ofr=tempfr ; weighted mean frequency
              ofrmax=f2
              ofrmin=f1
              opw=temppw ; power
            endif else begin
              ofr=[ofr, tempfr] ; frequency vector
              ofrmax=[ofrmax, f2]
              ofrmin=[ofrmin, f1]
              opw=[opw, temppw] ; power vector
            endelse
            l=0
          endif
          if l eq 2 then begin ; if it is an isolated frequency over the threshold
            if i gt 1 then temppw=self->power(mode,from=(self->freq())[idx[i-1]-1],to=(self->freq())[idx[i-1]+1]) $
            else temppw=0 ;gives a pw > 0 only if it is not the first frequency
            if total(ofr) eq -1 then begin ; it initializes the vectors if they do not exists
              ofr=(self->freq())[idx[i-1]] ; mean frequency
              ofrmax=(self->freq())[idx[i-1]]
              ofrmin=(self->freq())[idx[i-1]]
              opw=temppw ; power
            endif else begin
              ofr=[ofr, (self->freq())[idx[i-1]]] ; frequency vector
              ofrmax=[ofrmax, (self->freq())[idx[i-1]]]
              ofrmin=[ofrmin, (self->freq())[idx[i-1]]]
              opw=[opw, temppw] ; power vector
            endelse
            l=1
          endif
          ; set to 0 the number of consecutive frequencies, the temporary frequency,
          ; and the starting and ending frequencies
          j=0
          tempfr=0
          temppw=0
          f1=0
          f2=0
        endelse
      endfor
      opw100=opw/max(self->power(mode,/cum))*100. ; relative power vector
    endif else begin
      ; case of no frequencies over the threshold
      ofr=-1
      ofrmax=-1
      ofrmin=-1
      opw=-1
      opw100=-1
    endelse
    if kkk eq 0 then begin
      res=create_struct('spec'+string(mode,format='(i04)'),{fr: ofr, frmax: ofrmax, frmin: ofrmin, pw: opw, pw100: opw100})
    endif else begin
      res=create_struct(res,'spec'+string(mode,format='(i04)'),{fr: ofr, frmax: ofrmax, frmin: ofrmin, pw: opw, pw100: opw100})
    endelse
  endfor
  save, res, smooth, threshold, file=self._store_peaks_fname
  endelse
  if (ptr_valid(self._peaks))  THEN ptr_free, self._peaks
  self._peaks = ptr_new(res, /no_copy)
end

pro AOtime_series::addHelp, obj
    obj->addMethodHelp, "niter()",   "number of time steps"
    obj->addMethodHelp, "time_variance(series_idx)", "time-variance of series series_idx"
    obj->addMethodHelp, "ensemble_variance(iter_idx)", "ensemble-variance of iteration iter_idx"
    obj->addMethodHelp, "time_average(series_idx)", "time-average of series series_idx"
    obj->addMethodHelp, "ensemble_average(iter_idx)", "ensemble-average of iteration iter_idx"
    obj->addMethodHelp, "nfreqs()", "number of elements in the frequency vector"
    obj->addMethodHelp, "freq()",   "frequency vector [nfreqs] (Hz)"
    obj->addMethodHelp, "nspectra()",   "number of spectra"
    obj->addMethodHelp, "psd(spectrum_idx)",   "return psd of spectra identified by the index vector idx. All spectra if index is not present"
    obj->addMethodHelp, "phase(idx, from_freq=from, to_freq=to, /cumulative, /sumspectra)", "return phase of idx-th spectrum between frequencies from_freq and to_freq, eventually cumulating on freqs and/or summing on spectra"
    obj->addMethodHelp, "fftwindow()", "returns the type of apodization window applied in the computation of the PSD."
    obj->addMethodHelp, "set_fftwindow,fftwindow", "sets the apodization window to be used in the computation of the PSD."
    obj->addMethodHelp, "power(idx, from_freq=from, to_freq=to, /cumulative, /sumspectra)", "return power of idx-th spectrum between frequencies from_freq and to_freq, eventually cumulating on freqs and/or summing on spectra"
    obj->addMethodHelp, "findPeaks(idx, from_freq=from, to_freq=to, t100=t100)", "return the peaks of idx-th spectrum between frequencies from_freq and to_freq, t100 = % threshold of the returned results"
    obj->addMethodHelp, "showPeaks, idx, from_freq=from, to_freq=to, t100=t100", "show the peaks of idx-th spectrum between frequencies from_freq and to_freq, t100 = % threshold of the returned results"
    obj->addMethodHelp, "set_smooth,smooth", "sets the smooth to be used in the computation of the PEAKS."
    obj->addMethodHelp, "set_threshold,threshold", "ATTENTION: You are advise not to modify this value! sets the threshold to be used in the computation of the PEAKS."
    ;    obj->addMethodHelp, "findDirections(from_freq=from, to_freq=to, plot=plot, nfr=nfr, fstep=fstep)", $
;                        "return the direction of vibrations (width[Hz]=2*fstep, tip=0°, tilt=90°) between frequencies from_freq and to_freq"
    obj->addMethodHelp, "SpecPlot, idx", "plot psd of idx-th spectrum"
end

pro AOtime_series::test
    d=self->niter()
    d=self->time_variance()
    d=self->ensemble_variance()
    d=self->time_average()
    d=self->ensemble_average()
    d=self->nfreqs()
    d=self->freq()
    d=self->nspectra()
    d=self->psd()
    d=self->fftwindow()
    d=self->power()
    d=self->findPeaks()
    ;d=self->findDirections()
    ; self->specPlot,0
end

pro AOtime_series::free
    ptr_free, self._psd
    ptr_free, self._phase
    ptr_free, self._time_variance
    ptr_free, self._time_average
    ptr_free, self._ensemble_variance
    ptr_free, self._ensemble_average
    ptr_free, self._freq
    ptr_free, self._peaks
end


pro AOtime_series::forceCompute
    file_delete, self._store_psd_fname, /allow_nonexistent
    file_delete, self._store_peaks_fname, /allow_nonexistent
    self->AOtime_series::free
end

pro AOtime_series::Cleanup
    ;ptr_free, self._dati
    ptr_free, self._time_variance
    ptr_free, self._time_average
    ptr_free, self._ensemble_variance
    ptr_free, self._ensemble_average
    ptr_free, self._psd
    ptr_free, self._phase
    ptr_free, self._freq
    ptr_free, self._peaks
end

pro AOtime_series__define
struct = { AOtime_series, $
;    _dati              :  ptr_new(), $  ;ptr to dati area. Belongs to daughter class. Don't free
    _niter             :  0L        , $
    _nseries           :  0L		, $
    _nfreqs            :  0L		, $
    _nspectra          :  0L		, $
    _smooth            :  0L    , $
    _thr_peaks         :  0.    , $
;    _nospectra         :  0B       , $
    _time_variance     :  ptr_new()	, $
    _time_average      :  ptr_new()	, $
    _ensemble_variance :  ptr_new()	, $
    _ensemble_average  :  ptr_new()	, $
    _psd               :  ptr_new()	, $
    _phase             :  ptr_new()	, $
    _freq              :  ptr_new()	, $
    _peaks             :  ptr_new() , $
    _norm_factor	   :  0.		, $
    _store_psd_fname   :  ""		, $
    _store_peaks_fname     :  ""    , $
    _window            :  ""		, $
    _nwindows		   :  0L		, $
    _dt                :  0d  		, $
    _plots_title	   :  ""		, $
    _spectra_units	   :  ""		  $
}
end


