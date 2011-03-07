
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
function AOtime_series::power, spectrum_idx, from_freq=from_freq, to_freq=to_freq, cumulative=cumulative, sumspectra=sumspectra
    IF not (PTR_VALID(self._freq)) THEN self->SpectraCompute
    IF not (PTR_VALID(self._psd)) THEN self->SpectraCompute

    if n_elements(from_freq) eq 0 then from_freq = min(self->freq())
    if n_elements(to_freq)   eq 0 then to_freq = max(self->freq())
    if from_freq ge to_freq then message, "from_freq must be less than to_freq"
    if from_freq lt min(self->freq()) then from_freq = min(self->freq())
    if from_freq gt max(self->freq()) then from_freq = max(self->freq())
    if to_freq lt min(self->freq()) then to_freq = min(self->freq())
    if to_freq gt max(self->freq()) then to_freq = max(self->freq())

    idx_from = closest(from_freq, self->freq())
    idx_to   = closest(to_freq, self->freq())

    df=1./self._dt/(2*self->nfreqs()) ; see fft1.pro for total power computation
    if n_elements(spectrum_idx) eq 0 then begin
        return, total( (*(self._psd))[idx_from:idx_to, *], cumulative=cumulative ) * df
    endif else begin
        res = total( (*(self._psd))[idx_from:idx_to, spectrum_idx], 1, cumulative=cumulative ) * df ;[nfreqs, n_elements(spectrum_idx)]
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
        		if i eq 0 then peaks = create_struct(tag_spec,tempr) else $
        					   peaks = create_struct(peaks, tag_spec, tempr)
        	endif
        endfor
	if n_elements(peaks) eq 0 then message, 'Requested SPECs not found'


	;Extract selected intervals
	;------------------------------
    if (t100 gt 0) or (to_freq ne maxfreq) or (from_freq ne minfreq) then begin
		nt  = n_tags(peaks)
		tg1 = tag_names(peaks)
		for i=0, nt-1 do begin
			fridx = where_tag(peaks.(i), tag_name='fr', range=[from_freq, to_freq])
			t1idx = where_tag(peaks.(i), tag_name='pw100', range=[t100, 100.])
			idx = setintersection(fridx,t1idx)
			if idx[0] ne -1 then begin
          		tempr = {fr: peaks.(i).fr[idx], frmax: peaks.(i).frmax[idx], frmin: peaks.(i).frmin[idx] $
          			, pw: peaks.(i).pw[idx], pw100: peaks.(i).pw100[idx]}
				if n_elements(peaks2) eq 0 then peaks2 = create_struct(tg1[i], tempr) else $
												peaks2 = create_struct(peaks2, tg1[i], tempr)
			endif
		endfor
	endif else peaks2 = peaks


return, peaks2
end


;function AOtime_series::findpeaks, spectrum_idx, from_freq=from_freq, to_freq=to_freq, t100=t100
;
;  IF not (PTR_VALID(self._peaks)) THEN self->PeaksCompute
;  if not keyword_set(t100) then t100=0. ; threshold on the minimum power of the returned results
;
;  ; if from_freq and/or to_freq keywords are set the function find the peaks between these frequencies
;  if n_elements(from_freq) eq 0 then from_freq = minfreq else $
;  				from_freq = minfreq > from_freq < max(self->freq())
;
;  if n_elements(to_freq)   eq 0 then to_freq = max(self->freq()) else $
;  				to_freq = min(self->freq()) > to_freq < max(self->freq())
;
;  if from_freq ge to_freq then begin
;  	message, "from_freq must be less than to_freq", /info
;  	return, -1
;  endif
;
;  ; if spectrum_idx is not set the function runs for each mode else it runs for the modes selected in spectrum_idx
;  if n_elements(spectrum_idx) eq 0 then begin
;    if t100 eq 0 and to_freq eq max(self->freq()) and from_freq eq min(self->freq()) then begin
;      return, *(self._peaks)
;    endif else begin
;      nt = n_tags(*(self._peaks))
;      for i=0, nt-1 do begin
;        q=string(i,format='(i04)')
;        tag_spec = 'spec'+q
;        if tag_exist(*(self._peaks), tag_spec, index=i, /top) then ofr = (*(self._peaks)).(i).fr
;        if tag_exist(*(self._peaks), tag_spec, index=i, /top) then ofrmax = (*(self._peaks)).(i).frmax
;        if tag_exist(*(self._peaks), tag_spec, index=i, /top) then ofrmin = (*(self._peaks)).(i).frmin
;        if tag_exist(*(self._peaks), tag_spec, index=i, /top) then opw = (*(self._peaks)).(i).pw
;        if tag_exist(*(self._peaks), tag_spec, index=i, /top) then opw100 = (*(self._peaks)).(i).pw100
;        if t100 gt 0 then begin
;          idx=where(ofr ge from_freq and ofr le to_freq and opw100 ge t100)
;        endif else begin
;          idx=where(ofr ge from_freq and ofr le to_freq)
;        endelse
;        if i eq 0 then begin
;          if total(idx) ne -1 then $
;          res=create_struct('spec'+q,{fr: ofr(idx), frmax: ofrmax(idx), frmin: ofrmin(idx), pw: opw(idx), pw100: opw100(idx)}) $
;          else res=create_struct('spec'+q,{fr: -1, frmax: -1, frmin: -1, pw: -1, pw100: -1})
;        endif else begin
;          if total(idx) ne -1 then $
;          res=create_struct(res,'spec'+q,{fr: ofr(idx), frmax: ofrmax(idx), frmin: ofrmin(idx), pw: opw(idx), pw100: opw100(idx)}) $
;          else res=create_struct(res,'spec'+q,{fr: -1, frmax: -1, frmin: -1, pw: -1, pw100: -1})
;        endelse
;      endfor
;      return, res
;    endelse
;  endif else begin
;    if t100 eq 0 and to_freq eq max(self->freq()) and from_freq eq min(self->freq()) then begin
;      for i=0, n_elements(spectrum_idx)-1 do begin
;        q=string(spectrum_idx[i],format='(i04)')
;        tag_spec = 'spec'+q
;        if tag_exist(*(self._peaks), tag_spec, /top) then tempr = (*(self._peaks)).(spectrum_idx[i])
;        if i eq 0 then begin
;          res=create_struct('spec'+q,tempr)
;        endif else begin
;          res=create_struct(res,'spec'+q,tempr)
;        endelse
;      endfor
;      return, res
;    endif else begin
;      for i=0, n_elements(spectrum_idx)-1 do begin
;        q=string(spectrum_idx[i],format='(i04)')
;        tag_spec = 'spec'+q
;        if tag_exist(*(self._peaks), tag_spec, index=tagidx, /top) then begin
;        	ofr 	= (*(self._peaks)).(spectrum_idx[i]).fr
;			ofrmax 	= (*(self._peaks)).(spectrum_idx[i]).frmax
;        	ofrmin 	= (*(self._peaks)).(spectrum_idx[i]).frmin
;        	opw 	= (*(self._peaks)).(spectrum_idx[i]).pw
;        	opw100 	= (*(self._peaks)).(spectrum_idx[i]).pw100
;        endif
;        if t100 gt 0 then begin
;          idx=where(ofr ge from_freq and ofr le to_freq and opw100 ge t100)
;        endif else begin
;          idx=where(ofr ge from_freq and ofr le to_freq)
;        endelse
;        if i eq 0 then begin
;          if total(idx) ne -1 then $
;          res=create_struct('spec'+q,{fr: ofr(idx), frmax: ofrmax(idx), frmin: ofrmin(idx), pw: opw(idx), pw100: opw100(idx)}) $
;          else res=create_struct('spec'+q,{fr: -1, frmax: -1, frmin: -1, pw: -1, pw100: -1})
;        endif else begin
;          if total(idx) ne -1 then $
;          res=create_struct(res,'spec'+q,{fr: ofr(idx), frmax: ofrmax(idx), frmin: ofrmin(idx), pw: opw(idx), pw100: opw100(idx)}) $
;          else res=create_struct(res,'spec'+q,{fr: -1, frmax: -1, frmin: -1, pw: -1, pw100: -1})
;        endelse
;      endfor
;      return, res
;    endelse
;  endelse
;end

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
    idx=where(spsd gt thrs) ; index of the frequencies over the threshold
    if total(idx) ne -1 then begin ; case of at least one frequency over the threshold
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
      for i=1, n_elements(idx)-1 do begin ; for cycle of each frequencies over the threshold
      if j gt 1 and idx[i] lt n_elements(spsd)-2 then begin ; if there are at least two consecutive frequencies and we are not at the end of the psd
        if spsd[idx[i]] lt spsd[idx[i]-1] and spsd[idx[i]] lt spsd[idx[i]+1] then flag=0 else flag=1 ;it checks if it is in a local minimum, if so flag=0
      endif else begin
        flag=1
      endelse
        if idx[i] eq idx[i-1]+1 and flag then begin ; case of two consecutive frequencies and no local minimum
          if j eq 0 then f1=(self->freq())[idx[i-1]] ; set the starting frequency
          if i eq 1 then j=2 else j+=1
          if i eq n_elements(idx)-1 then begin ; if it is the last step
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

;function AOtime_series::finddirections, from_freq=from_freq, to_freq=to_freq, plot=plot, nfr=nfr, fstep=fstep
;  IF not keyword_set(plot) THEN plot=0
;  IF not keyword_set(fstep) THEN fstep=0.25
;  IF not keyword_set(nfr) THEN nfr=5
;  IF not (PTR_VALID(self._freq)) THEN self->SpectraCompute
;  IF not (PTR_VALID(self._psd)) THEN self->SpectraCompute
;
;  if n_elements(from_freq) eq 0 then from_freq = min(self->freq())
;  if n_elements(to_freq)   eq 0 then to_freq = max(self->freq())
;  if from_freq ge to_freq then message, "from_freq must be less than to_freq"
;  if from_freq lt min(self->freq()) then from_freq = min(self->freq())
;  if from_freq gt max(self->freq()) then from_freq = max(self->freq())
;  if to_freq lt min(self->freq()) then to_freq = min(self->freq())
;  if to_freq gt max(self->freq()) then to_freq = max(self->freq())
;
;  if self._niter eq -1 then self->Compute
;
;  idx_from = closest(from_freq, self->freq())
;  idx_to   = closest(to_freq, self->freq())
;
;  peaks=self->findpeaks([0,1], from_freq=from_freq, to_freq=to_freq)
;
;  frtemp=[peaks.(0).fr,peaks.(1).fr]
;  pwtemp=[peaks.(0).pw,peaks.(1).pw]
;  flag=0
;  j=0
;
;  cc = [-1,255.,255.*256,255.*256*256,255.*256*100,255.*100]
;
;  while flag eq 0 do begin
;    idx=where(abs(frtemp - frtemp[j]) lt 0.6)
;    if total(idx) ne -1 then begin
;      for k=0,n_elements(idx)-1 do begin
;        if idx[k] ne j then begin
;          if idx[k] lt n_elements(frtemp)-1 then begin
;            frtemp=[frtemp[0:idx[k]-1],frtemp[idx[k]+1:*]]
;            pwtemp=[pwtemp[0:idx[k]-1],pwtemp[idx[k]+1:*]]
;          endif else begin
;            frtemp=frtemp[0:idx[k]-1]
;            pwtemp=pwtemp[0:idx[k]-1]
;          endelse
;          idx=idx-1
;        endif
;      endfor
;    endif
;    j+=1
;    if j gt n_elements(frtemp)-1 then flag=1
;  endwhile
;
;  if n_elements(pwtemp) lt nfr then nnn=n_elements(pwtemp) else nnn=nfr
;  maxr=dblarr(nnn)
;  idxmax=dblarr(nnn)
;  fvibmax=dblarr(nnn)
;  pow=dblarr(nnn)
;  rm1=dblarr(self._niter,nnn)
;  rm2=dblarr(self._niter,nnn)
;  ab=dblarr(2,nnn)
;  cor=dblarr(nnn)
;  var=dblarr(nnn)
;  xy=dblarr(nnn)
;  plt=0
;  if plot eq 1 then $
;    window, /free
;  for ijk = 0, nnn-1 do begin
;    if ijk eq 0 then pw=pwtemp else pw[idxmax(ijk-1)]=0
;    maxr(ijk) = max(pw,idxmaxtemp)
;    idxmax(ijk) = idxmaxtemp
;    fvibmax(ijk) = frtemp[idxmax(ijk)]
;    pow(ijk) = pwtemp[idxmax(ijk)]
;    a1t=fft((*(self->GetDati()))[*,0])
;    a2t=fft((*(self->GetDati()))[*,1])
;    p=self._niter*self._dt
;
;    if p*fstep lt 1 then fstep=1./p
;    a1t[0:p*(fvibmax(ijk)-fstep)-1]=0
;    a1t[p*(fvibmax(ijk)+fstep):p*(1./self._dt-fvibmax(ijk)-fstep)-1]=0
;    a1t[p*(1./self._dt-fvibmax(ijk)+fstep):*]=0
;    a2t[0:p*(fvibmax(ijk)-fstep)-1]=0
;    a2t[p*(fvibmax(ijk)+fstep):p*(1./self._dt-fvibmax(ijk)-fstep)-1]=0
;    a2t[p*(1./self._dt-fvibmax(ijk)+fstep):*]=0
;    rm1[*,ijk]=fft(a1t,1)
;    rm2[*,ijk]=fft(a2t,1)
;    ab1 = linfit(rm1[*,ijk],rm2[*,ijk])
;    ab2 = linfit(rm2[*,ijk],rm1[*,ijk])
;    cor1 = variance( rm2[*,ijk]-ab1[1]*rm1[*,ijk]-ab1[0] )
;    cor2 = variance( rm1[*,ijk]-ab2[1]*rm2[*,ijk]-ab2[0] )
;    if cor1 lt cor2 then begin
;      cor[ijk]=cor1
;      ab[*,ijk]=ab1
;      var[ijk] = variance( rm2[*,ijk] )
;    endif else begin
;      cor[ijk]=cor2
;      ab[*,ijk]=ab2
;      xy[ijk]=1
;      var[ijk] = variance( rm1[*,ijk] )
;    endelse
;    if ijk eq 0 then begin
;      if plot eq 1 then $
;        plot, 1.1*minmax([rm1[*,ijk],rm2[*,ijk]]), 1.1*minmax([rm1[*,ijk],rm2[*,ijk]]), $
;            xtitle='direction 0', ytitle='direction 1', title='vibrations from '+strtrim(from_freq,2)+'Hz to '+strtrim(to_freq,2)+'Hz', charsize=1.2, /nodata
;      if plot eq 1 then $
;        oplot, rm1[*,ijk], rm2[*,ijk], psym=3
;      frvib= fvibmax(ijk)
;      colo=-1
;    endif else begin
;      if plot eq 1 then $
;        oplot, rm1[*,ijk], rm2[*,ijk], psym=3, col=CC[ijk]
;      frvib=[frvib, fvibmax(ijk)]
;      colo=[colo,CC[ijk]]
;    endelse
;    if plot eq 1 then begin
;      if fvibmax(ijk) lt to_freq and fvibmax(ijk) gt from_freq then begin
;        if xy[ijk] eq 0 then oplot, 1.2*minmax(rm1[*,0]), ab[1,ijk]*1.2*minmax(rm1[*,0])+ab[0,ijk], col=CC[ijk] $
;          else oplot, ab[1,ijk]*1.2*minmax(rm2[*,0])+ab[0,ijk], 1.2*minmax(rm2[*,0]), col=CC[ijk]
;      endif
;    endif
;  endfor
;  if plot eq 1 then $
;    legend, strtrim(frvib,2)+'Hz', psym=fltarr(nnn)-1, col=colo
;  angle=( (-1)^xy*atan(ab[1,*])+xy*!pi/2 )*180/!pi
;
;  directions={$
;    freq: frvib, $
;    power: pow, $
;    error_var: cor, $
;    signal_var: var, $
;    angle: angle $
;    }
;
;  return, directions
;end

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
    obj->addMethodHelp, "power(idx, from_freq=from, to_freq=to, /cumulative, /sumspectra)", "return power of idx-th spectrum between frequencies from_freq and to_freq, eventually cumulating on freqs and/or summing on spectra"
    obj->addMethodHelp, "findPeaks(idx, from_freq=from, to_freq=to, t100=t100)", "return the peaks of idx-th spectrum between frequencies from_freq and to_freq, t100 = % threshold of the returned results"
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
    ptr_free, self._time_variance
    ptr_free, self._time_average
    ptr_free, self._ensemble_variance
    ptr_free, self._ensemble_average
    ptr_free, self._freq
    ptr_free, self._peaks
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


