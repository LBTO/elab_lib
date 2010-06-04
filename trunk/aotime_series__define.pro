
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
        restore, self._store_psd_fname, /v
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


