

;+
;
;-


function AOspectra::Init, dati_obj, dt, fftwindow=fftwindow
    ; test parameter type
    self._dt   = dt
    self._dati_obj = dati_obj
    if n_elements(fftwindow) ne 0 then self._window = fftwindow else self._window = "hamming"
    ;self->Compute

    return, 1
end


pro AOspectra::Compute

    if file_test(self._store_psd_fname) then begin
        restore, self._store_psd_fname
    endif else begin
        dati = self._dati_obj->Dati()

        nfreqs   = n_elements(dati[*,0])/2
        nspectra   = n_elements(dati[0,*])
        dati_psd = fltarr(nfreqs, nspectra)
        for i=0,nspectra-1 do begin
            case strlowcase(self._window) of
                ""         :  begin & dati1d = dati[*,i] & winnorm = 1.0 & end
                "hanning"  :  begin & dati1d = HANNING(2*nfreqs) * dati[*,i] & winnorm = 1.0/0.375  & end ; winnorm = 1./mean(hanning(10)^2)
                "hamming"  :  begin & dati1d = HANNING(2*nfreqs, alpha=0.56) * dati[*,i] & winnorm = 1.0/0.4104 & end
            endcase
            fft1, dati1d, self._dt, fspec=fspec, psd=psd, /noplot
            dati_psd[*,i]=psd*winnorm
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

function AOspectra::nfreqs
    return, self._nfreqs
end

function AOspectra::nspectra
    return, self._nspectra
end

function AOspectra::psd, spectrum_idx
    IF not (PTR_VALID(self._psd)) THEN return, 0d
    if n_elements(spectrum_idx) eq 0 then return, *(self._psd) else return,  (*(self._psd))[*,spectrum_idx]
end

function AOspectra::freq
    IF (PTR_VALID(self._freq)) THEN return, *(self._freq) else return, 0d
end

;pro AOspectra::setfftwindow, fftwindow
;    self._window = strlowcase(ftwindow)
;    self->ComputeSpectra
;end

;
;
;
function AOspectra::power, spectrum_idx, from_freq=from_freq, to_freq=to_freq
    if n_elements(from_freq) eq 0 then from_freq = min(*(self._freq))
    if n_elements(to_freq)   eq 0 then to_freq = max(*(self._freq))
    if from_freq ge to_freq then message, "from_freq must be less than to_freq"
    if from_freq lt min(*(self._freq)) then from_freq = min(*(self._freq))
    if from_freq gt max(*(self._freq)) then from_freq = max(*(self._freq))
    if to_freq lt min(*(self._freq)) then to_freq = min(*(self._freq))
    if to_freq gt max(*(self._freq)) then to_freq = max(*(self._freq))

    idx_from = closest(from_freq, *(self._freq))
    idx_to   = closest(to_freq, *(self._freq))

    if n_elements(spectrum_idx) eq 0 then begin
        return, total( (*(self._psd))[idx_from:idx_to, *] )
    endif else begin
        return, total( (*(self._psd))[idx_from:idx_to, spectrum_idx],1 )
    endelse
end

;
; for testing purpose
;pro AOspectra::SetDatiSinus, ampl, npts, dt
;    if ptr_valid(self._dati) then ptr_free, self._dati
;    self._dati = ptr_new(ampl*sin(findgen(npts)/10) )
;    self._dt   = dt
;    self->ComputeSpectra
;end

pro AOspectra::Cleanup
    ;heap_free, self._dati
    heap_free, self._psd
    heap_free, self._freq
    ;self->AOhelp::Cleanup
end

pro AOspectra__define
    struct = { AOspectra, $
        _dati_obj          :  obj_new(), $
        _psd               :  ptr_new(), $
        _freq              :  ptr_new(), $
        _store_psd_fname   :  "", $
        _window            :  "", $
        _nfreqs            :  0L, $
        _nspectra          :  0L, $
        _dt                :  0d  $
        ;inherits AOHelp           $
    }
end

