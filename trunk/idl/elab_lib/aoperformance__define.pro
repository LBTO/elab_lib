

;+
; This object provides data obtained as elaboration of two measures: one open-loop, the other closed-loop
;-

function AOperformance::Init, elab_ol, elab_cl
    self._elab_ol = ptr_new(elab_ol)
    self._elab_cl = ptr_new(elab_cl)
    if (*self._elab_ol->modal_rec())->rec_file()  ne (*self._elab_cl->modal_rec())->rec_file()  then $
        message, 'open_loop and closed_loop measurements use differ modal reconstructor. Does this means that the modal basis is different?'
    return, 1
end

function AOperformance::elab_ol
    IF (PTR_VALID(self._elab_ol)) THEN return, *(self._elab_ol) else return, 0d 
end

function AOperformance::elab_cl
    IF (PTR_VALID(self._elab_cl)) THEN return, *(self._elab_cl) else return, 0d 
end

function AOperformance::RejTF
  
    if not ptr_valid(self._rejtf) then begin
        elab_ol = self->elab_ol()
        elab_cl = self->elab_cl()
        
        spectra_cl =  (elab_cl->residual_modes())->obj_psd()
        spectra_ol =  (elab_ol->residual_modes())->obj_psd()

        ; determine number of modes to use
        nmodes_ol =  spectra_ol->nspectra()
        nmodes_cl =  spectra_cl->nspectra()
        if nmodes_ol ne nmodes_cl then message, 'open_loop and closed_loop have different number of modes. TODO'
        nmodes = nmodes_ol
        
        ; determine frequencies
        nfreqs_ol = spectra_ol->nfreqs()
        nfreqs_cl = spectra_cl->nfreqs()
        freq_ol = spectra_ol->freq()
        freq_cl = spectra_cl->freq()
        
        ; TODO check and double-check this stupid algorithm
        if nfreqs_ol gt nfreqs_cl then begin
            freqs  = freq_cl
            psd_cl = spectra_cl->psd()
            psd_ol = psd_cl*0.0
            for ii=0, spectra_ol->nspectra()-1 do psd_ol[*,ii] = interpol(spectra_ol->psd(ii), freq_ol, freq_cl)  
        endif else begin
            freqs  = freq_ol
            psd_ol = spectra_ol->psd()
            psd_cl = psd_ol*0.0
            for ii=0, spectra_cl->nspectra()-1 do psd_cl[*,ii] = interpol(spectra_cl->psd(ii), freq_cl, freq_ol)  
        endelse
        

        rejtf = fltarr(n_elements(freqs), nmodes)
        for ii=0, nmodes-1 do rejtf[*,ii] = sqrt(psd_cl[*,ii]/psd_ol[*,ii])

        self._rejtf = ptr_new(RejTF, /no_copy)
        self._freqs = ptr_new(freqs, /no_copy)
    endif
    return, *(self._rejtf)
end

function AOperformance::MeanRejTF
	return, total(self->rejtf(),2)/n_elements( (self->rejtf())[0,*] )
end

function AOperformance::MeanRejTFdb 
    return, 20.*alog10( self->MeanRejTF()) 
end

function AOperformance::FreqRejTF
    r=self->RejTF() ;; TODO serve per calcolare le freqs . cambiare la logica
    if (PTR_VALID(self._freqs)) THEN return, *(self._freqs) else return, 0d 
end


pro AOperformance::PlotModalVariance
    plotModalVariance, self._elab_ol, self._elab_cl
end


pro AOperformance::Cleanup
    ; don't heap_free elab_ol and elab_cl, otherwise they are destroyed in the heap
    ptr_free, self._elab_ol
    ptr_free, self._elab_cl
    ptr_free, self._rejtf
    ptr_free, self._freqs
end


pro AOperformance__define
    struct = { AOperformance, $
        _elab_ol           :  ptr_new(), $
        _elab_cl           :  ptr_new(), $
        _rejtf             :  ptr_new(), $
        _freqs             :  ptr_new(), $ 
        INHERITS AOhelp $    
    }
end

