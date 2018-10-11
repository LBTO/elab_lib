;+
;
; Residual modes: Measurement of residual wf in the pupil projected on the control modal basis.
; m = alfa * R * s
;
; modes are in meter rms, surface.
; To convert to wavefront use self->norm_factor()
;
;	 R: reconstruction matrix
;
;	 s: slopes vector
;
;    alfa: gain factor (optical pyramid gain (TO BE IMPLEMENTED) and IM mismatch factor
;
; NOTE: since IMs were calibrated with retroreflector, and NOT divided by 2 to take into account
;       of the double-pass, ONSKY residual modes are under-estimated by a factor 2.
;       This factor is now taken into account below as self._mistmatch_factor
;
;-

function AOresidual_modes::Init, root_obj, slopes_obj, modal_rec_obj, store_label=store_label

	if not obj_valid(modal_rec_obj) then return,0
	if not obj_valid(slopes_obj) then return,0

    if not keyword_set(store_label) then store_label=''

    self._store_fname     = filepath(root=root_obj->elabdir(), store_label+'.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), store_label+'_psd.sav')
    self._store_peaks_fname = filepath(root=root_obj->elabdir(), store_label+'_peaks.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._store_peaks_fname, /allow_nonexistent
    endif

    self._root_obj      = root_obj
    self._slopes_obj    = slopes_obj
    self._modal_rec_obj = modal_rec_obj
    self._mistmatch_factor = self._root_obj->operation_mode() eq 'RR' ? 1. : 2.

    ; initialize time series
    if not self->AOtime_series::Init((root_obj->frames_counter())->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._norm_factor   = 1e9 * self._root_obj->reflcoef() * self._mistmatch_factor	;nm wf
	self._spectra_units = textoidl('[nm-wf]')
	self._plots_title = root_obj->tracknum()

	;Initialize WF
	;self._wf = obj_new('AOwf', self._root_obj, root_obj->modeShapes(), self)
    if not self->AOwf::Init(self._root_obj, self._root_obj->modeShapes()) then message, 'WF object not available', /info

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOresidual_modes', 'Modal wavefront residue') then return, 0
    self->addMethodHelp, "modes()", "modal residue (m rms, surface) [niter, nmodes]"
    self->addMethodHelp, "nmodes()", "number of residual modes"
    ;if obj_valid(self._wf) then self->addleaf, self._wf, 'wf'
    self->AOwf::addHelp, self
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOresidual_modes::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        ; compute residual modes from slopes and modal rec
        t_rec = (self._modal_rec_obj->rec())[*, 0:self._modal_rec_obj->lastmode()]
        modes = t_rec ##  self._slopes_obj->slopes()
        save, modes, file=self._store_fname
    endelse
    self._modes = ptr_new(modes, /no_copy)

end

;function AOresidual_modes::variance
;    if (PTR_VALID(self._variance)) THEN return, *(self._variance) else return, 0d
;end

function AOresidual_modes::modes, _extra=ex ;TODO add extra for series_idx or iter_idx
    ;if (PTR_VALID(self._modes)) THEN return, *(self._modes) else return, 0d
    return, self->dati(_extra=ex)
end

function AOresidual_modes::nmodes
    return, self->AOtime_series::nseries()
end

pro AOresidual_modes::plotJitter, from_freq=from_freq, to_freq=to_freq, _extra=ex, overplot=overplot
    coeff2arcsec = self._root_obj->reflcoef() * 4 / ao_pupil_diameter() / 4.848d-6 * self._mistmatch_factor
    freq = self->freq(from=from_freq, to=to_freq)
    tip  = self->power(0, from=from_freq, to=to_freq, /cum) * coeff2arcsec^2
    tilt = self->power(1, from=from_freq, to=to_freq, /cum) * coeff2arcsec^2
    if not keyword_set(overplot) then begin
    	plot, freq, sqrt(tip + tilt), xticklen=1, yticklen=1, xgridstyle=1, ygridstyle=1, $
        	title=self._plots_title, xtitle='Freq [Hz]', ytitle='Cumulated PSD [arcsec rms]', _extra=ex
    	oplot, freq, sqrt(tip), col='0000ff'x
    	oplot, freq, sqrt(tilt), col='00ff00'x
    	legend, ['Tilt+Tip', 'Tip', 'Tilt'],linestyle=[0,0,0],colors=[!P.COLOR, '0000ff'x, '00ff00'x], charsize=1.2
	endif else begin
    	oplot, freq, sqrt(tip + tilt)
    	oplot, freq, sqrt(tip), col='0000ff'x
    	oplot, freq, sqrt(tilt), col='00ff00'x
	endelse

    sigmatot2 = max ( tip + tilt)  / 2
    ldmas = 1.6d-6 / ao_pupil_diameter() / 4.848d-6 ; l/D in arcsec
    print, 'SR attenuation in H band due to TT jitter ', 1. / (1. + (!pi^2 /2 )*( sqrt(sigmatot2)/ ldmas)^2)
end


; to be implemented in AOtime_series subclasses
function AOresidual_modes::GetDati
    if not ptr_valid(self._modes) then self->datiProducer
    return, self._modes
end

pro AOresidual_modes::free
    if ptr_valid(self._modes) then ptr_free, self._modes
    self->AOwf::free
    self->AOtime_series::free
end


pro AOresidual_modes::Cleanup
    ;obj_destroy, self._obj_psd
    if ptr_valid(self._modes) then ptr_free, self._modes
    self->AOwf::Cleanup
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end


function AOresidual_modes::mistmatch_factor
	return, self._mistmatch_factor
end


pro AOresidual_modes__define
    struct = { AOresidual_modes, $
        _modes           : ptr_new(), $
        _store_fname     : "" 	  	, $
        _root_obj        : obj_new(), $
        _modal_rec_obj   : obj_new(), $
        _slopes_obj      : obj_new(), $
        _mistmatch_factor : 0.	  	, $
        INHERITS    AOwf, $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }

end