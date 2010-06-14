
;+
;
; Residual modes m=Rs
;-

function AOresidual_modes::Init, root_obj, slopes, rec
    self._rec    =  ptr_new(rec)
    self._slopes =  ptr_new(slopes)

    self._store_fname     = filepath(root=root_obj->elabdir(), 'residual_modes.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'residual_modes_psd.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
    endif

    if not self->AOtime_series::Init((root_obj->frames_counter())->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('[nm-wf Hz^{-1/2}]')
	self._plots_title = root_obj->tracknum()

    ; create residual_modes and analyze
    ;self->datiProducer

    ;self->AOtime_series::Compute


    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOresidual_modes', 'Modal wavefront residue') then return, 0
    self->addMethodHelp, "modes()", "modal residue [niter, nmodes]"
    self->addMethodHelp, "slopes()", "reference to slopes object"
    self->addMethodHelp, "rec()", "reference to modal reconstructor object"
    self->addMethodHelp, "nmodes()", "number of residual modes"
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOresidual_modes::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname, /v
    endif else begin
        ; compute residual modes from slopes and modal rec
        t_rec = (*self._rec->rec())[*,*self._rec->modes_idx()]
        modes =  t_rec ##  *self._slopes->slopes()
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

function AOresidual_modes::slopes
    if (OBJ_VALID(*self._slopes)) THEN return, *self._slopes else return, 0d
end

function AOresidual_modes::rec
    if (OBJ_VALID(*self._rec)) THEN return, *self._rec else return, 0d
end

function AOresidual_modes::nmodes
    return, self->AOtime_series::nseries()
end

pro AOresidual_modes::plotJitter, _extra=ex
    plot, self->freq(), sqrt(self->power(0, /cum)+self->power(1, /cum)), title=self._plots_title, _extra=ex
    oplot, self->freq(), sqrt(self->power(0, /cum)), col=255
    oplot, self->freq(), sqrt(self->power(1, /cum)), col=255L*256

    ;sigmatot2 = max ( self->power(0, /cum)+self->power(1, /cum) ) / 2
    ;DpupM = 8.22	;m
    ;ldmas = self->lambda() / DpupM / 4.848d-6 ; l/D in arcsec
    ;print, 'SR attenuation due to TT jitter ', 1. / (1. + (!pi^2 /2 )*( sqrt(sigmatot2)/ ldmas)^2)
end


; to be implemented in AOtime_series subclasses
function AOresidual_modes::GetDati
    if not ptr_valid(self._modes) then self->datiProducer
    return, self._modes
end

pro AOresidual_modes::free
    ptr_free, self._modes
    self->AOtime_series::free
end


pro AOresidual_modes::Cleanup
    ;obj_destroy, self._obj_psd
    ptr_free, self._modes
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOresidual_modes__define
    struct = { AOresidual_modes, $
        _modes         : ptr_new(), $
        _slopes        : ptr_new(), $
        _rec           : ptr_new(), $
        _store_fname   : "" ,       $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }

end

