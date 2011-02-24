
;+
;
; OffloadModes
;-

function AOoffloadmodes::Init, root_obj, offload_matrix_fname
	if not file_test(offload_matrix_fname) then return,0
    self._fname = offload_matrix_fname

    self._pos_obj = root_obj->positions()
    self._fc_obj  = root_obj->frames_counter()
    if not obj_valid(self._pos_obj) then return, 0

    self._store_fname = filepath(root=root_obj->elabdir(), 'offloadmodes.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'offloadmodes_psd.sav')
    self._store_peaks_fname = filepath(root=root_obj->elabdir(), 'offloadmodes_peaks.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._store_peaks_fname, /allow_nonexistent
    endif

    if not self->AOtime_series::Init(self._fc_obj->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
;	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
;	self._spectra_units = textoidl('[nm Hz^{-1/2}]')
	self._plots_title = root_obj->tracknum()

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOoffloadmodes', 'Represent offload zernike modes') then return, 0
    self->addMethodHelp, "offloadmodes()", "offload zernike modes matrix [noffloadmodes,niter]"
    self->addMethodHelp, "noffloadmodes()", "number of offload modes"
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOoffloadmodes::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        restore, self._fname  ; pmpos, f2p, cl_act
        offloadmodes = pmpos ## (self._pos_obj->positions() -  ( self._pos_obj->flatpositions() ## replicate(1.0, self._pos_obj->niter()) ) )
        save, offloadmodes, file=self._store_fname
    endelse
    self._offloadmodes = ptr_new(offloadmodes, /no_copy)

end

function AOoffloadmodes::fname
    return, self._fname
end

function AOoffloadmodes::offloadmodes, _extra=ex
    return, self->dati(_extra=ex)
end

function AOoffloadmodes::noffloadmodes
    return, self->AOtime_series::nseries()
end

; to be implemented in AOtime_series subclasses
function AOoffloadmodes::GetDati
    if not ptr_valid(self._offloadmodes) then self->datiProducer
    return, self._offloadmodes
end

pro AOoffloadmodes::free
    if ptr_valid(self._offloadmodes) then ptr_free, self._offloadmodes
    self->AOtime_series::free
end


pro AOoffloadmodes::Cleanup
    if ptr_valid(self._offloadmodes) then ptr_free, self._offloadmodes
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOoffloadmodes__define
    struct = { AOoffloadmodes, $
        _fname             : "" , $
        _offloadmodes      :  ptr_new(), $
        _pos_obj           :  obj_new(), $
        _fc_obj            :  obj_new(), $
        _store_fname      : "", $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }

end


