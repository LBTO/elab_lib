
;+
;
; modalpositions
; mirror positions projected on modal basis
;-

function AOmodalpositions::Init, root_obj

    self._pos_obj = root_obj->positions()
    self._m2c_obj = root_obj->control()
    self._fc_obj  = root_obj->frames_counter()
    if not obj_valid(self._pos_obj) then return, 0

    self._store_fname = filepath(root=root_obj->elabdir(), 'modalpositions.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'modalpositions_psd.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
    endif

    if not self->AOtime_series::Init(self._fc_obj->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('[nm-wf Hz^{-1/2}]')
	self._plots_title = root_obj->tracknum()

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOmodalpositions', 'Represent mirror positions projected on modal basis') then return, 0
    self->addMethodHelp, "modalpositions()",  "mirror position modes matrix [nmodes,niter]"
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOmodalpositions::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        modalpositions = self._m2c_obj->c2m() ## self._pos_obj->positions()
        save, modalpositions, file=self._store_fname
    endelse
    self._modalpositions = ptr_new(modalpositions, /no_copy)

end

function AOmodalpositions::modalpositions, _extra=ex
    return, self->dati(_extra=ex)
end

function AOmodalpositions::nmodes
    return, self->AOtime_series::nseries()
end

; to be implemented in AOtime_series subclasses
function AOmodalpositions::GetDati
    if not ptr_valid(self._modalpositions) then self->datiProducer
    return, self._modalpositions
end

pro AOmodalpositions::free
    if ptr_valid(self._modalpositions) then ptr_free, self._modalpositions
    self->AOtime_series::free
end


pro AOmodalpositions::Cleanup
    if ptr_valid(self._modalpositions) then ptr_free, self._modalpositions
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOmodalpositions__define
    struct = { AOmodalpositions, $
        _modalpositions    :  ptr_new(), $
        _m2c_obj           :  obj_new(), $
        _pos_obj           :  obj_new(), $
        _fc_obj            :  obj_new(), $
        _store_fname      : "", $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }

end

