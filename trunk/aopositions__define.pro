
;+
;
; positions
;-

function AOpositions::Init, root_obj, positions_file, fc_obj
	if not file_test(positions_file) then begin
        message, positions_file + ' not found', /info
        return,0
    endif
    self._fname = positions_file
    self._fc_obj = fc_obj
    self._fitsheader = ptr_new(headfits(self._fname, /SILENT), /no_copy)
    self._root_obj = root_obj

    self._store_fname = filepath(root=self._root_obj->elabdir(), 'positions.sav')
    self._store_psd_fname = filepath(root=self._root_obj->elabdir(), 'positions_psd.sav')
    if self._root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
    endif

    if not self->AOtime_series::Init(fc_obj->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('[nm-wf Hz^{-1/2}]')
	self._plots_title = root_obj->tracknum()

    ;self->datiProducer
    ;self->AOtime_series::Compute

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOpositions', 'Represent positions') then return, 0
    self->addMethodHelp, "fname()", "positionsfile name (string)"
    self->addMethodHelp, "header()", "header of positionsfile (strarr)"
    self->addMethodHelp, "positions()", "positions matrix [npositions,niter]"
    self->addMethodHelp, "npositions()", "number of positions"
    self->addMethodHelp, "flatpositions()", "positions of flat shape"
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOpositions::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        positions = readfits(self._fname, /SILENT)
        positions = transpose(temporary(positions))
        positions  = interpolate_with_frames_counter(positions, self._fc_obj)
        save, positions, file=self._store_fname
    endelse
    self._positions = ptr_new(positions, /no_copy)

end

function AOpositions::fname
    return, self._fname
end

function AOpositions::header
    if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, 0d
end

function AOpositions::positions, _extra=ex
    return, self->dati(_extra=ex)
end

function AOpositions::npositions
    return, self->AOtime_series::nseries()
end

; to be implemented in AOtime_series subclassess
function AOpositions::GetDati
    if not ptr_valid(self._positions) then self->datiProducer
    return, self._positions
end

function AOpositions::flatpositions
    restore, ao_datadir()+path_sep()+(self._root_obj->adsec_status())->shape_file()
    flat=flattened_status.position
    return, flat
end

pro AOpositions::free
    if ptr_valid(self._fitsheader) then ptr_free, self._fitsheader
    if ptr_valid(self._positions) then ptr_free, self._positions
    self->AOtime_series::free
end


pro AOpositions::Cleanup
    if ptr_valid(self._positions) then ptr_free, self._positions
    if ptr_valid(self._fitsheader) then ptr_free, self._fitsheader
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOpositions__define
    struct = { AOpositions, $
        _fname            : "", $
        _fitsheader       :  ptr_new(), $
        _positions         :  ptr_new(), $
        _fc_obj           :  obj_new(), $
        _root_obj         :  obj_new(), $
        _store_fname      : "", $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }

end


