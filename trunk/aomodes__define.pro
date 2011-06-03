
;+
;
; Modes
;-

function AOmodes::Init, root_obj, modes_file, fc_obj
    if not file_test(modes_file) then begin
        message, modes_file + ' not found', /info
        return, 0
    endif
    self._fname = modes_file
    self._fc_obj = fc_obj
    self._fitsheader = ptr_new(headfits(self._fname, /SILENT), /no_copy)

    self._store_fname = filepath(root=root_obj->elabdir(), 'modes.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'modes_psd.sav')
    self._store_peaks_fname = filepath(root=root_obj->elabdir(), 'modes_peaks.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._store_peaks_fname, /allow_nonexistent
    endif

    if not self->AOtime_series::Init(fc_obj->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('[nm-wf Hz^{-1/2}]')
	self._plots_title = root_obj->tracknum()

	;Initialize WF
    if not self->AOwf::Init(root_obj, root_obj->modeShapes()) then message, 'WF object not available', /info

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOmodes', 'Represent integrated modes') then return, 0
    self->addMethodHelp, "fname()", "modesfile name (string)"
    self->addMethodHelp, "header()", "header of modesfile (strarr)"
    self->addMethodHelp, "modes()", "integrated modes matrix [nmodes,niter]"
    self->addMethodHelp, "nmodes()", "number of modes"
    self->AOwf::addHelp, self
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOmodes::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        modes = readfits(self._fname, /SILENT)
        modes = transpose(temporary(modes))
        modes  = interpolate_with_frames_counter(modes, self._fc_obj)
        save, modes, file=self._store_fname
    endelse
    self._modes = ptr_new(modes, /no_copy)

end

function AOmodes::fname
    return, self._fname
end

function AOmodes::header
    if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, 0d
end

function AOmodes::modes, _extra=ex
    return, self->dati(_extra=ex)
end

function AOmodes::nmodes
    return, self->AOtime_series::nseries()
end

; to be implemented in AOtime_series subclasses
function AOmodes::GetDati
    if not ptr_valid(self._modes) then self->datiProducer
    return, self._modes
end

pro AOmodes::free
    if ptr_valid(self._modes) then ptr_free, self._modes
    if ptr_valid(self._fitsheader) then ptr_free, self._fitsheader
    self->AOwf::free
    self->AOtime_series::free
end


pro AOmodes::Cleanup
    if ptr_valid(self._modes) then ptr_free, self._modes
    self->AOwf::Cleanup
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOmodes__define
    struct = { AOmodes, $
        _fname            : "", $
        _fitsheader       :  ptr_new(), $
        _modes            :  ptr_new(), $
        _fc_obj           :  obj_new(), $
        _store_fname      :  ""		  , $
        INHERITS    AOwf, $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }

end

