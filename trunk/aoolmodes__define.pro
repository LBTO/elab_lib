
;+
;
; Open Loop Modes
;-

function AOolmodes::Init, root_obj, residual_modes, commands_modes, fc_obj
    self._fc_obj = fc_obj
    self._residual_modes = ptr_new(residual_modes)
    self._commands_modes = ptr_new(commands_modes)
    self._dt = fc_obj->deltat()
    self._fr = ((root_obj->wfs_status())->ccd39())->framerate()
    self._nframes = (root_obj->frames_counter())->nframes()
    self._store_fname = filepath(root=root_obj->elabdir(), 'olmodes.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'olmodes_psd.sav')
    if root_obj->recompute() eq 1B then begin
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
    if not self->AOhelp::Init('AOolmodes', 'Represent reconstructed open loop modes') then return, 0
    
    ;self->addMethodHelp, "fname()", "modesfile name (string)"
    ;self->addMethodHelp, "header()", "header of modesfile (strarr)"
    
    self->addMethodHelp, "modes()", "reconstructed open loop modes matrix [nmodes,niter]"
    self->addMethodHelp, "nmodes()", "number of modes"
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOolmodes::datiProducer
    if file_test(self._store_fname) then begin
        restore, self._store_fname, /v
    endif else begin
        mir_delay = 1d-3
        wfs_delay = 1d/self._fr
        delay = mir_delay + wfs_delay
        frac = delay / self._dt
        ntemp = self._nframes-2-fix(frac)
        temp_comm = dblarr(ntemp,672)
        for i=0, 671 do temp_comm[*,i] = interpolate((*self._commands_modes->modes())[*,i],findgen(ntemp)+frac)
        modes = (*self._residual_modes->modes())[0:ntemp,*] + temp_comm
        save, modes, file=self._store_fname
    endelse
    self._modes = ptr_new(modes, /no_copy)
end

function AOolmodes::modes, _extra=ex
    return, self->dati(_extra=ex)
end

function AOolmodes::nmodes
    return, self->AOtime_series::nseries()
end

; to be implemented in AOtime_series subclasses
function AOolmodes::GetDati
    if not ptr_valid(self._modes) then self->datiProducer
    return, self._modes
end

pro AOolmodes::free
    ptr_free, self._modes
    self->AOtime_series::free
end


pro AOolmodes::Cleanup
    ptr_free, self._modes
    ptr_free, self._fitsheader
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOolmodes__define
    struct = { AOolmodes, $
        _modes            :  ptr_new(), $
        _fc_obj           :  obj_new(), $
        _residual_modes   :  ptr_new(), $
        _commands_modes   :  ptr_new(), $
        _fr               :  0., $
        _nframes          :  0L, $
        _store_fname      :  "", $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }

end

