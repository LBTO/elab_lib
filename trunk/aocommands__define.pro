
;+
;
; Commands
;-

function AOcommands::Init, root_obj, commands_file, fc_obj
	if not file_test(commands_file) then begin
        message, commands_file + ' not found', /info
        return,0
    endif
    self._fname = commands_file
    self._fc_obj = fc_obj
    self._fitsheader = ptr_new(headfits(self._fname, /SILENT), /no_copy)

    self._store_fname = filepath(root=root_obj->elabdir(), 'commands.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'commands_psd.sav')
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
    if not self->AOhelp::Init('AOcommands', 'Represent commands') then return, 0
    self->addMethodHelp, "fname()", "commandsfile name (string)"
    self->addMethodHelp, "header()", "header of commandsfile (strarr)"
    self->addMethodHelp, "commands()", "commands matrix [ncommands,niter]"
    self->addMethodHelp, "ncommands()", "number of commands"
    self->AOtime_series::addHelp, self
    return, 1
end

pro AOcommands::datiProducer

    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        commands = readfits(self._fname, /SILENT)
        commands = transpose(temporary(commands))
        commands  = interpolate_with_frames_counter(commands, self._fc_obj)
        save, commands, file=self._store_fname
    endelse
    self._commands = ptr_new(commands, /no_copy)

end

function AOcommands::fname
    return, self._fname
end

function AOcommands::header
    if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, 0d
end

function AOcommands::commands, _extra=ex
    return, self->dati(_extra=ex)
end

function AOcommands::ncommands
    return, self->AOtime_series::nseries()
end

; to be implemented in AOtime_series subclasses
function AOcommands::GetDati
    if not ptr_valid(self._commands) then self->datiProducer
    return, self._commands
end

pro AOcommands::free
    if ptr_valid(self._commands) then ptr_free, self._commands
    ;if ptr_valid(self._fitsheader) then ptr_free, self._fitsheader
    self->AOtime_series::free
end


pro AOcommands::Cleanup
    if ptr_valid(self._commands) then ptr_free, self._commands
    if ptr_valid(self._fitsheader) then ptr_free, self._fitsheader
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

pro AOcommands__define
    struct = { AOcommands, $
        _fname            : "", $
        _fitsheader       :  ptr_new(), $
        _commands         :  ptr_new(), $
        _fc_obj           :  obj_new(), $
        _store_fname      : "", $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }

end







