
;+
;
;-

function AOtimestamps::Init, root_obj, timestamps_filename

    if file_test(timestamps_filename) eq 0 then begin
        message, 'Cannot find timestamps file: '+timestamps_filename, /inform
        return, 0
    endif

    self._filename = timestamps_filename

    self._header = ptr_new(headfits(self._filename, /SILENT), /no_copy)
    self._nframes  = long(aoget_fits_keyword(self->header(), 'NAXIS2'))

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOtimestamps', 'Represent system timestamps') then return, 0
    self->addMethodHelp, "timestamps_fname()", "timestamps file name (string)"
    self->addMethodHelp, "header()", "header of timestamps fits file (strarr)"
    self->addMethodHelp, "timestamps()", "timestamps in seconds, double[nframes]"
    self->addMethodHelp, "nframes()", "number of timestamps saved (long)"
    return, 1
end

function AOtimestamps::timestamps_fname
    return, self._filename
end

function AOtimestamps::nframes
    return, self._nframes
end

function AOtimestamps::header
    if (PTR_VALID(self._header)) THEN return, *(self._header) else return, 0d
end


;+
; RESTORES THE TIMESTAMPS DATA
;-
function AOtimestamps::timestamps
    fr = double(readfits(self._filename, /SILENT))
    if n_elements(fr) eq 1 then return,-1
    return, fr
end


pro AOtimestamps::free
    if ptr_valid(self._header) then ptr_free, self._header
end

pro AOtimestamps::Cleanup
    self->free
    self->AOhelp::Cleanup
end

pro AOtimestamps__define
    struct = { AOtimestamps,             $
        _filename          : ""        , $
        _header            : ptr_new() , $
        _nframes           : 0L	       , $
        INHERITS AOhelp                  $ 
    }
end
