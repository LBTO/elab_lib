

function AOvalid_pixels::Init, valid_pixels_file, wfs_status_obj
    if not file_test(valid_pixels_file) then return,0
    self._valid_pixels_file = valid_pixels_file
    if file_test(valid_pixels_file) eq 0 then begin
        message, 'Cannot find valid_pixels file: '+valid_pixels_file, /inform
        return, 0
    endif

    if not obj_valid(wfs_status_obj) then return, 0
    if not obj_valid(wfs_status_obj->camera()) then return, 0
    self._framerate=(wfs_status_obj->camera())->framerate()
    
    valid_pixels = readfits(valid_pixels_file, header, /SILENT)
    self._nframes = n_elements(valid_pixels)
    if self._nframes le 2 then return,0

    self._vp = ptr_new(valid_pixels, /no_copy)
    self._header = ptr_new(header, /no_copy)
    self._decimation = -1
    self._deltat = -1

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOvalid_pixels', 'Valid pixels: valid, non duplicated pixels in each frame') then return, 0
    self->addMethodHelp, "fname()",      "valid pixels file name (string)"
    self->addMethodHelp, "header()",      "valid pixels file header (strarr)"
    self->addMethodHelp, "all_frames_valid_pixels()",      "valid pixels vector (lonarr)"
    self->addMethodHelp, "isok(cause=cause)",     "return 1 if valid pixels is ok, 0 otherwise. cause contains error description "
    self->addMethodHelp, "decimation()",      "return valid pixels decimation (long)"
    self->addMethodHelp, "deltat()",      "return interval between diagnostic frames (seconds): (decimation+1)/ccdrate "
    self->addMethodHelp, "nframes()",	  "returns number of frames available"
    self->addMethodHelp, "duplicate_frames_idx()", "return indexes of duplicated frames (array, long)"
    self->addMethodHelp, "duplicate_frames_num()", "return number of duplicagted frames (long)"
    self->addMethodHelp, "original_frames_idx()", "return indexes of original frames (array, long)"
    self->addMethodHelp, "original_frames_num()", "return number of original frames (long)"
    return, 1
end

pro AOvalid_pixels::compute
    self._duplicate_frames_idx =  ptr_new( where(*self._vp eq 0 , cnti, COMPLEMENT=vpo, NCOMPLEMENT=cnto), /no_copy)
    self._duplicate_frames = cnti
    self._original_frames_idx = ptr_new( vpo, /no_copy)
    self._original_frames = cnto

    self._decimation = (cnti+cnto) / (cnto) -1
    self._deltat = 1. / self._framerate  * (self._decimation+1)
end

function AOvalid_pixels::fname
    return, self._valid_pixels_file
end

function AOvalid_pixels::header
    return, self._header
end

function AOvalid_pixels::all_frames_valid_pixels
    return, *self._vp
end

function AOvalid_pixels::nframes
	return, self._nframes
end

;
; return 1 in case frame counter is ok, 0 otherwise
; additional explanations are added to the string cause
;
function AOvalid_pixels::isok, cause=cause
    if self->decimation() gt 100 then begin
        cause += " - suspect high decimation "+strtrim(self->decimation(),2)
        return, 0
    endif
    max_stop = max(self->duplicate_frames(),idx)
    if max_stop gt 50 then begin
        cause += " - long stop of "+strtrim(max_stop,2) +" frames at frame "+strtrim((self->duplicate_frames_idx())[idx],2)
        return, 0
    endif
    total_stop = total(self->duplicate_frames())
    if max_stop gt 100 then begin
        cause += " - total number of duplicate frames: "+strtrim(total_stop,2)
        return, 0
    endif
    return, 1
end

;
; interval between diagnostic frames (seconds): (decimation+1)/ccdrate
;
function AOvalid_pixels::deltat
	if self._deltat eq -1 then self->compute
    return, self._deltat
end

function AOvalid_pixels::decimation
	if self._decimation eq -1 then self->compute
    return, self._decimation
end

function AOvalid_pixels::duplicate_frames_idx
    if not (ptr_valid(self._duplicate_frames_idx)) then self->compute
    return, *self._duplicate_frames_idx
end

;
; decimation is accounted for in the computation of duplicate_frames
function AOvalid_pixels::duplicate_frames_num
    if not (ptr_valid(self._duplicate_frames)) then self->compute
    return, self._duplicate_frames
end

function AOvalid_pixels::original_frames_idx
    if not (ptr_valid(self._duplicate_frames_idx)) then self->compute
    return, *self._original_frames_idx
end

function AOvalid_pixels::original_frames_num
    if not (ptr_valid(self._original_frames)) then self->compute
    return, self._original_frames
end

pro AOvalid_pixels::free

end

pro AOvalid_pixels::Cleanup
    ptr_free, self._duplicate_frames_idx
    ptr_free, self._duplicate_frames
    ptr_free, self._header
    ptr_free, self._vp
    self->AOhelp::Cleanup
end

pro AOvalid_pixels__define
    struct = { AOvalid_pixels, $
        _valid_pixels_file : "" , $
        _framerate           : 0., $
        _header              : ptr_new(), $
        _decimation          : 0L, $
        _deltat              : 0d, $
        _nframes			 : 0L, $
        _vp                  : ptr_new(), $
        _duplicate_frames_idx: ptr_new(), $
        _duplicate_frames    : 0L, $
        _original_frames_idx: ptr_new(), $
        _original_frames    : 0L, $
        INHERITS AOhelp $
    }
end
