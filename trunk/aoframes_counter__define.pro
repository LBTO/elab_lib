

function AOframes_counter::Init, frames_counter_file, wfs_status_obj
    if not file_test(frames_counter_file) then return,0
    self._frames_counter_file = frames_counter_file

    if file_test(frames_counter_file) eq 0 then begin
        message, 'Cannot find frames_counter file: '+frames_counter_file, /inform
        return, 0
    endif

    if not obj_valid(wfs_status_obj) then return, 0
    if not obj_valid(wfs_status_obj->ccd39()) then return, 0
    self._framerate=(wfs_status_obj->ccd39())->framerate()
;    self->compute
	frames_counter = readfits(frames_counter_file, header, /SILENT)
    self._nframes = n_elements(frames_counter)
	if self._nframes le 2 then return,0

    self._fc = ptr_new(frames_counter, /no_copy)
    self._header = ptr_new(header, /no_copy)
;    dfc = (*self._fc-shift(*self._fc,1) ) [1:*]
;    self._decimation = min(dfc)-1
;    self._deltat = 1. / ( (wfs_status_obj->ccd39())->framerate() ) * (self._decimation+1)
;    self._lost_frames_idx =  ptr_new( where(dfc gt self._decimation+1, cnt), /no_copy)
;    self._n_jumps = cnt
;    if self._n_jumps gt 0 then self._lost_frames  =  ptr_new( dfc[*self._lost_frames_idx]/(self._decimation+1) - 1, /no_copy)
	self._decimation = -1
	self._deltat = -1
	self._n_jumps = -1

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOframescounter', 'Frames counter: decimation and lost frames') then return, 0
    self->addMethodHelp, "fname()",      "frames counter file name (string)"
    self->addMethodHelp, "header()",      "frames counter file header (strarr)"
    self->addMethodHelp, "frames_counter()",      "frames counter vector (lonarr)"
    self->addMethodHelp, "isok(cause=cause)",     "return 1 if frames counter is ok, 0 otherwise. cause contains error description "
    self->addMethodHelp, "decimation()",      "return frames counter decimation (long)"
    self->addMethodHelp, "deltat()",      "return interval between diagnostic frames (seconds): (decimation+1)/ccdrate "
    self->addMethodHelp, "nframes()",	  "returns number of frames available"
    self->addMethodHelp, "n_jumps()",  "return how many times a frame loss was detected (long)"
    self->addMethodHelp, "lost_frames_idx()", "return indexes of frames counter vector where frames loss was detected (array, long)"
    self->addMethodHelp, "lost_frames()",     "return number of frames lost (array, long)"
    return, 1
end

pro AOframes_counter::compute
;    self._fc = ptr_new( readfits(self._frames_counter_file, header, /SILENT), /no_copy)
;    self._header = ptr_new(header, /no_copy)
;    self._nframes = n_elements(*self._fc)
    dfc = (*self._fc-shift(*self._fc,1) ) [1:*]
    self._decimation = min(dfc)-1
    self._deltat = 1. / self._framerate  * (self._decimation+1)
    self._lost_frames_idx =  ptr_new( where(dfc gt self._decimation+1, cnt), /no_copy)
    self._n_jumps = cnt
    if self._n_jumps gt 0 then self._lost_frames  =  ptr_new( dfc[*self._lost_frames_idx]/(self._decimation+1) - 1, /no_copy)
end

function AOframes_counter::fname
    return, self._frames_counter_file
end

function AOframes_counter::header
    return, self._header
end

function AOframes_counter::frames_counter
    return, *self._fc
end

function AOframes_counter::nframes
	return, self._nframes
end

;
; return 1 in case frame counter is ok, 0 otherwise
; additional explanations are added to the string cause
;
function AOframes_counter::isok, cause=cause
    if self->n_jumps() eq 0 then begin
        return, 1
    endif
    if self->decimation() gt 100 then begin
        cause += " - suspect high decimation "+strtrim(self->decimation(),2)
        return, 0
    endif
    max_stop = max(self->lost_frames(),idx)
    if max_stop gt 50 then begin
        cause += " - long stop of "+strtrim(max_stop,2) +" frames at frame "+strtrim((self->lost_frames_idx())[idx],2)
        return, 0
    endif
    total_stop = total(self->lost_frames())
    if max_stop gt 100 then begin
        cause += " - total number of lost frames: "+strtrim(total_stop,2)
        return, 0
    endif
    return, 1
end

;
; interval between diagnostic frames (seconds): (decimation+1)/ccdrate
;
function AOframes_counter::deltat
	if self._deltat eq -1 then self->compute
    return, self._deltat
end

;
; number of jumps detected in the frames counter array
;
function AOframes_counter::n_jumps
	if self._n_jumps eq -1 then self->compute
    return, self._n_jumps
end

function AOframes_counter::decimation
	if self._decimation eq -1 then self->compute
    return, self._decimation
end

function AOframes_counter::lost_frames_idx
    if not (ptr_valid(self._lost_frames_idx)) then self->compute
    return, *self._lost_frames_idx
end

;
; decimation is accounted for in the computation of lost_frames
function AOframes_counter::lost_frames
    if not (ptr_valid(self._lost_frames)) then self->compute
    return, *self._lost_frames
end

pro AOframes_counter::free
    ;if ptr_valid(self._lost_frames_idx) then ptr_free, self._lost_frames_idx
    ;if ptr_valid(self._lost_frames) then ptr_free, self._lost_frames
    ;if ptr_valid(self._fc) then ptr_free, self._fc
end

pro AOframes_counter::Cleanup
    ptr_free, self._lost_frames_idx
    ptr_free, self._lost_frames
    ptr_free, self._header
    ptr_free, self._fc
    self->AOhelp::Cleanup
end


pro AOframes_counter__define
    struct = { AOframes_counter, $
        _frames_counter_file : "" , $
        _framerate           : 0., $
        _header              : ptr_new(), $
        _decimation          : 0L, $
        _n_jumps             : 0L, $
        _deltat              : 0d, $
        _nframes			 : 0L, $
        _fc                  : ptr_new(), $
        _lost_frames_idx     : ptr_new(), $
        _lost_frames         : ptr_new(), $
        INHERITS AOhelp $
    }
end
