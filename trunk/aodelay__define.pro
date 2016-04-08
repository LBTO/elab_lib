
;+
; loop delay
;-

function AOdelay::Init, root_obj

    self._root_obj = root_obj
    fs = ((self._root_obj->wfs_status())->ccd39())->framerate()
    binning = ((self._root_obj->wfs_status())->ccd39())->binning()
    T = 1/fs

    ;FLAO computation time (slope computation not parallelized)
    comp_time = 0.72e-3 
    ; ASM set time
    dm_set = 1e-3
	;CCD39 characteristics for binning [1,2,3,4]
   	ReadOutSpeed = [0.95, 1.56, 1.90, 0.68]*1e-3
    CCD_speed = ReadOutSpeed[binning-1]

    ; delay in seconds
    self._delay = T/2. + CCD_speed + comp_time + dm_set/2. + T/2.

    if not self->AOhelp::Init('AOdelay', 'Compute closed loop delay') then return, 0
    self->addMethodHelp, "delay()", "loop delay in seconds"
    self->addMethodHelp, "frames_delay()", "loop delay in frames"
    return, 1
end

function AOdelay::delay
    return, self._delay
end

function AOdelay::frames_delay
    fs = ((self._root_obj->wfs_status())->ccd39())->framerate()
    return, self._delay*fs
end

pro AOdelay::test
    d=self->delay()
end

pro AOdelay::free
end

pro AOdelay::Cleanup
  self->AOhelp::Cleanup
end

pro AOdelay__define
struct = { AOdelay, $
    _root_obj          : obj_new(), $
    _delay             : 0.0,       $
    INHERITS    AOhelp 		        $
}
end


