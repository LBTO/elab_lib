
;+
; loop delay
;-

function AOdelay::Init, root_obj
    self._root_obj = root_obj
    self._delay = ((self._root_obj->wfs_status())->camera())->computeDelay()
    if not self->AOhelp::Init('AOdelay', 'Compute closed loop delay') then return, 0
    self->addMethodHelp, "delay()", "loop delay in seconds"
    self->addMethodHelp, "frames_delay()", "loop delay in frames"
    return, 1
end

function AOdelay::delay
    return, self._delay
end

function AOdelay::frames_delay
    fs = ((self._root_obj->wfs_status())->camera())->framerate()
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


