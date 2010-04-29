

function AOtracknum::Init, tracknum
    if stregex(tracknum, '^[0-9]{8}_[0-9]{6}$', /bool) ne 1 then begin
        message, 'tracknum must be in the form YYYYMMDD_hhmmss', /inform
        return, 0
    endif

    self._tracknum = tracknum

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOtracknum', 'Represent tracking number') then return, 0
    self->addMethodHelp, "tracknum()", "return tracking number (string)"
    self->addMethodHelp, "julday()", "return julian date of tracking number (double)"
    return, 1
end

function AOtracknum::JulDay
    y  = fix(strmid(self._tracknum,  0, 4))
    m  = fix(strmid(self._tracknum,  4, 2))
    d  = fix(strmid(self._tracknum,  6, 2))
    hh = fix(strmid(self._tracknum,  9, 2))
    mm = fix(strmid(self._tracknum, 11, 2))
    ss = fix(strmid(self._tracknum, 13, 2))
    return, julday(m, d, y, hh, mm, ss)
end

function AOtracknum::tracknum
    return, self._tracknum
end

pro AOtracknum::Cleanup
    self->AOhelp::Cleanup
end


pro AOtracknum__define
    struct = { AOtracknum, $
        _tracknum           : "", $
        INHERITS AOhelp $
    }
end
