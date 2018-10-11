
;+
; loop sanity check
;-

function AOSanityCheck::Init, $
        crcerrors_fname, $
        fltimeout_fname, $
        loopclosed_fname, $
        pendingcounter_fname, $
        skipcounter_fname, $
        timestamp_fname, $
        wfsglobaltimeout_fname

    self._crcerrors_fname        = crcerrors_fname
    self._fltimeout_fname        = fltimeout_fname
    self._loopclosed_fname       = loopclosed_fname
    self._pendingcounter_fname   = pendingcounter_fname
    self._skipcounter_fname      = skipcounter_fname
    self._timestamp_fname        = timestamp_fname
    self._wfsglobaltimeout_fname = wfsglobaltimeout_fname
    self._closedloop        = -1.0
    self._crcerror          = -1L
    self._fltimeout         = -1L
    self._pendingcounter    = -1L
    self._skipcounter       = -1L
    self._timestamp         = -1L
    self._wfsglobaltimeout  = -1L
    self._isok = -2
    self._isok_cause = ''


    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOSanityCheck', 'Represent loop sanity check') then return, 0
    ;self->addMethodHelp, "crcerrors_fname()", "crc errors file name (string)"
    ;self->addMethodHelp, "fltimeout_fname()", "fast link timeout file name (string)"
    ;self->addMethodHelp, "loopclosed_fname()", "loop closed file name (string)"
    self->addMethodHelp, "ClosedLoop()", "return 0/1 if loop is open/closed, -1 if unknown, 0<x<1 if status changed"
    self->addMethodHelp, "CRCerror()", "return crc errors, 0=ok, 1=err, -1 if unknown"
    self->addMethodHelp, "FLTimeout()", "return Fast Link timeout, 0=ok, 1=err, -1 if unknown"
    self->addMethodHelp, "PendingCounter()", "return pending counter, 0=ok, 1=err, -1 if unknown"
    self->addMethodHelp, "SkipCounter()", "return skip counter, 0=ok, 1=err, -1 if unknown"
    self->addMethodHelp, "TimeStamp()", "return timestamp, 0=ok, 1=err, -1 if unknown"
    self->addMethodHelp, "WFSGlobalTimeout()", "return WFS global timeout, 0=ok, 1=err, -1 if unknown"
    self->addMethodHelp, "IsOK()", "return 1 if everything is OK, 0 otherwise, -1 if unknown"
    return, 1
end

;
; return 1 if everything is OK, 0 otherwise.
; additional explanations are added to the string cause
;
function AOSanityCheck::IsOK, cause=cause
    if n_elements(cause) eq 0 then cause=""
    if self._isok eq -2 then begin
        self._isok = 0B

        if (self->CRCerror() eq 0) and (self->FLTimeout() eq 0) and (self->PendingCounter() eq 0) $
            and (self->SkipCounter() eq 0) and (self->TimeStamp() eq 0) and (self->WFSGlobalTimeout() eq 0) then self._isok = 1 

        if self->CRCerror() eq -1 then self._isok_cause += " - CRC unknown"
        if self->FLTimeout() eq -1 then self._isok_cause += " - FLTimeout unknown"
        if self->PendingCounter() eq -1 then self._isok_cause += " - PendingCounter unknown"
        if self->SkipCounter() eq -1 then self._isok_cause += " - SkipCounter unknown"
        if self->TimeStamp() eq -1 then self._isok_cause += " - TimeStamp unknown"
        if self->WFSGlobalTimeout() eq -1 then self._isok_cause += " - WFSGlobalTimeout unknown"
 
        if self->CRCerror() gt 0 then self._isok_cause += " - CRC error"
        if self->FLTimeout() gt 0 then self._isok_cause += " - FLTimeout error"
        if self->PendingCounter() gt 0 then self._isok_cause += " - PendingCounter error"
        if self->SkipCounter() gt 0 then self._isok_cause += " - SkipCounter error"
        if self->TimeStamp() gt 0 then self._isok_cause += " - TimeStamp error"
        if self->WFSGlobalTimeout() gt 0 then self._isok_cause += " - WFSGlobalTimeout error"

    endif 
    cause += self._isok_cause
    return, self._isok
end

; 
; Return the fraction of time spent in closed optical loop 
; Should be always 1.0 or 0.0
; -1.0 is returned if closedloop file does not exists 
; 
function AOSanityCheck::ClosedLoop
    if self._closedloop eq -1.0 then begin
	    if not file_test(self._loopclosed_fname) then return, -1.0
        cl = readfits(self._loopclosed_fname, /silent)
        self._closedloop = mean(cl)
    endif
    return, self._closedloop
end

; 
; Return the crc errors 
; Should be always 0.0
; -1.0 is returned if crcerrors file does not exists 
; 
function AOSanityCheck::CRCerror
    if self._crcerror eq -1L then begin
	    if not file_test(self._crcerrors_fname) then return, -1L
        ret = readfits(self._crcerrors_fname, /silent)
        self._crcerror = max(ret)
    endif
    return, self._crcerror
end

; 
; Return the fast link timeout 
; Should be always 0.0
; -1.0 is returned if fltimeout file does not exists 
; 
function AOSanityCheck::FLTimeout
    if self._fltimeout eq -1L then begin
	    if not file_test(self._fltimeout_fname) then return, -1L
        ret = readfits(self._fltimeout_fname, /silent)
        self._fltimeout = max(ret)
    endif
    return, self._fltimeout
end

; 
; Return the pending counter 
; Should be always 0.0
; -1.0 is returned if pending counter file does not exists 
; 
function AOSanityCheck::PendingCounter
    if self._pendingcounter eq -1L then begin
	    if not file_test(self._pendingcounter_fname) then return, -1L
        ret = readfits(self._pendingcounter_fname, /silent)
        self._pendingcounter = max(ret)
    endif
    return, self._pendingcounter
end

; 
; Return the skip counter 
; Should be always 0.0
; -1.0 is returned if skip counter file does not exists 
; 
function AOSanityCheck::SkipCounter
    if self._skipcounter eq -1L then begin
	    if not file_test(self._skipcounter_fname) then return, -1L
        ret = readfits(self._skipcounter_fname, /silent)
        self._skipcounter = max(ret)
    endif
    return, self._skipcounter
end

; 
; Return the timestamp 
; Should be always 0.0
; -1.0 is returned if timestamp file does not exists 
; 
function AOSanityCheck::TimeStamp
    if self._timestamp eq -1L then begin
	    if not file_test(self._timestamp_fname) then return, -1L
        ret = readfits(self._timestamp_fname, /silent)
        self._timestamp = max(ret)
    endif
    return, self._timestamp
end

; 
; Return the WFS Global Timeout 
; Should be always 0.0
; -1.0 is returned if wfsglobaltimeout file does not exists 
; 
function AOSanityCheck::WFSGlobalTimeout
    if self._wfsglobaltimeout eq -1L then begin
	    if not file_test(self._wfsglobaltimeout_fname) then return, -1L
        ret = readfits(self._wfsglobaltimeout_fname, /silent)
        self._wfsglobaltimeout = max(ret)
    endif
    return, self._wfsglobaltimeout
end

pro AOSanityCheck::test
    d=self->ClosedLoop()
    d=self->CRCerror()
    d=self->FLTimeout()
    d=self->PendingCounter()
    d=self->SkipCounter()
    d=self->TimeStamp()
    d=self->WFSGlobalTimeout()
    d=self->IsOK()
end

pro AOSanityCheck::Cleanup
    self->AOhelp::Cleanup
end

pro AOSanityCheck__define
    struct = { AOSanityCheck, $
        _crcerrors_fname            : "", $
        _fltimeout_fname            : "", $
        _loopclosed_fname           : "", $
        _pendingcounter_fname       : "", $
        _skipcounter_fname          : "", $
        _timestamp_fname            : "", $
        _wfsglobaltimeout_fname     : "", $
        _closedloop                 : 0.0, $
        _crcerror                   : 0L, $ 
        _fltimeout                  : 0L, $ 
        _pendingcounter             : 0L, $ 
        _skipcounter                : 0L, $ 
        _timestamp                  : 0L, $ 
        _wfsglobaltimeout           : 0L, $ 
        _isok                       : 0, $ 
        _isok_cause                 : "", $ 
        INHERITS    AOhelp $
    }

end
