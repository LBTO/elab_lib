
;+
; test class for aotime_series
;-
function AOtest_time_series::Init, time_series, dt, _extra=e
    
    self._time_series = ptr_new(time_series)
    self._store_psd_fname = getenv('IDL_TMPDIR')+'merda_psd.sav'
    self._store_peaks_fname = getenv('IDL_TMPDIR')+'merda_peaks.sav'
    if not self->AOtime_series::Init(dt, _extra=e) then return,0
   	self->forceCompute
    return,1
end

; to be implemented in AOtime_series subclassess
function AOtest_time_series::GetDati
    return, self._time_series
end

pro AOtest_time_series::Cleanup
    ptr_free, self._time_series
    self->AOtime_series::Cleanup
end

pro AOtest_time_series__define
    struct = { AOtest_time_series, $
        _time_series      :  ptr_new(), $
        INHERITS    AOtime_series $
    }

end

