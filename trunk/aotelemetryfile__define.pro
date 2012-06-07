
;+
;
;-

function AOtelemetryfile::Init, fname

    self._fname = fname

    if not self->AOhelp::Init('AOtelemetryfile', 'Represents a telemetry file') then return, 0
    self->addMethodHelp, "fname()", "telemetry filename (string)"

    return ,1
end

; Parse a single line from a telemetry log file
;
; line (input) (string) line to parse
; keyword (output) (string) telemetry logger
; timestamp (output) (double) log timestamp
; values (output) (dblarr) log values
;

pro AOtelemetryfile::parseline, line, keyword, timestamp, values

   keyword=''
   if strpos(line, ' > ') lt 0 then return

   parts = strsplit(line, ' |', /EXTRACT)
   idx = where(parts eq '>')
   keyword = (parts[idx-1])[0]
   values = float(parts[ idx+1:*])
   h = (parts[idx-2])[0]
   d = (parts[idx-3])[0]
   timestamp = julday(strmid(d,5,2), strmid(d,8,2), strmid(d,0,4), strmid(h,0,2), strmid(h,3,2), strmid(h,6,2))+double(strmid(h,9))
end

function AOtelemetryfile::fname
   return, self._fname
end

; parse a telemetry file.
; Return a struct where, for each logger 'TAG', three tags are defined
; struct.TAG = 1
; struct._T_TAG = *dblarr(..)  with timestamp values
; struct._V_TAG = *dblarr(..)  with logged values

pro AOtelemetryfile::parse

    s =create_struct('_dummy',1)
    line='dummy'
    openr, unit, self->fname(), /GET_LUN
    repeat begin
        readf, unit, line
        self->parseline, line, k, t, v
        pos = where(tag_names(s) eq strupcase(k))
        if pos eq -1 then begin
            s = create_struct(k,1,'_t_'+k, ptr_new(t), '_v_'+k, ptr_new(v), s)
        endif else begin
            *s.(pos+1) = [*s.(pos+1), t]
            *s.(pos+2) = [[*s.(pos+2)], [v]]
        endelse
    endrep until eof(unit)

    if ptr_valid(self._data) then ptr_free, self._data
    self._data = ptr_new(s)

end

function AOtelemetryfile::export

    names = tag_names(*self._data)
    idx = where(strmid(names,0,1) ne '_')
    for i=0,n_elements(idx)-1 do begin
        name = names[idx[i]]
        t = *((*self._data).(idx[i]+1))
        v = *((*self._data).(idx[i]+2))
        writefits,name+'_t.fits',t 
        writefits,name+'_v.fits',v
    endfor
end

function AOtelemetryfile::names
    if not ptr_valid(self._data) then self->parse
    names = tag_names(*self._data)
    idx = where(strmid(names,0,1) ne '_', count)
    if count gt 0 then return, names[idx]
    return,-1
end

function AOtelemetryfile::times, logger
    name = strupcase(logger)
    if not ptr_valid(self._data) then self->parse
    pos = where(tag_names(*self._data) eq name)
    if pos eq -1 then begin
        message,'No data for logger '+logger, /info
        return,-1
    end
    return, *((*self._data).(pos+1))
end

function AOtelemetryfile::values, logger
    name = strupcase(logger)
    if not ptr_valid(self._data) then self->parse
    pos = where(tag_names(*self._data) eq name)
    if pos eq -1 then begin
        message,'No data for logger '+logger, /info
        return, -1
    endif
    return, *((*self._data).(pos+2))
end

pro AOtelemetryfile::free
end

pro AOtelemetryfile::Cleanup
    if ptr_valid(self._data) then ptr_free, self._data
end

pro AOtelemetryfile__define
    struct = { AOtelemetryfile        , $
       _fname             : ""        , $
       _data              : ptr_new() , $
       INHERITS AOhelp                  $
    }
end
