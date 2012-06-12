
;+
;
;-

function AOtelemetryfile::Init, fname, recompute=recompute

    self._fname = fname
    if strmid(fname,strlen(fname)-3) eq '.gz' then begin
        myfname = strmid(fname,0,strlen(fname)-3)
        self._gzip = 1
    endif else begin
        myfname = fname
        self._gzip =0
    endelse

    parts = strsplit(fname,'.',/EXTRACT)
    if n_elements(parts) ge 2 then begin
        s = parts[ n_elements(parts)-2-self._gzip]
        if strmid(s,0,1) eq '0' then begin
            caldat,epoch2julian(s), m, d, y
            self._date = string(format='(%"%04d%02d%02d")', y,m,d)
            datadir = filepath(root=ao_elabdir(), sub=[self._date],'')
            if file_test(datadir, /dir) eq 0 then begin
                file_mkdir, datadir
                file_chmod, datadir, /a_read, /a_write
            endif
            datadir = filepath(root=ao_elabdir(), sub=[self._date, 'telemetry'],'')
            if file_test(datadir, /dir) eq 0 then begin
                file_mkdir, datadir
                file_chmod, datadir, /a_read, /a_write
            endif
            self._data_fname = filepath(root=ao_elabdir(), sub=[self._date, 'telemetry'], file_basename(myfname)+'.sav')

            if keyword_set(recompute) then file_delete, self._data_fname, /allow_nonexistent
        endif
    endif

    if not self->AOhelp::Init('AOtelemetryfile', 'Represents a telemetry file') then return, 0
    self->addMethodHelp, "fname()", "telemetry filename (string)"
    self->addMethodHelp, "names()", "returns a strarr with the active logger names"
    self->addMethodHelp, "times(logger)", "returns an array of timestamps (julian date) for the specified logger name"
    self->addMethodHelp, "values(logger)", "returns an array of values for the specified logger name"

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
   hour = (parts[idx-2])[0]
   day = (parts[idx-3])[0]
   m = strmid(day,5,2)
   d = strmid(day,8,2)
   y = strmid(day,0,4)
   h = strmid(hour,0,2)
   mn = strmid(hour,3,2)
   s = strmid(hour,6,2)
   us = strmid(hour,9)
   timestamp = julday(m, d, y, h ,mn, s)+double(us/1e6)
end

function AOtelemetryfile::fname
   return, self._fname
end

function AOtelemetryfile::data_fname
   return, self._data_fname
end

; parse a telemetry file.
; Return a struct where, for each logger 'TAG', three tags are defined
; struct.TAG = 1
; struct._T_TAG = *dblarr(..)  with timestamp values
; struct._V_TAG = *dblarr(..)  with logged values

pro AOtelemetryfile::parse

    if file_test(self._data_fname) then begin
        restore, self._data_fname
    endif else begin
        s =create_struct('_dummy',1)
        line='dummy'   ; force string type
        openr, unit, self->fname(), /GET_LUN, COMPRESS = self._gzip
        while not eof(unit) do begin
            readf, unit, line
            self->parseline, line, k, t, v
            if k eq '' then continue
            pos = where(tag_names(s) eq strupcase(k))
            if pos eq -1 then begin
                s = create_struct(k,1,'_t_'+k, ptr_new(t), '_v_'+k, ptr_new(v), s)
            endif else begin
                *s.(pos+1) = [*s.(pos+1), t]
                *s.(pos+2) = [[*s.(pos+2)], [v]]
            endelse
        endwhile
        close,unit
        free_lun, unit

        ; Sort everything
        idx = where(strmid(tag_names(s),0,1) ne '_', count)
        if count gt 0 then begin
            for i=0,n_elements(idx)-1 do begin
                t = *(s.(idx[i]+1))
                srt = sort(t)
                *s.(idx[i]+1) = (*s.(idx[i]+1))[srt]
                *s.(idx[i]+2) = (*s.(idx[i]+2))[*,srt]
            endfor
        endif

        ; Might not have a savefile if reading live data
        if self._data_fname ne '' then save, s, file=self._data_fname
    endelse

    self->free_data
    self._data = ptr_new(s)

end


; Archive file into ao_datadir! 
pro AOtelemetryfile::archive

    if self->num_streams() lt 1 then return

    names = self->names()
    mmm = [1e9,-1e9]
    for n=0,n_elements(names)-1 do begin
        name = names[n]
        t = self->times(name)
        v = self->values(name)

        today = t[0]
        repeat begin
            caldat, today,  m,d,y
            date = string(format='(%"%04d%02d%02d")', y,m,d)
            datadir = filepath(root=ao_datadir(), sub=['adsec_data',date], '')
            if file_test(datadir, /dir) eq 0 then begin
                file_mkdir, datadir
                file_chmod, datadir, /a_read, /a_write
            endif
            datadir = filepath(root=ao_datadir(), sub=['adsec_data',date,'telemetry'], '')
            if file_test(datadir, /dir) eq 0 then begin
                file_mkdir, datadir
                file_chmod, datadir, /a_read, /a_write
            endif
            idx = where(long(t) eq long(today),count)
            if count gt 0 then begin
                fname_t = filepath(root=datadir, name+'_t.fits')
                fname_v = filepath(root=datadir, name+'_v.fits')
                t_new = t[idx]
                v_new = v[*,idx]

                ; Merge data removing duplicate elements
                if file_test(fname_t) then begin
                    t_old = readfits(fname_t, /SILENT)
                    v_old = readfits(fname_v, /SILENT)
                    t_new = [temporary(t_new),t_old]
                    v_new = [[temporary(v_new)],[v_old]]
                    s = sort(t_new)
                    t_new = (temporary(t_new))[s]
                    v_new = (temporary(v_new))[*,s]
                    u = uniq(t_new)
                    t_new = (temporary(t_new))[u]
                    v_new = (temporary(v_new))[*,u]
                endif
                writefits, fname_t, t_new
                writefits, fname_v, v_new
            endif
            today += 1
        endrep until today gt max(t)
    endfor
end

function AOtelemetryfile::num_streams
    if not ptr_valid(self._data) then self->parse
    names = tag_names(*self._data)
    idx = where(strmid(names,0,1) ne '_', count)
    return,count
end

function AOtelemetryfile::has_name, logger
    name = strupcase(logger)
    if not ptr_valid(self._data) then self->parse
    pos = where(tag_names(*self._data) eq name)
    if pos ge 0 then return, 1 else return,0
end

function AOtelemetryfile::names
    if not ptr_valid(self._data) then self->parse
    names = tag_names(*self._data)
    idx = where(strmid(names,0,1) ne '_', count)
    if count gt 0 then return, names[idx]
    return,-1
end

function AOtelemetryfile::times, logger
    if n_elements(logger) eq 0 then begin
        message,'Must specify a logger', /info
        return,-1
    endif
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
    if n_elements(logger) eq 0 then begin
        message,'Must specify a logger', /info
        return,-1
    endif
    name = strupcase(logger)
    if not ptr_valid(self._data) then self->parse
    pos = where(tag_names(*self._data) eq name)
    if pos eq -1 then begin
        message,'No data for logger '+logger, /info
        return, -1
    endif
    return, *((*self._data).(pos+2))
end

function AOtelemetryfile::date
    return, self._date
end

pro AOtelemetryfile::free
end

pro AOtelemetryfile::Cleanup
    self->free_data
end

pro AOtelemetryfile::free_data
    if ptr_valid(self._data) then begin
        idx = where(strmid(self->names(),0,1) ne '_')
        for i=0,n_elements(idx)-1 do begin
            if n_tags((*self._data)) gt (idx[i]+1) then if ptr_valid( (*self._data).(idx[i]+1)) then ptr_free, (*self._data).(idx[i]+1)
            if n_tags((*self._data)) gt (idx[i]+2) then if ptr_valid( (*self._data).(idx[i]+2)) then ptr_free, (*self._data).(idx[i]+2)
        endfor
        ptr_free, self._data
    endif
end

pro AOtelemetryfile__define
    struct = { AOtelemetryfile        , $
       _fname             : ''        , $
       _gzip              : 0B        , $
       _data              : ptr_new() , $
       _data_fname        : ''        , $
       _date              : ''        , $
       INHERITS AOhelp                  $
    }
end

pro archive_all

   files = file_search('/local/aolog/current/*.tel.gz')
   for f=0,n_elements(files)-1 do begin
      print,files[f]
      a= obj_new('aotelemetryfile', files[f])
      if a->data_fname() ne '' then a->archive
      obj_destroy,a
   endfor

     

end
