
;+
;
;-

function AOtelemetry::Init, root_obj

    if not obj_valid(root_obj) then return, 0
    self._rootobj = root_obj

    tracknum = self._rootobj->tracknum()
    date = strmid(tracknum,0,8)
    self._datadir = filepath(root=ao_datadir(), sub=['adsec_data',date,'telemetry'], '')

    self._from = (self._rootobj->obj_tracknum())->julday()
    self._to   = self._from + self._rootobj->duration()/86400.

    if self._rootobj->recompute() eq 1B then begin
        files = FILE_SEARCH(filepath(root=self._rootobj->elabdir(),'telemetry_*.sav'))
        if files[0] ne '' then for f=0,n_elements(files)-1 do FILE_DELETE, files[f]
    endif

    if not self->AOhelp::Init('AOtelemetry', 'Interface to telemetry data') then return, 0
    self->addMethodHelp, 'names()', 'list of active telemetry streams'
    self->addMethodHelp, "get(stream, [FROM=FROM], [TO=TO])", "struct{ timestamp, values} for the specified stream"

    return ,1
end

function AOtelemetry::names
    files = FILE_SEARCH(filepath(root=self._datadir, '*_t.fits'))
    for f=0,n_elements(files)-1 do begin
        ff = file_basename(files[f])
        files[f] = strmid(ff,0,strlen(ff)-7)
    endfor
    return, files
end

function AOtelemetry::from
    return, self._from
end

function AOtelemetry::to
    return, self._to
end

function AOtelemetry::get, logger, FROM=FROM, TO=TO, AROUND=AROUND

    ; without FROM/TO, defaults to the tracknum data and we can cache it
    if (not keyword_set(FROM)) and (not keyword_set(TO)) and (not keyword_set(AROUND)) then begin
        ; try to restore file from cache
        cachefile = filepath(root=self._rootobj->elabdir(),'telemetry_'+strupcase(logger)+'.sav')
        if file_test(cachefile) then begin
            restore, cachefile
        endif else begin
            data = self->extract( logger, self._from, self._to)
            save, data, file=cachefile
        endelse
        return, data
    endif else begin
        if not keyword_set(FROM) then FROM = self._from
        if not keyword_set(TO) then TO = self._to
        if keyword_set(AROUND) then begin
            FROM -= AROUND
            TO += AROUND
        endif
        return, self->extract( logger, FROM, TO) 
    endelse

end

function AOtelemetry::extract, logger, from, to

    logger = strupcase(logger)
    file_t = filepath(root=self._datadir, logger+'_t.fits')
    file_v = filepath(root=self._datadir, logger+'_v.fits')
    if not file_test(file_t) then return,-1
    if not file_test(file_v) then return,-1
    t = readfits(file_t)
    v = readfits(file_v)
    idx = where((t ge from) and (t le to), count)
    if count gt 0 then begin
        t = t[idx]
        v = v[*,idx]
        return, create_struct('t',t,'v',v)
    endif else begin
        return,-1
    endelse
end


pro AOtelemetry::free
end

pro AOtelemetry::Cleanup
end

pro AOtelemetry__define
    struct = { AOtelemetry            , $
       _rootobj           : obj_new() , $
       _datadir           : ''        , $
       _from              : 0d        , $
       _to                : 0d        , $
       INHERITS AOhelp                  $
    }
end
