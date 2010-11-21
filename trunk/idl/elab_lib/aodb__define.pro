;;;;;;;;;;;;;;;;;
function aodb::Init, recompute=recompute


    if keyword_set(recompute) then begin
        res = dialog_message('Do you really want to rebuild the whole database?', /center, /question)
        if res eq 'Yes' then file_delete, self->db_fname(), /allow_nonexistent
    endif

    if file_test( self->db_fname() ) then begin
        print, 'restoring db from: '+self->db_fname()
        self->restore
        return,1
    endif
    
    cfg = [  $
        {name:'tracknum',                           type:'string'}, $
        {name:'wfs_status.ccd39.binning',           type:'integer'}, $
        {name:'wfs_status.ccd39.framerate',         type:'real'}, $
        {name:'wfs_status.modulation',              type:'real'}, $
        {name:'wfs_status.filtw1.name',             type:'string'}, $
        {name:'wfs_status.filtw2.name',             type:'string'}, $
        {name:'frames.nphsub_per_int_av',           type:'real'}, $
        {name:'modal_rec.nmodes',                   type:'integer'}, $
        {name:'modal_rec.fname',                    type:'string'}, $
        ;; aggiunger gain min e gain max
        {name:'mag',                                type:'real'}, $
        ; tel.el aggiustare unita misura ;{name:'tel.el',                             type:'real'}, $
        {name:'tel.wind_speed',                     type:'real'}, $
        {name:'irtc.lambda',                        type:'real'}, $
        {name:'irtc.exptime',                       type:'real'}, $
        {name:'irtc.framerate',                     type:'real'}, $
        {name:'irtc.nframes',                       type:'real'}, $
        {name:'irtc.sr_se',                         type:'real'}  $
    ]

    
    
    ; create db
    self._db_prop_dicts = obj_new('IDL_container')

    for i=0L, n_elements(cfg)-1 do begin
        self._db_prop_dicts->add, obj_new('aodict', cfg[i].type, 'string') ;, /multi)             
    endfor

    self._db_prop_names = ptr_new(cfg(*).name)

    return, 1
end

pro aodb::remove, tracknum_list
    for i=0L, n_elements(tracknum_list)-1 do begin
        for j=0L, n_elements(self->property_names()) - 1 do begin
            (self._db_prop_dicts->get(pos=j))->remove, tracknum_list[i]
        endfor
    endfor
end

pro aodb::insert, tracknums=tracknums, from_tracknum=from_tracknum, to_tracknum=to_tracknum
    ; either it is specified a tracknums list...
    if keyword_set(tracknums) then begin 
        tracknum_list=tracknums
    ; ... or a time interval from/to
    endif else begin
        if not keyword_set(from_tracknum) then from_tracknum = ""
        if not keyword_set(to_tracknum)   then to_tracknum = ""
        obj_from_tracknum = obj_new('AOtracknum', from_tracknum)
        obj_to_tracknum   = obj_new('AOtracknum', to_tracknum)
        if (not obj_valid(obj_from_tracknum)) or (not obj_valid(obj_to_tracknum)) then begin
            message, 'Tracknum error: '+from_tracknum+' / '+to_tracknum
        endif
        from_julday = obj_from_tracknum->julday()
        to_julday   = obj_to_tracknum->julday()
        obj_destroy, obj_from_tracknum
        obj_destroy, obj_to_tracknum

        tracknums = file_basename(file_search(ao_datadir()+path_sep()+'adsec_data'+path_sep()+'*'+path_sep()+'Data_*', /TEST_DIRECTORY))
        for i=0L, n_elements(tracknums)-1 do tracknums[i]  = strmid(tracknums[i], 5)

        for i=0L, n_elements(tracknums)-1 do begin
            o_track = obj_new('AOtracknum', tracknums[i])
            inRange = (o_track->julday() ge from_julday) and (o_track->julday() le to_julday)
            obj_destroy, o_track
            if (inRange) then self->insert, TRACKNUMS=tracknums[i]
        end
        return
    endelse
    
    error = 0
    ; populate db
    ; loop on the elements of the tracknum list
    for i=0L, n_elements(tracknum_list)-1 do begin
        catch, error
        if error eq 0 then begin
            ee = getaoelab(tracknum_list[i])
        endif else begin ; in case of error, simply continue
            print, format='(%"%s NOT added to db. %d:%s ")', tracknum_list[i], error, !error_state.msg
            catch, /cancel
            continue
        endelse
        catch, /cancel
        ; loop on the property in the db
        for j=0L, n_elements(self->property_names()) -1 do begin
            func = ( self->property_names() ) [j] ; e.g. 'wfs_status.ccd39.binning'
            catch, error
            if error eq 0 then begin
                ; insert the pair tracknum:value into the dictionary corresponding to the property
                pos = where(self->property_names() eq func, count)
                if  count eq 0 then message, func+': this property is not part of the database. Cannot insert'
                (self._db_prop_dicts->get(pos=pos))->insert, ee->tracknum(), ee->ex(func)  ;i 
                print, format='(%" %s: %s added to db")', func, ee->tracknum()
            endif else begin
                ; in case of error, simply continue
                print, format='(%" %s: %s NOT added to db")', func, ee->tracknum()
            endelse
            catch, /cancel
        endfor
        obj_destroy, ee
    endfor
end

function aodb::db_fname
    return, filepath(root=ao_elabdir(), 'db.sav')
end

pro aodb::save
    db_prop_dicts = self._db_prop_dicts
    db_prop_names = self->property_names()
    save, db_prop_dicts, db_prop_names, file=self->db_fname()
end

pro aodb::restore
    restore, file=self->db_fname()

    if (ptr_valid(self._db_prop_names))  THEN ptr_free, self._db_prop_names
    self._db_prop_names = ptr_new(db_prop_names, /no_copy)
    obj_destroy, self._db_prop_dicts
    self._db_prop_dicts = db_prop_dicts
end


; e.g. select tracknum where wfs.ccd39.framerate between 50 and 500Hz
function aodb::query, property, op, value
    pos = where(self->property_names() eq property, count)
    if  count eq 0 then message, 'unknown property to select on'
    tracks = (self._db_prop_dicts->get(pos=pos))->select(op, value, count_matching)
    if count_matching gt 0 then begin
        print, 'select tracknum where '+property+' '+op+' '+strjoin(strtrim(string(value),2), ' ') 
        print, tracks
    endif
    return, tracks
end

pro aodb::show, tracknums ;, from=from, to=to
        for j=0L, n_elements(self->property_names()) -1 do begin
            func = ( self->property_names() ) [j] ;'wfs_status.ccd39.binning'
            catch, error
            if error eq 0 then begin
                pos = where(self->property_names() eq func, count)
                if  count eq 0 then message, func+': this property is not part of the database. Cannot insert'
                idx = (self._db_prop_dicts->get(pos=pos))->where('eq', tracknums, count) ;i 
                if count gt 0 then begin
                    value = ((self._db_prop_dicts->get(pos=pos))->values())[idx]  
                    print, format='(%" %s %s: %s")', tracknums, func, string(value)
                endif
            endif else begin
                print, format='(%" %s %s error")', tracknums, func
            endelse
            catch, /cancel
        endfor
end

function aodb::tracknums
    pos = where(self->property_names() eq 'tracknum', count)
    tns = (self._db_prop_dicts->get(pos=pos))->values()     
    return, tns
end

function aodb::property_names
    return, *(self._db_prop_names)
end

function aodb::property_dict
    return, self._db_prop_dicts
end

pro aodb::Cleanup
    obj_destroy, self._db_prop_dicts
    ptr_free,    self._db_prop_names
;    obj_destroy, self._dict_binning
;    obj_destroy, self._dict_framerate
;    obj_destroy, self._dict_modulation
;    obj_destroy, self._dict_irtc_sr_se
;    obj_destroy, self._dict_irtc_exptime
;    obj_destroy, self._dict_irtc_framerate
end

pro aodb__define
    struct = { aodb, $
        _db_prop_dicts          : obj_new(), $
        _db_prop_names          : ptr_new(), $
;        _dict_binning           : obj_new() ,$
;        _dict_framerate         : obj_new() ,$
;        _dict_modulation        : obj_new() ,$
;        _dict_irtc_sr_se        : obj_new() ,$
;        _dict_irtc_exptime      : obj_new() ,$
;        _dict_irtc_framerate    : obj_new() ,$
        _dummy              : 0B  $
    }
end


;;;;;;;;;;;;;;;;;;;;;;
pro test_aodb
    db = obj_new('aodb')    

    tracknums=['20100622_083355', '20100621_040247', '20100621_052151', '20100621_035214', '20100604_034224']
    db->insert, tracknums
    print, db->property_names()
    tracks = db->query('wfs_status.ccd39.framerate', 'in', [10.,500])
    db->save
    stop
    db->remove, '20100621_035214'
    db->remove, '20100621_040247'
    print, db->tracknums()
    db->insert, '20100621_040247'
    db->insert, '20100621_035214'
    stop
    obj_destroy, db
    
    
;    print, dict_binning->select('eq',1)
;    print, dict_binning->keys()
;    print, dict_binning->values()
end

