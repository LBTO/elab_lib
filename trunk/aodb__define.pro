;;;;;;;;;;;;;;;;;
function aodb::Init, SYSTEM_ID, recompute=recompute

	if n_elements(SYSTEM_ID) eq 0 then begin
		message, 'USAGE: db=getdb(SYSTEM_ID) where SYSTEM_ID is: 1 (FLAO1); 2 (FLAO2).',/info
		return,0
	endif

	CASE SYSTEM_ID OF
		1: 	self._db_fname = 'db_flao1.sav'
		2:	self._db_fname = 'db_flao2.sav'
		else: begin
			message, 'Unknown AO system',/info
			return,0
		endelse
	ENDCASE

    if keyword_set(recompute) then begin
        res = dialog_message('Do you really want to rebuild the whole database?', /center, /question)
        if res eq 'Yes' then file_delete, self->db_fname(), /allow_nonexistent
    endif

    if not self->AOhelp::Init('AOdb', 'The AO database') then return, 0
    self->addMethodHelp, "remove, tracknum_list", "Remove from the db the items in the tracknum_list"
    self->addMethodHelp, "insert, tracknum_list", "Insert in the db the items in the tracknum_list"
    self->addMethodHelp, 'alter, tracknum_list, property, value', "Alter items in tracknum_list assigning value to their property"
    self->addMethodHelp, "db_fname()", "Name of the file containing the archive"
    self->addMethodHelp, "save", "Save the working copy of the archive to file"
    self->addMethodHelp, "restore", "Restore the archive from file"
    self->addMethodHelp, "query(property, op, value)", "Query the db. op can be 'gt', 'ge', ... ,'in','like'"
    self->addMethodHelp, "show, tracknums, /property", "Show tracknums data in the archive or properties if keyword is set"
    self->addMethodHelp, "tracknums()", "Return the list of tracknums in the archive"
    self->addMethodHelp, "property_names()", "Return the list of properties in the archive"
    ;self->addMethodHelp, "property_dict()", "NO API.  Return the reference to the dictionary of properties"


    if file_test( self->db_fname() ) then begin
        message, 'restoring db from: '+self->db_fname(), /info
        self->restore
        return,1
    endif else begin
    	message, 'The file '+self->db_fname()+' in '+ao_elabdir()+' does not exist!', /info
    	print, 'Are you sure you want to create a db file from scratch???? (Y/N):'
    	READKEY: key = get_kbrd(0.01)
    	CASE STRLOWCASE(key) OF
			'y': print, 'New db will be created....'
			'n': return, 0
			else:  goto, READKEY
		ENDCASE
	endelse


    cfg = [  $
        {name:'refStar',                            type:'string'}, $
        {name:'ignore',                             type:'integer'}, $
        {name:'tracknum',                           type:'string'}, $
        {name:'mag',                                type:'real'}, $
        {name:'operation_mode',                     type:'string'}, $
        {name:'isok',								type:'integer'}, $
        {name:'errorDescription',					type:'string'}, $
        {name:'wfs_status.wunit',                   type:'integer'}, $
        {name:'wfs_status.modulation',              type:'real'}, $
        {name:'wfs_status.rerotator',               type:'real'}, $
        ;{name:'wfs_status.camera_lens',               type:'real'}, $
        {name:'wfs_status.stagex',                 	type:'real'}, $
        {name:'wfs_status.stagey',                 	type:'real'}, $
        {name:'wfs_status.stagez',                 	type:'real'}, $
        {name:'wfs_status.lamp_intensity',          type:'real'}, $
        {name:'wfs_status.cube_angle',              type:'real'}, $
        {name:'wfs_status.cube_stage',              type:'real'}, $
        {name:'wfs_status.ccd39.framerate',         type:'real'}, $
        {name:'wfs_status.ccd39.binning',           type:'integer'}, $
        {name:'wfs_status.ccd39.readout_speed',     type:'real'}, $
        {name:'wfs_status.filtw1.name',             type:'string'}, $
        {name:'wfs_status.filtw2.name',             type:'string'}, $
        {name:'adsec_status.fsm_state',             type:'string'}, $
        {name:'adsec_status.b0_a_file',             type:'string'}, $
        {name:'adsec_status.b_delay_a_file',        type:'string'}, $
        {name:'adsec_status.a_delay_file',          type:'string'}, $
        {name:'adsec_status.c_file',                type:'string'}, $
        {name:'adsec_status.g_gain_a_file',         type:'string'}, $
        {name:'adsec_status.disturb_file',          type:'string'}, $
        {name:'adsec_status.disturb_status',        type:'integer'}, $
        {name:'adsec_status.shape_file',            type:'string'}, $
        {name:'sanitycheck.isok',                   type:'integer'}, $
        {name:'control.m2c_fname',                  type:'string'}, $
        {name:'control.intmat_fname',               type:'string'}, $
        {name:'frames_counter.decimation',          type:'integer'}, $
        {name:'modal_rec.fname',                    type:'string'}, $
        {name:'modal_rec.nmodes',                   type:'integer'}, $
        {name:'intmat.fname',                       type:'string'}, $
        {name:'intmat.modalamp_fname',              type:'string'}, $
        {name:'intmat.modal_dist_fname',            type:'string'}, $
        {name:'intmat.basis',            			type:'string'}, $
        {name:'intmat.im_type',            			type:'string'}, $
        {name:'frames.nphsub_per_int_av',           type:'real'}, $
        {name:'frames.antidrift_status', 			type:'integer'}, $
        {name:'disturb.fname',                      type:'string'}, $
        {name:'disturb.type',                       type:'string'}, $
        {name:'disturb.dist_freq',                  type:'real'}, $
        {name:'disturb.reflcoef',                   type:'integer'}, $
        {name:'disturb.seeing',                     type:'real'}, $
        {name:'disturb.r0',                         type:'real'}, $
        {name:'disturb.L0',                         type:'real'}, $
        {name:'disturb.vwind',                      type:'real'}, $
        {name:'olmodes.seeing',                     type:'real'}, $
        {name:'tel.rot_angle',                      type:'real'}, $
        {name:'tel.az',                             type:'real'}, $
        {name:'tel.el',                             type:'real'}, $
        {name:'tel.ra',                             type:'real'}, $
        {name:'tel.dec',                            type:'real'}, $
        {name:'tel.wind_speed',                     type:'real'}, $
        {name:'tel.extern_wind_speed',              type:'real'}, $
        {name:'tel.extern_wind_direction',          type:'real'}, $
        {name:'tel.dimm_seeing',                    type:'real'}, $
        {name:'tel.guidecam_fwhm_x',                type:'real'}, $
        {name:'tel.guidecam_fwhm_y',                type:'real'}, $
        {name:'tv.nframes',                         type:'real'}, $
        {name:'tv.frame_w',                         type:'integer'}, $
        {name:'tv.frame_h',                         type:'integer'}, $
        {name:'tv.lambda',                          type:'real'}, $
        {name:'tv.pixelscale',                      type:'real'}, $
        {name:'tv.exptime',                         type:'real'}, $
        {name:'tv.framerate',                       type:'real'}, $
        {name:'tv.binning',                         type:'integer'}, $
        {name:'tv.gaussfit.fwhm',                   type:'real'}, $
        ;{name:'tv.gaussian.center',                 type:'real'}, $
        {name:'tv.gaussfit.cx',						type:'real'}, $
        {name:'tv.gaussfit.cy',						type:'real'}, $
        {name:'tv.sr_se',                           type:'real'}, $
        {name:'irtc.nframes',                       type:'real'}, $
        {name:'irtc.frame_w',                       type:'integer'}, $
        {name:'irtc.frame_h',                       type:'integer'}, $
        {name:'irtc.lambda',                        type:'real'}, $
        {name:'irtc.pixelscale',                    type:'real'}, $
        {name:'irtc.exptime',                       type:'real'}, $
        {name:'irtc.framerate',                     type:'real'}, $
        {name:'irtc.binning',                       type:'integer'}, $
        {name:'irtc.gaussfit.fwhm',                 type:'real'}, $
        ;{name:'irtc.gaussian.center',                 type:'real'}, $
        {name:'irtc.gaussfit.cx',					type:'real'}, $
        {name:'irtc.gaussfit.cy',					type:'real'}, $
        {name:'irtc.sr_se',                         type:'real'},  $
        {name:'control.zerogain',                   type:'integer'},  $
        {name:'control.iskalman',                   type:'integer'},  $
        {name:'control.maxgain',                    type:'real'},  $
        {name:'control.mingain',                    type:'real'},  $
		{name:'frames.ron',							type:'real'},  $
 		{name:'control.ttgain',                     type:'real'},  $
		{name:'control.mogain',                   	type:'real'},  $
		{name:'control.hogain',                   	type:'real'},  $
		{name:'frames.mean_center_separation',		type:'real'}  $
    ]

    ; create db
    self._db_prop_dicts = obj_new('IDL_container')

    for i=0L, n_elements(cfg)-1 do begin
        self._db_prop_dicts->add, obj_new('aodict', cfg[i].type, 'string') ;, /multi)
    endfor

    self._db_prop_names = ptr_new(cfg(*).name)

    return, 1
end

; use with care, you can destroy the entire database
pro aodb::addproperty, property, type
    if (type ne 'real') and (type ne 'integer') and (type ne 'string') then message, 'unsupport type '+type

    self->save, /backup

    self._db_prop_dicts->add, obj_new('aodict', type, 'string')
    db_prop_names = self->property_names()
    db_prop_names = [db_prop_names, property]
    db_prop_dicts = self._db_prop_dicts
    save, db_prop_dicts, db_prop_names, file=self->db_fname()
    message, 'db saved. Exit IDL and restart elab-lib', /info
end

pro aodb::remove, tracknum_list
    for i=0L, n_elements(tracknum_list)-1 do begin
        for j=0L, n_elements(self->property_names()) - 1 do begin
            (self._db_prop_dicts->get(pos=j))->remove, tracknum_list[i]
        endfor
    endfor
end

;pro aodb::insert, tracknums=tracknums, from_tracknum=from_tracknum, to_tracknum=to_tracknum
pro aodb::insert, tracknum_list, property=propertylist
    error = 0
    ; populate db
    ; loop on the elements of the tracknum list
    for i=0L, n_elements(tracknum_list)-1 do begin
        catch, error
        if error eq 0 then begin
            ee = getaoelab(tracknum_list[i])
        endif else begin ; in case of error, simply continue
            catch, /cancel
            print, format='(%"%s NOT added to db. %d:%s ")', tracknum_list[i], error, !error_state.msg
            continue
        endelse
        catch, /cancel
        if not obj_valid(ee) then continue
        if n_elements(propertylist) eq 0 then propertylist = self->property_names()
        ; loop on the property in the db
        for j=0L, n_elements(propertylist) -1 do begin
            func = propertylist[j] ; e.g. 'wfs_status.ccd39.binning'
            catch, error
            if error eq 0 then begin
                ; insert the pair tracknum:value into the dictionary corresponding to the property
                pos = where(self->property_names() eq func, count)
                if  count eq 0 then message, func+': this property is not part of the database. Cannot insert'
                (self._db_prop_dicts->get(pos=pos))->insert, ee->tracknum(), ee->ex(func)  ;i
                print, format='(%" %s: %s added to db")', func, ee->tracknum()
            endif else begin
                catch, /cancel
                ; in case of error, simply continue
                print, format='(%" %s: %s NOT added to db")', func, ee->tracknum()
            endelse
            catch, /cancel
        endfor
        ;ee->free
        obj_destroy, ee
    endfor
end

function aodb::db_fname
    return, filepath(root=ao_elabdir(), self._db_fname)
end

pro aodb::save, backup=backup
    if keyword_set(backup) then begin
        caldat, systime(/julian), m,d,y,hh,mm,ss
        tracknum = string(format='(%"%04d%02d%02d_%02d%02d%02d")', y,m,d,hh,mm,ss)
        fname = self->db_fname()+'.'+tracknum
    endif else begin
        fname = self->db_fname()
    endelse
    db_prop_dicts = self._db_prop_dicts
    db_prop_names = self->property_names()
    save, db_prop_dicts, db_prop_names, file=fname
end

pro aodb::restore
    restore, file=self->db_fname()
    dum = where(routine_info() eq 'AODICT__DEFINE', cnt)
    if cnt eq 0 then resolve_routine,  'AODICT__DEFINE'

    if (ptr_valid(self._db_prop_names))  THEN ptr_free, self._db_prop_names
    self._db_prop_names = ptr_new(db_prop_names, /no_copy)
    obj_destroy, self._db_prop_dicts
    self._db_prop_dicts = db_prop_dicts
end

pro aodb::alter, tracknum_list, property, value
    for i=0L, n_elements(tracknum_list)-1 do begin
        catch, error
        if error eq 0 then begin
            ; insert the pair tracknum:value into the dictionary corresponding to the property
            pos = where(self->property_names() eq property, count)
            if  count eq 0 then message, property+': this property is not part of the database. Cannot insert'
            ;Alter must verify that a valid tracknum exists already in database
            postr =  where(self->property_names() eq 'tracknum')
            merdalorenzo = (self._db_prop_dicts->get(pos=postr))->where('eq', tracknum_list[i], count)
            if count eq 0 then begin
            	message, Tracknum_list[i]+' not in db. NOT altered.', /info
            	continue
            endif
            (self._db_prop_dicts->get(pos=pos))->remove, tracknum_list[i]
            (self._db_prop_dicts->get(pos=pos))->insert, tracknum_list[i], value  ;i
            print, format='(%" %s: %s added to db")', property, tracknum_list[i]
        endif else begin
            ; in case of error, simply continue
            print, format='(%" %s: %s NOT added to db")', property, tracknum_list[i]
        endelse
        catch, /cancel
    end
end

; set is used to query on a given subset (aodataset), that is typically the results of a previous query
; e.g. select tracknum where wfs.ccd39.framerate between 50 and 500Hz
function aodb::query, property, op, value, subset=subset
    pos = where(self->property_names() eq property, count)
    if  count eq 0 then begin
    	message, 'Unknown property', /info
    	return, -1
    endif

    tracks = (self._db_prop_dicts->get(pos=pos))->select(op, value, count_matching)
    if count_matching eq 0 then begin
		message, 'No matching found in whole database',/info
		return, obj_new('aodataset')
	endif

    if n_elements(subset) ne 0 then begin
        count_matching = 0
        if obj_isa(subset, 'aodataset') then subsettn=subset->tracknums() else message, 'subset must be an aodataset'
        for i=0, n_elements(subsettn)-1 do begin
            idx=where(subsettn[i] eq tracks, cnt)
            if cnt gt 0 then begin
                if count_matching eq 0 then res = [subsettn[i]] else res = [res, subsettn[i]]
                count_matching += 1
            endif
        endfor
        if count_matching gt 0 then tracks = res else begin
        	message, 'No matching found in subset', /info
        	return, obj_new('aodataset')
        endelse
    endif

    print, 'select tracknum where '+property+' '+op+' '+strjoin(strtrim(string(value),2), ' ')
    return, obj_new('aodataset', tracks)
end


pro aodb::show, tracknum, property=property ;, from=from, to=to
    if keyword_set(property) then begin
        print, transpose(self->property_names())
        return
    endif
    for j=0L, n_elements(self->property_names()) -1 do begin
        func = ( self->property_names() ) [j] ;'wfs_status.ccd39.binning'
        catch, error
        if error eq 0 then begin
            pos = where(self->property_names() eq func, count)
            if  count eq 0 then message, func+': this property is not part of the database'
            idx = (self._db_prop_dicts->get(pos=pos))->where('eq', tracknum, count) ;i
            if count gt 0 then begin
                value = ((self._db_prop_dicts->get(pos=pos))->values())[idx]
                print, format='(%" %s %s: %s")', tracknum, func, string(value)
            endif
        endif else begin
            print, format='(%" %s %s error")', tracknum, func
        endelse
        catch, /cancel
    endfor
end

function aodb::value, property,  subset=subset
    pos = where(self->property_names() eq property, count)
    if  count eq 0 then begin
    	message, property+': this property is not part of the database',/info
    	return, -1
    endif
    tracknums = n_elements(subset) ne 0 ? subset->tracknums() : self->tracknums()
    nel=0
    for i=0, n_elements(tracknums)-1 do begin
        idx = (self._db_prop_dicts->get(pos=pos))->where('eq', tracknums[i], count) ;i
        if count gt 0 then begin
            if nel eq 0 then value = ((self._db_prop_dicts->get(pos=pos))->values())[idx] else $
            value = [value, ((self._db_prop_dicts->get(pos=pos))->values())[idx] ]
            nel+=1
        endif
    endfor
    return, value
end



pro aodb::plot, X_VAR, Y_VAR, subset=subset, HISTO_VAR=HISTO_VAR, GROUP_VAR=GROUP_VAR $
		, g_leg_title=g_leg_title, h_leg_title=h_leg_title, _EXTRA = ex, CURSOR=CURSOR

	nparams = n_params()
	if nparams ne 2 then begin
		message, 'Usage: db->plot, X, Y, [,...]',/info
		return
	endif

	;X and Y variables
	;- - - - - - - - - - -
	X = self->value(X_VAR, subset=subset)
	if size(X,/type) eq 2 then if X[0] eq -1 then return
	Y = self->value(Y_VAR, subset=subset)
	if size(Y,/type) eq 2 then if Y[0] eq -1 then return
	if n_elements(X) ne n_elements(Y) then begin
		message, 'X and Y not compatible dimensions',/info
		return
	endif

	;Histogram variable
	;- - - - - - - - - - -
	if n_elements(HISTO_VAR) ne 0 then begin
		H = self->value(HISTO_VAR, subset=subset)
		if H[0] eq -1 then return
		if n_elements(H) ne n_elements(X) then begin
			message, '[X,Y] and HISTO_VAR not compatible dimensions',/info
			return
		endif
		if n_elements(h_leg_title) eq 0 then h_leg_title = HISTO_VAR+':'
	endif

	;Group variable
	;- - - - - - - - - - -

	if n_elements(GROUP_VAR) ne 0 then begin
		G = self->value(GROUP_VAR, subset=subset)
		if size(G,/type) eq 2 then if G[0] eq -1 then return
		if n_elements(G) ne n_elements(X) then begin
			message, '[X,Y] and GROUP_VAR not compatible dimensions',/info
			return
		endif
		if n_elements(g_leg_title) eq 0 then g_leg_title = GROUP_VAR+':'
	endif

	if keyword_set(CURSOR) then begin
		tr = self->value('tracknum', subset=subset)
	endif

	aoplot, X, Y, HISTO_VAR=H, GROUP_VAR=G, _EXTRA = ex, CURSOR=CURSOR, tr=tr $
		  , g_leg_title=g_leg_title, h_leg_title=h_leg_title

end

function aodb::tracknums
    if self->count() eq 0 then message, 'db is empty'
    pos = where(self->property_names() eq 'tracknum', count)
    tns = (self._db_prop_dicts->get(pos=pos))->values()
    return, tns
end

function aodb::count
    pos = where(self->property_names() eq 'tracknum', count)
    return, (self._db_prop_dicts->get(pos=pos))->count()
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
		 _db_fname				: ""		, $
        INHERITS AOhelp $
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

