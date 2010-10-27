
;+
;
; KEYWORD
;    lastminute   analyze tracknums acquired in the last lastminute minutes
;
;-

function AOdataset::Init, objarray=objarray, from=from_tracknum, to=to_tracknum, root_dir=root_dir, lastminute=lastminute, _extra=ex
    if not self->IDL_container::Init() then return, 0
    if not keyword_set(root_dir)      then root_dir = !ao_env.root
    self._root_dir = root_dir

	if n_elements(objarray) eq 0 then begin
    	if not keyword_set(from_tracknum) then from_tracknum = ""
    	if not keyword_set(to_tracknum)   then to_tracknum = ""
        if keyword_set(lastminute) then begin
            now =  systime(/Julian)
            caldat, now, m,d,y,hh,mm,ss
            to_tracknum = string(format='(%"%04d%02d%02d_%02d%02d%02d")', y,m,d,hh,mm,ss)
            caldat, now - (lastminute/1440.d),  m,d,y,hh,mm,ss
            from_tracknum = string(format='(%"%04d%02d%02d_%02d%02d%02d")', y,m,d,hh,mm,ss)
        endif
    	self._from_tracknum = obj_new('AOtracknum', from_tracknum)
    	self._to_tracknum   = obj_new('AOtracknum', to_tracknum)

    	if (not obj_valid(self._from_tracknum)) or (not obj_valid(self._to_tracknum)) then return, 0

    	tracknums = file_basename(file_search(ao_datadir()+path_sep()+'adsec_data'+path_sep()+'*'+path_sep()+'Data_*', /TEST_DIRECTORY))
    	for i=0L, n_elements(tracknums)-1 do tracknums[i]  = strmid(tracknums[i], 5)

    	from_julday = self._from_tracknum->julday()
	    to_julday   = self._to_tracknum->julday()

    	for i=0L, n_elements(tracknums)-1 do begin
        	o_track = obj_new('AOtracknum', tracknums[i])
        	if (o_track->julday() ge from_julday) and (o_track->julday() le to_julday) then begin
        		obj = getaoelab(tracknums[i], _extra=ex)
	;            obj =  obj_new('AOelab', tracknums[i], _extra=ex)
    	        if obj_valid(obj) then begin
        	        obj->free
            	    self->add, obj
            	endif
        	endif
        	obj_destroy, o_track
    	end
	endif else begin
		for ii=0, n_elements(objarray)-1 do $
			if obj_isa(objarray[ii], 'AOelab') then self->add, objarray[ii]
	endelse

    nelem = self->Count()
    if nelem ne 0 then begin
        self._tracknums = ptr_new(strarr(nelem))
        objref = self->Get(/all)
        for i=0L, self->Count()-1 do (*(self._tracknums))[i] = objref[i]->tracknum()
    endif

    return, 1
end

function AOdataset::tracknums
    if ptr_valid(self._tracknums) then return, *(self._tracknums) else return, ""
end

function AOdataset::from_tracknum
    if obj_valid(self._from_tracknum) then return, self._from_tracknum else return, obj_new()
end

function AOdataset::to_tracknum
    if obj_valid(self._to_tracknum) then return, self._to_tracknum else return, obj_new()
end

;
; add dataset2 to this one
; dataset2 is emptied
;
pro AOdataset::merge, dataset2
    if not obj_isa(dataset2, 'AOdataset') then message, 'argument is not a valid AOdataset object', BLOCK='elab', name='ao_oaa_dataset'
    nelem2 = dataset2->Count()
    elem2  = dataset2->Get(/all)
    for i=0L,nelem2-1 do begin
        elem2tracknum = elem2[i]->tracknum()
        ; skip duplicated elements
        if obj_valid(self->GetFromTracknum(elem2tracknum)) then continue
        ; tracknum not present in this dataset: add it
        self->add, elem2[i]
        self._tracknums = ptr_new( [ self->tracknums(), elem2tracknum], /no_copy )
        ; now remove elem2 from dataset2. Otherwise if in the future dataset2 is destroyed
        ; the referenced objects (that are now referenced also in this dataset) are destroyed
        dataset2->remove, elem2[i]
    endfor
end

;+
; Get subset of a dataset.
;-
;function AOdataset::getset,

;
; tracknum : string or string array
;
function AOdataset::GetFromTracknum, tracknum, idx=idx
    ;if not ptr_valid(self._tracknums) then return, obj_new()

    if n_elements(tracknum) eq 1 then begin
        tidx = where(self->tracknums() eq tracknum, count)
        if count ne 0 then idx=tidx
    endif else begin
        for i=0L, n_elements(tracknum)-1 do begin
            tidx = where(self->tracknums() eq tracknum[i], count)
            if count ne 0 then idx = n_elements(idx) eq 0 ? tidx : [idx, tidx]
        endfor
    endelse
    if n_elements(idx) gt 0 then begin
        return, self->Get(pos=idx)
    endif else begin
        message, 'tracknum not found in dataset', /informational, /continue, BLOCK='elab', name='ao_oaa_dataset'
        return, obj_new()
    endelse
end

pro AOdataset::RemoveFromTracknum, tracknum
    self->Remove, self->GetFromTracknum( tracknum )
end

;
; report the measures that are NOT ok, and why
;
pro AOdataset::Wrong
    idx = self->where('isok', 'eq', 0)
    for i=0L,n_elements(idx)-1 do begin
        r=(self->get(pos=idx[i]))->isok(cause=cause)
        tn=(self->get(pos=idx[i]))->tracknum()
        print, tn, cause
    endfor
end


;
; free memory
;
pro AOdataset::free
    objref = self->Get(/all)
    for i=0L, self->Count()-1 do begin
        objref[i]->free
    endfor
end

;
;
;
function AOdataset::value, cmd, index, index_out=index_out, VERBOSE=VERBOSE
	apex = string(39B)
  	nparams = n_params()

    if nparams eq 2 then objref = self->Get(pos=index) else begin
    	objref = self->Get(/all)
    	index = lindgen(self->count())
    endelse
	nel = n_elements(objref)
    isvalid = bytarr(nel)

    cmds = strsplit(cmd, '.', /extr)

	for i=0L, nel-1 do begin
		tmpobj=objref[i]
		if keyword_set(verbose) then print, 'Inspecting :'+tmpobj->tracknum()
      	for j=0L, n_elements(cmds)-2 do begin
            method_name = (strsplit(cmds[j], '(', /extr))[0]
            add_brackets = strpos(cmds[j], '(') eq -1 ? '()' : ''
            r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+method_name+apex+')')
            r=execute('if (hasmethod) then tmpobj= tmpobj->'+cmds[j]+add_brackets)
            if not obj_valid(tmpobj) then break ;
        endfor
        if j eq n_elements(cmds)-1 then begin
            method_name = (strsplit(cmds[j], '(', /extr))[0]
            add_brackets = strpos(cmds[j], '(') eq -1 ? '()' : ''
            r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+method_name+apex+')')
            r=execute('if (hasmethod) then begin & value = tmpobj->'+cmds[j]+add_brackets+'  & isvalid[i]=1 & endif')
            if test_type(value, /obj_ref) eq 0 then if obj_valid(value) eq 0 then isvalid[i]=0
        endif
        if isvalid[i] eq 1 then begin
            if  n_elements(v) eq 0 then v = ptrarr(nel) ; first time value is valid
            v[i]=ptr_new(value, /no_copy)
            if keyword_set(verbose) then print, 'data found'
        endif else if keyword_set(verbose) then print, 'data NOT found'
        if obj_valid(objref[i]) then $
        	if obj_hasmethod(objref[i], 'free') then objref[i]->free
	endfor
    valid = where(isvalid eq 1, cntvalid)

	; If data has different characteristics (dimensions, type, etc) return a pointer.
	; Otherwise, return a data array.
	if cntvalid ne 0 then begin

    	if arg_present(index_out) then index_out = index[where(isvalid)]

		;check type
		type_all = lonarr(cntvalid)
		for jj=0L, cntvalid-1 do type_all[jj] = size( *v[valid[jj]], /type)
		if max(type_all)-min(type_all) ne 0 then return, v
		type = type_all[0]

		;check number of dimensions
		ndim_all = lonarr(cntvalid)
		for jj=0L, cntvalid-1 do ndim_all[jj] = size( *v[valid[jj]], /n_dim)
		if max(ndim_all)-min(ndim_all) ne 0 then return, v
		n_dim = ndim_all[0]

		;check number of elements in each dimension
		if n_dim ne 0 then dim_all = make_array(cntvalid, n_dim, /LONG) else $
						   dim_all = make_array(cntvalid, /LONG)
		for jj=0L, cntvalid-1 do dim_all[jj,*] = size( *v[valid[jj]], /dim)
		if n_dim eq 0 then zero = 0 else zero = lonarr(n_dim)
		if total( max(dim_all, dim=1)-min(dim_all, dim=1) ne zero ) then return, v
		dim = reform(dim_all[0,*])

		;create the data array
		n_ele = size( *v[valid[0]], /n_elements)
		if n_dim eq 0 then data = make_array(dim=cntvalid, type=type) else $
						   data = make_array(dim=[dim,cntvalid], type=type)

    	for i=0L, cntvalid-1 do data[n_ele*i] = reform(*v[valid[i]],n_ele)
    	ptr_free, v
		return, data

	endif else begin
		if arg_present(index_out) then index_out = [-1]
		return, -1
	endelse

end
;
; operand can be one of
;
; KEYWORDS
;   ptrdata    (output) array of pointers to values matching the condition
;
; return index of dataset items that matches the condition operand+reference_value
;
;
function AOdataset::where, cmd, operand, reference_value, ptrdata=ptrdata, AOdata_subset=AOdata_subset
    apex = string(39B)

    objref = self->Get(/all)
    nel = self->Count()
    isvalid = bytarr(nel)

    cmds = strsplit(cmd, '.', /extr)

    for i=0L, nel-1 do begin
        ; tmpobj = (*self._array)[i]
        ; for j=0, n_elements(cmds)-2 do begin
        ;   hasmethod = obj_hasmethod(tmpobj, '$cmds[j]$')
        ;   if (hasmethod) then tmpobj= tmpobj->$cmds[j]$()
        ;   if not obj_valid(tmpobj) then break;
        ; endfor
        ; hasmethod = obj_hasmethod(tmpobj, '$cmds[j]$')
        ; if (hasmethod) then value = tmpobj->$cmds[j]$()
        ;
        tmpobj = objref[i]      ; (*self._array)[i]
        for j=0L, n_elements(cmds)-2 do begin
            method_name = (strsplit(cmds[j], '(', /extr))[0]
            add_brackets = strpos(cmds[j], '(') eq -1 ? '()' : ''
            r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+method_name+apex+')')
            r=execute('if (hasmethod) then tmpobj= tmpobj->'+cmds[j]+add_brackets)
            if not obj_valid(tmpobj) then break ;
        endfor
        if j eq n_elements(cmds)-1 then begin
            method_name = (strsplit(cmds[j], '(', /extr))[0]
            add_brackets = strpos(cmds[j], '(') eq -1 ? '()' : ''
            r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+method_name+apex+')')
            r=execute('if (hasmethod) then begin & value = tmpobj->'+cmds[j]+add_brackets+'  & isvalid[i]=1 & endif')
            if test_type(value, /obj_ref) eq 0 then if obj_valid(value) eq 0 then isvalid[i]=0
        endif

        ; if it is invalid (maybe method or object not-existing) skip to next record
        if isvalid[i] eq 0 then continue

        ; check operand and value
        if n_elements(operand) ne 0 and n_elements(reference_value) ne 0 then begin
            case operand of
                'lt': if not (value lt reference_value) then isvalid[i]=0
                'gt': if not (value gt reference_value) then isvalid[i]=0
                'eq': if not (value eq reference_value) then isvalid[i]=0
                'ne': if not (value ne reference_value) then isvalid[i]=0
                'between': if not ( (value ge reference_value[0]) and (value le reference_value[1]) ) then isvalid[i]=0
                'outside': if not ( (value lt reference_value[0]) or (value gt reference_value[1]) ) then isvalid[i]=0
                'contains': begin
                    if test_type(value, /string) eq 0 then begin
                        if strpos(value, reference_value) eq -1 then isvalid[i]=0
                    endif else begin
                        isvalid[i]=0
                    endelse
                    end
            endcase
        endif

        ; if elements match the condition
        if arg_present(ptrdata) then $
         if isvalid[i] eq 1 then begin
            if  n_elements(v) eq 0 then v = ptrarr(nel) ; first time value is valid
            v[i]=ptr_new(value, /no_copy)
         endif
        if obj_valid(objref[i]) then $
        	if obj_hasmethod(objref[i], 'free') then objref[i]->free
    endfor
    valid = where(isvalid eq 1, cntvalid)
    if arg_present(ptrdata) then ptrdata = cntvalid eq 0 ? ptr_new() : v[valid]

	if arg_present(AOdata_subset) then $
		AOdata_subset = obj_new('aodataset', objarray=self->Get(pos=valid))

    return, valid
end

pro AOdataset::Cleanup
    heap_free, self._tracknums
    heap_free, self._from_tracknum
    heap_free, self._to_tracknum
    self->IDL_CONTAINER::Cleanup
end


pro AOdataset__define
    struct = { AOdataset, $
        _root_dir           : "", $
        _from_tracknum      : obj_new(), $
        _to_tracknum        : obj_new(), $
        _tracknums          : ptr_new(), $
        inherits  IDL_CONTAINER $
    }
end

