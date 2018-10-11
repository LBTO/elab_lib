
;+
;
; KEYWORD
;    lastminute   analyze tracknums acquired in the last lastminute minutes
;    check        check that tracknums contain valid data and reject improper saved data
;
;-

function AOdataset::Init, tracknumlist, from=from_tracknum, to=to_tracknum, lastminute=lastminute, recompute = recompute, check=check
    ;if not self->AOlist::Init() then return, 0
    self._nelems = 0

	if n_elements(tracknumlist) eq 0 then begin
	    if (keyword_set(from_tracknum) eq 0) and (keyword_set(to_tracknum) eq 0) then return, 1
    	if not keyword_set(from_tracknum) then from_tracknum = ""
    	if not keyword_set(to_tracknum)   then to_tracknum = ""
        if keyword_set(lastminute) then begin
            now =  systime(/Julian)
            caldat, now, m,d,y,hh,mm,ss
            to_tracknum = string(format='(%"%04d%02d%02d_%02d%02d%02d")', y,m,d,hh,mm,ss)
            caldat, now - (lastminute/1440.d),  m,d,y,hh,mm,ss
            from_tracknum = string(format='(%"%04d%02d%02d_%02d%02d%02d")', y,m,d,hh,mm,ss)
        endif
        if test_type(from_tracknum, /real) eq 0 then begin ; allow julian dates to be passed as 'from' argument
            caldat, from_tracknum,  m,d,y,hh,mm,ss
            from_tracknum = string(format='(%"%04d%02d%02d_%02d%02d%02d")', y,m,d,hh,mm,ss)
        endif
        if test_type(to_tracknum, /real) eq 0 then begin ; allow julian dates to be passed as 'to' argument
            caldat, to_tracknum,  m,d,y,hh,mm,ss
            to_tracknum = string(format='(%"%04d%02d%02d_%02d%02d%02d")', y,m,d,hh,mm,ss)
        endif

    	objfrom_tracknum = obj_new('AOtracknum', from_tracknum)
    	objto_tracknum   = obj_new('AOtracknum', to_tracknum)
    	if (not obj_valid(objfrom_tracknum)) or (not obj_valid(objto_tracknum)) then return, 0
    	from_julday = objfrom_tracknum->julday()
	    to_julday   = objto_tracknum->julday()
	    obj_destroy, objfrom_tracknum
	    obj_destroy, objto_tracknum
		;Check that from_julday is before to_julday
		if from_julday gt to_julday then begin
			message, 'The "to" tracknum date is earlier than the "from" tracknum date!', /info
			return,0
		endif
        date0 = strmid(from_tracknum,0,8)
        date1 = strmid(to_tracknum,0,8)
        if date0 eq date1 then begin
            searchdirregexp = date0
        endif else begin
            date = [date0, date1]
            date = date[sort(date)]
            for i=1, strlen(date[0])-1 do begin
                if ~strcmp(strmid(date0, 0,i),strmid(date1, 0, i)) then break
            endfor
            searchdirregexp = strmid(date0, 0, i-1)+'*'
        endelse

    	tracknums = file_basename(file_search(ao_datadir()+path_sep()+'adsec_data'+path_sep()+searchdirregexp+path_sep()+'Data_*', /TEST_DIRECTORY))
    	for i=0L, n_elements(tracknums)-1 do tracknums[i]  = strmid(tracknums[i], 5)

    	for i=0L, n_elements(tracknums)-1 do begin
        	o_track = obj_new('AOtracknum', tracknums[i])
        	if (o_track->julday() ge from_julday) and (o_track->julday() le to_julday) then begin
        	    if keyword_set(check) then begin
        	        tmp = getaoelab(tracknums[i])
        	        if not obj_valid(tmp) then continue
        	    endif
            	self->add, tracknums[i]
        	endif
        	obj_destroy, o_track
    	end
	endif else begin
		; check that the values are string representing tracknums
		for ii=0, n_elements(tracknumlist)-1 do begin
			if not stregex(tracknumlist[ii], '^[0-9]{8}_[0-9]{6}$', /bool) then continue
			if keyword_set(check) then begin
			    tmp = getaoelab(tracknumlist[ii])
			    if not obj_valid(tmp) then continue
			endif
			self->add, tracknumlist[ii]
		endfor
	endelse

    if keyword_set(recompute) then begin
        for i=0L, self->Count()-1 do ee = getaoelab(self->Get(pos=i),/recompute)
    endif

    if not self->AOhelp::Init('AOdataset', 'Represent a list of AO measurements') then return, 0
    self->addMethodHelp, "tracknums()", "returns an array  of tracknums"
    self->addMethodHelp, "tracknums_str", "print the list of tracknums as a string"
    self->addMethodHelp, "count()", "number of TNs in dataset"
    self->addMethodHelp, "RemoveTracknum, tracknums", "remove the array of tracknums from this set"
    self->addMethodHelp, "value(cmd)", "evaluates 'cmd' for all tracknums and returns an array of results"
    self->addMethodHelp, "where(cmd, operand, value)", "return an index of dataset items that match cmd+operand+value"
    self->addMethodHelp, "modalplot", "overall modalplot of the dataset"
    self->addMethodHelp, "plot, varX, varY", "Plot procedure: use value() syntaxis for varX and varY"
    self->addMethodHelp, "autogains", "returns an array of tracknums which are of type autogains"
    self->addMethodHelp, "aohelp, str, idx=idx", "Call help method of first (idx=0) AOelab member of dataset"
    return, 1
end

function AOdataset::tracknums
    ;if ptr_valid(self._tracknums) then return, *(self._tracknums) else return, ""
    return, self->get()
end

;Print, the list of tracknums as a string
pro AOdataset::tracknums_str
	tr = self->tracknums()
	ntr_per_col = 10
	str = "["
	cc=0
	for ii=0, n_elements(tr)-1 do begin
		if ii ne n_elements(tr)-1 then begin
			if cc lt ntr_per_col then begin
				str += ("'"+tr[ii]+"', ")
				cc+=1
			endif else begin
				str += ("'"+tr[ii]+"',$")
				cc=0
				print, str
				str = ""
			endelse
		endif else begin
			str += ("'"+tr[ii]+"']")
			print, str
		endelse
	endfor
end

;
; return a set union of this and of the passed one
;
function AOdataset::union, dataset2
    if not obj_isa(dataset2, 'AOdataset') then message, 'argument is not a valid AOdataset object', BLOCK='elab', name='ao_oaa_dataset'
    elem2  = dataset2->Get()
    elem   = self->Get()
    return, obj_new('aodataset', [elem, elem2])
end

;
; return a set that is the intersection of this and of the passed one
;
function AOdataset::intersection, dataset2
    if not obj_isa(dataset2, 'AOdataset') then message, 'argument is not a valid AOdataset object', BLOCK='elab', name='ao_oaa_dataset'
    elem2  = dataset2->Get()
    elem   = self->Get()
    res = obj_new('aodataset')
    for i=0, n_elements(elem2)-1 do begin
        idx=where(elem2[i] eq elem, cnt)
        if cnt gt 0 then res->add, elem2[i]
    endfor
    return, res
end



pro AOdataset::RemoveTracknum, tracknums
    for i=0, n_elements(tracknums)-1 do begin
        idx = where (self->Get() eq tracknums[i], cnt)
        if cnt gt 0 then dum = self->Remove(idx)
    endfor
end

;
; report the measures that are NOT ok, and why
;
;pro AOdataset::Wrong
;    idx = self->where('isok', 'eq', 0)
;    for i=0L,n_elements(idx)-1 do begin
;        r=(self->get(pos=idx[i]))->isok(cause=cause)
;        tn=(self->get(pos=idx[i]))->tracknum()
;        print, tn, cause
;    endfor
;end

;
; free memory
;
pro AOdataset::free
    tns = self->Get()
    for i=0L, self->Count()-1 do begin
        ee = getaoelab(tns[i])
        if obj_valid(ee) eq 0 then begin
            message, tns[i] + ' skipped because it lacks required data', /info
            continue
        endif
        ee->free
    endfor
end

;
;
;
function AOdataset::value, cmd, set_out=set_out, VERBOSE=VERBOSE

    objref = self->Get()
    index = lindgen(self->count())
	nel = n_elements(objref)
    isvalid = bytarr(nel)
    error = 0
    start = systime(/sec)

	for i=0L, nel-1 do begin
           now = systime(/sec)

		catch, error
		if error eq 0 then tmpobj=getaoelab(objref[i]) else begin
            catch, /cancel
            if keyword_set(verbose) then print, format='(%"%s skipped. %d:%s ")', objref[i], error, !error_state.msg
            continue
        endelse
        catch, /cancel
       	if not obj_valid(tmpobj) then begin
            if keyword_set(verbose) then print, objref[i] + ' skipped.'
            continue
        endif
        if now-start gt 2 then print, format='($, %"%s, %d%% done\r")',tmpobj->tracknum(), fix(i*100.0/nel)
        close,/all ; Every now and then a logical file unit remains open!
		if keyword_set(verbose) then print, 'Inspecting :'+tmpobj->tracknum()
		catch, error
		if error eq 0 then begin
			value = tmpobj->ex(cmd, isvalid=iv)
      		isvalid[i] = iv
        	if isvalid[i] eq 1 then begin
            	if n_elements(v) eq 0 then v = ptrarr(nel) ; first time value is valid
            	v[i] = ptr_new(value, /no_copy)
            	if keyword_set(verbose) then print, 'data found'
        	endif else if keyword_set(verbose) then print, 'data NOT found'
       		if obj_hasmethod(tmpobj, 'free') then tmpobj->free
       	endif else begin
       		catch, /cancel
       		if keyword_set(verbose) then print, 'data NOT found'
       		if obj_hasmethod(tmpobj, 'free') then tmpobj->free
       	endelse
       	catch, /cancel
	endfor
        if now-start gt 2 then print, format='($, %"                          \r")'

	;;;; If data has different characteristics (dimensions, type, etc) return a pointer.
	;;;; Otherwise, return a data array.
    valid = where(isvalid eq 1, cntvalid)
	if cntvalid ne 0 then begin

    	if arg_present(set_out) then set_out = obj_new('aodataset', self->Get(pos=index[valid]))

		;check type
		type_all = lonarr(cntvalid)
		for jj=0L, cntvalid-1 do type_all[jj] = size( *v[valid[jj]], /type)
		if max(type_all)-min(type_all) ne 0 then return, v[valid]
		type = type_all[0]

		if type eq 8 then begin		;;;case of data structures

			for i=0L, cntvalid-1 do $
			  if i eq 0 then data = create_struct(      'tr'+objref[valid[i]], *v[valid[i]]) else $
							 data = create_struct(data, 'tr'+objref[valid[i]], *v[valid[i]])

		endif else begin			;;;case of other data types

			;check number of dimensions
			ndim_all = lonarr(cntvalid)
			for jj=0L, cntvalid-1 do ndim_all[jj] = size( *v[valid[jj]], /n_dim)
			if max(ndim_all)-min(ndim_all) ne 0 then return, v[valid]
			n_dim = ndim_all[0]

			;check number of elements in each dimension
			if n_dim ne 0 then dim_all = make_array(cntvalid, n_dim, /LONG) else $
							   dim_all = make_array(cntvalid, /LONG)
			for jj=0L, cntvalid-1 do dim_all[jj,*] = size( *v[valid[jj]], /dim)
			if n_dim eq 0 then zero = 0 else zero = lonarr(n_dim)
			if total( max(dim_all, dim=1)-min(dim_all, dim=1) ne zero ) then return, v[valid]
			dim = reform(dim_all[0,*])

			;create the data array
			n_ele = size( *v[valid[0]], /n_elements)
			if n_dim eq 0 then data = make_array(dim=cntvalid, type=type) else $
							   data = make_array(dim=[dim,cntvalid], type=type)

    		for i=0L, cntvalid-1 do data[n_ele*i] = reform(*v[valid[i]],n_ele)
    	endelse

    	ptr_free, v
		return, data

	endif else begin
		if arg_present(set_out) then set_out = obj_new()
		return, -1
	endelse

end
;
; operand can be one of
;
; KEYWORDS
;   ptrdata    (output) array of pointers to values matching the condition
;   index        if set, return an index array instead of a new dataset
;
; return a dataset of items that matches the condition operand+reference_value
;
;
function AOdataset::where, cmd, operand, reference_value, ptrdata=ptrdata, verbose=verbose, index=index

    objref = self->Get()
    nel = self->Count()
    isvalid = bytarr(nel)
    error = 0

    for i=0L, nel-1 do begin
    	catch, error
		if error eq 0 then tmpobj = getaoelab(objref[i]) else begin
            catch, /cancel
            if keyword_set(verbose) then print, format='(%"%s skipped. %d:%s ")', objref[i], error, !error_state.msg
            continue
        endelse
        catch, /cancel
       	if not obj_valid(tmpobj) then begin
            if keyword_set(verbose) then print, objref[i] + ' skipped.'
            continue
        endif

		catch, error
		if error eq 0 then begin
        	value = tmpobj->ex(cmd, isvalid=iv)
      		isvalid[i] = iv

        	; if it is invalid (maybe method or object not-existing) skip to next record
        	if isvalid[i] eq 0 then continue

        	; check operand and value
        	if n_elements(operand) ne 0 and n_elements(reference_value) ne 0 then begin
            	case operand of
                	'lt': if not (value lt reference_value) then isvalid[i]=0
                	'le': if not (value le reference_value) then isvalid[i]=0
                	'gt': if not (value gt reference_value) then isvalid[i]=0
                	'ge': if not (value ge reference_value) then isvalid[i]=0
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
                    else : begin
                    		message, 'operand not recognized',/info
                    		return, obj_new()
                    	   end
            	endcase
        	endif

        	; if elements match the condition
        	if arg_present(ptrdata) then $
         	if isvalid[i] eq 1 then begin
            	if  n_elements(v) eq 0 then v = ptrarr(nel) ; first time value is valid
            	v[i]=ptr_new(value, /no_copy)
         	endif
        	if obj_valid(tmpobj) then $
        		if obj_hasmethod(tmpobj, 'free') then tmpobj->free
        endif else begin
       		catch, /cancel
       		if keyword_set(verbose) then print, 'data NOT found'
       		if obj_hasmethod(tmpobj, 'free') then tmpobj->free
       	endelse
       	catch, /cancel
    endfor

    valid = where(isvalid eq 1, cntvalid)
    if keyword_set(index) then return,valid
    if arg_present(ptrdata) then ptrdata = cntvalid eq 0 ? ptr_new() : v[valid]
	if cntvalid eq 0 then return, obj_new()

	;if arg_present(AOdata_subset) then $
	;	AOdata_subset = obj_new('aodataset', objarray=self->Get(pos=valid))
    ;return, valid
	return, obj_new('aodataset', self->Get(pos=valid))
end

;;;;;;;;;;;;;;;;;;; list of string implementation (list()like)  ;;;;;;;;;;;;;;
; in IDL8 this will be replaced by list() ;

function aodataset::count
    return, self._nelems
end

function aodataset::Get, pos=pos
    if self->count() eq 0 then return, ""
    if n_elements(pos) ne 0  then begin
        if max(pos) ge self->count() then begin
            message, 'aodataset::Get: Index is out of range', /info
            return, ""
        endif
        return, (*self._values)[pos]
    endif
    return, *self._values
end


pro aodataset::Add, value
    if self->count() gt 0 then begin
        values = self->Get()
        dum = where(values eq value, cnt)
        if cnt gt 0 then return
        ptr_free, self._values
        self._values = ptr_new( [values, value], /no_copy)
    endif else begin
        self._values = ptr_new([value], /no_copy)
    endelse
    self._nelems += 1
end


function aodataset::Remove, idx
    vals = self->Get()
    nobj = self->count()
    if idx ge nobj then message, 'AOLIST::REMOVE: Index is out of range', /info
    res = vals[idx]
    ptr_free, self._values
    ; decrement counter
    self._nelems -= 1
    ; if set contains just one elements simply return it
    if nobj eq 1 then return, res
    case idx of
        0:        newvals = vals[1:*]
        nobj-1:   newvals = vals[0:nobj-2]
        else:     newvals = [ vals[0:idx-1], vals[idx+1:*] ]
    endcase
    self._values = ptr_new( newvals, /no_copy)
    return, res
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Graphical methods for datasets

;+
;
;-
pro aodataset::modalplot, average = average, overplot = overplot, _extra=ex
	tr = self->tracknums()
	case self->count() of
	  1: cols = [!P.COLOR]
	  2: cols = [!P.COLOR, 250L]
	  else: cols = [[!P.COLOR],comp_colors(self->count()-1)]
	endcase

    n=0
    if keyword_set(average) then begin
        init_progress, self->count()
        for ii=0, self->count()-1 do begin
            ao = getaoelab(self->get(pos=ii))
            if not obj_valid(ao) then continue
            if ao->meas_type() ne 'LOOP' then continue
            if n_elements(clvar) eq 0 then clvar = (ao->modalpositions())->time_variance()*0
            if n_elements(nmodes) eq 0 then nmodes = (ao->modes())->nmodes()
            if n_elements(olvar) eq 0 then if obj_valid(ao->adsec_status()) then if (ao->adsec_status())->disturb_status() ne 0 then begin
                olvar  = (ao->modaldisturb())->time_variance() * ((ao->modaldisturb())->norm_factor())^2.
            endif
            clvar += (ao->modalpositions())->time_variance() * (1e9*ao->reflcoef())^2.
            n +=1
            progress, n, ao->tracknum()
            ao->free
        endfor
        clvar  = clvar /n
        yrange = sqrt(minmax(clvar))
        if n_elements(olvar) gt 0 then yrange = sqrt(minmax([clvar,olvar]))
        if not keyword_set(overplot) then begin
            title = tr[0] + ' to ' + tr[n_elements(tr)-1] + ' (average)'
            plot_oo, lindgen(nmodes)+1, sqrt(clvar), psym=-1, symsize=0.8, charsize=1.2, ytitle='nm rms wf', xtitle='mode number', $
                 yrange=yrange, title=title, _extra=ex
        endif else begin
            oplot, lindgen(nmodes)+1, sqrt(clvar), psym=-1, _extra=ex
        endelse
        if (ao->adsec_status())->disturb_status() ne 0 then begin
                oplot, lindgen(nmodes)+1, sqrt(olvar), psym=-2, symsize=0.8, color='0000ff'x
                legend, ['disturbance','closed-loop'], color=['0000ff'x,!P.color], psym=-[2,1], /right
        endif
        ao->free
    endif else begin
        init_progress, self->count()
        first=1
	    for ii=0, self->count()-1 do begin
		    ao = getaoelab(self->get(pos=ii))
            if not obj_valid(ao) then continue
            if ao->meas_type() ne 'LOOP' then continue
		    if first eq 1 then ao->modalplot, _extra=ex, title='' else $
		    			       ao->modalplot, /OVERPLOT, COLOR=cols[ii]
            first=0
            progress, ii, ao->tracknum()
            ao->free
	    endfor
	    legend, tr, psym=replicate(-1,self->count()), color=cols, /right
    endelse
end


;+
;
;-
pro aodataset::plot, X_VAR, Y_VAR, HISTO_VAR=HISTO_VAR, GROUP_VAR=GROUP_VAR $
		, g_leg_title=g_leg_title, h_leg_title=h_leg_title, _EXTRA = ex, CURSOR=CURSOR

	nparams = n_params()
	if nparams ne 2 then begin
		message, 'Usage: db->plot, X, Y, [,...]',/info
		return
	endif

	;X and Y variables
	;- - - - - - - - - - -
	X = self->value(X_VAR, set_out=set1)
	if size(X,/type) eq 2 then if X[0] eq -1 then return
	Y = set1->value(Y_VAR, set_out=set2)
	if size(Y,/type) eq 2 then if Y[0] eq -1 then return
	if n_elements(X) ne n_elements(Y) then X = set2->value(X_VAR)


	;Histogram variable
	;- - - - - - - - - - -
	if n_elements(HISTO_VAR) ne 0 then begin
		H = set2->value(HISTO_VAR, set_out=set3)
		if H[0] eq -1 then return
		if n_elements(H) ne n_elements(X) then begin
			X = set3->value(X_VAR)
			Y = set3->value(Y_VAR)
		endif
		if n_elements(h_leg_title) eq 0 then h_leg_title = HISTO_VAR+':'
	endif else set3 = set2

	;Group variable
	;- - - - - - - - - - -

	if n_elements(GROUP_VAR) ne 0 then begin
		G = set3->value(GROUP_VAR, set_out=set4)
		if size(G,/type) eq 2 then if G[0] eq -1 then return
		if n_elements(G) ne n_elements(X) then begin
			X = set4->value(X_VAR)
			Y = set4->value(Y_VAR)
			H = set4->value(HISTO_VAR)
		endif
		if n_elements(g_leg_title) eq 0 then g_leg_title = GROUP_VAR+':'
	endif else set4 = set3

	if keyword_set(CURSOR) then begin
		tr = set4->value('tracknum')
	endif

	aoplot, X, Y, HISTO_VAR=H, GROUP_VAR=G, _EXTRA = ex, CURSOR=CURSOR, tr=tr $
		  , g_leg_title=g_leg_title, h_leg_title=h_leg_title

end

function aodataset::autogains
    if self->Count() eq 0 then return, -1
    return, (self->tracknums())[ where( self->value('meas_type') eq 'AG')]
end

pro aodataset::aohelp, metodo, idx=idx
	if n_elements(idx) eq 0 then idx=0
	ao = getaoelab(self->get(pos=idx))
	ao->help, metodo
end

pro AOdataset::recompute
    for i=0L, self->Count()-1 do ee = getaoelab(self->Get(pos=i),/recompute)
end

function AOdataset::group, cmd
    v = self->value(cmd)
    vv = v[uniq(v)]
    res = obj_new('IDL_container')
    for i=0,n_elements(vv)-1 do res->add, self->where(cmd, 'eq', vv[i])
    return, res

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro AOdataset::Cleanup
    ptr_free, self._values
    self->AOhelp::Cleanup
end


pro AOdataset__define
    struct = { AOdataset, $
        _root_dir           : "", $
        _values         : ptr_new() , $
        _nelems         : 0L, $
        _allocated_size : 0L, $
        INHERITS    AOhelp $
         ;inherits  AOlist $
    }
end

