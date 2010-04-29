
;+
;
;-

function AOcontainer::Init
    return, 1
end

pro AOcontainer::Add, object
    IF (not PTR_VALID(self._array)) then begin 
        array = objarr(1)
        array[0] = object
        self._array = ptr_new(array, /no_copy)
    endif else begin
        vvv = arrinsert((*self._array), object)
        PTR_FREE,self._array
        self._array = ptr_new(vvv,/no_copy)
    endelse
    self._nelem = n_elements(*self._array)
end

; think carefully before implementing it!
;pro AOcontainer::Remove, index
;    if not ptr_valid(self._array) then 
;end

function AOcontainer::nelem
    return, self->IDL_CONTAINER::Count()
end


;
; return {data, indexvalid}
;
function AOcontainer::cerca, cmd
    apex = string(39B)
    
    nel = n_elements(*self._array)
    cmds = strsplit(cmd, '.', /extr)
    isvalid = bytarr(nel)

    for i=0, nel-1 do begin
        ; tmpobj = (*self._array)[i]
        ; for j=0, n_elements(cmds)-2 do begin
        ;   hasmethod = obj_hasmethod(tmpobj, '$cmds[j]$')
        ;   if (hasmethod) then tmpobj= tmpobj->$cmds[j]$()
        ;   if not obj_valid(tmpobj) then break;
        ; endfor
        ; hasmethod = obj_hasmethod(tmpobj, '$cmds[j]$')
        ; if (hasmethod) then value = tmpobj->$cmds[j]$()
        ;
        tmpobj = (*self._array)[i]
        for j=0, n_elements(cmds)-2 do begin
            r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+cmds[j]+apex+')')
            r=execute('if (hasmethod) then tmpobj= tmpobj->'+cmds[j]+'()')
            if not obj_valid(tmpobj) then break ;
        endfor
        if j eq n_elements(cmds)-1 then begin
            r=execute('hasmethod = obj_hasmethod(tmpobj, '+apex+cmds[j]+apex+')')
            r=execute('if (hasmethod) then begin & value = tmpobj->'+cmds[j]+'() & isvalid[i]=1 & endif')
            if test_type(value, /obj_ref) eq 0 and obj_valid(value) eq 0 then isvalid[i]=0
        endif 
        if isvalid[i] eq 1 then begin
            if  n_elements(v) eq 0 then begin ; first time value is valid
                v = replicate(value,nel)
            endif
            v[i]=value
        endif
    endfor
    if n_elements(v) eq 0 then return, {data:0, valid:-1};  if no valid values found, return null data
    valid = where(isvalid eq 1)
    return, {data:v[valid], valid:valid}
end


pro AOcontainer::Cleanup
    heap_free, self._array
    self->IDL_CONTAINER::Cleanup
end

pro AOcontainer__define
    struct = { AOcontainer, $
        _array : ptr_new(), $
        _nelem  : 0L,       $
        INHERITS IDL_CONTAINER $
    }
end

