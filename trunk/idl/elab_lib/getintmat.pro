
function aomultiton_im::Init
    self._tag_list = ptr_new([''])
    self._obj_list = obj_new('IDL_Container')
    return, 1
end

function aomultiton_im::getobj, fname
    if fname eq "" then return, obj_new()
    if (n_elements(*self._tag_list)-1) ne self._obj_list->Count() then message, 'FATAL ERROR. CALL FQP'
    
    pos = where(*self._tag_list eq fname, cnt)
    if cnt ne 0 then begin
        obj = self._obj_list->Get(pos=pos)
        if not obj_valid(obj) then message, 'Elablib error. aointmat object not valid '+fname
        return, obj
    endif

    oo = obj_new('AOintmat', fname)
    if not obj_valid(oo) then return, obj_new()

    tags = *self._tag_list
    ptr_free, self._tag_list
    self._tag_list = ptr_new([fname, tags])
    self._obj_list->add, oo, pos=0
    return, self._obj_list->Get(pos=0)
end

; for debug
pro aomultiton_im::debug
    print, *self._tag_list
    print, self._obj_list->Get(/all)
end

pro aomultiton_im__define
    struct = { aomultiton_im, $
        _tag_list   : ptr_new() ,$
        _obj_list   : obj_new()  $
    }
end

function getintmat, tag
    defsysv, "!aomultiton_im", EXISTS=exists
    if not exists then begin
        aomultiton_im = obj_new('aomultiton_im')
        defsysv, "!aomultiton_im", aomultiton_im
    endif
    return, !aomultiton_im->getobj(tag)
end

