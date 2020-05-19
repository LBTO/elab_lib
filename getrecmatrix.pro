
function aomultiton_rec::Init
    self._tag_list = ptr_new([''])
    self._obj_list = obj_new('IDL_Container')
    return, 1
end

function aomultiton_rec::getobj, fname
    if fname eq "" then return, obj_new()
    if (n_elements(*self._tag_list)-1) ne self._obj_list->Count() then message, 'FATAL ERROR'

    pos = where(*self._tag_list eq fname, cnt)
    if cnt gt 1 then message, 'Elablib error. 2 rec with the same name saved in a aomultiton_rec'
    if cnt ne 0 then begin
        obj = self._obj_list->Get(pos=pos)
        if not obj_valid(obj) then message, 'Elablib error. aorecmatrix object not valid '+fname
        return, obj
    endif
    
    oo = obj_new('AOrecmatrix', fname)
    if not obj_valid(oo) then message, 'Could not initialize aorecmatrix '+fname
    
    tags = *self._tag_list
    ptr_free, self._tag_list
    self._tag_list = ptr_new([fname, tags])
    self._obj_list->add, oo, pos=0 
    return, self._obj_list->Get(pos=0)
end

; for debug
pro aomultiton_rec::debug
    print, *self._tag_list
    print, self._obj_list->Get(/all)
end

pro aomultiton_rec__define
    struct = { aomultiton_rec, $
        _tag_list   : ptr_new() ,$
        _obj_list   : obj_new()  $
    }
end

function getrecmatrix, tag
    defsysv, "!aomultiton_rec", EXISTS=exists
    if not exists then begin
        aomultiton_rec = obj_new('aomultiton_rec')
        defsysv, "!aomultiton_rec", aomultiton_rec
    endif
    return, !aomultiton_rec->getobj(tag) 
end

