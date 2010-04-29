
function aomultiton_rec::Init
    self._tag_list = ptr_new([''])
    self._obj_list = obj_new('IDL_Container')
    return, 1
end

function aomultiton_rec::getobj, fname
    if fname eq "" then return, obj_new()

    tags = *self._tag_list
    pos = where(tags eq fname, cnt)
    if cnt ne 0 then begin
        obj = self._obj_list->Get(pos=pos)
        if obj_valid(obj) then return, obj
    endif
    
    oo = obj_new('AOrecmatrix', fname)
    if not obj_valid(oo) then return, obj_new()
    
    ptr_free, self._tag_list
    self._tag_list = ptr_new([fname, tags])
    self._obj_list->add, oo, pos=0 
    return, self._obj_list->Get(pos=0)
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

