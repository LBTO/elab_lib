function aomultiton_adsecstructs::Init
    self._tag_list = ptr_new([''])
    self._obj_list = obj_new('IDL_Container')
    return, 1
end

function aomultiton_adsecstructs::getobj, fname
    if fname eq "" then return, obj_new()
    if (n_elements(*self._tag_list)-1) ne self._obj_list->Count() then message, 'FATAL ERROR'

    pos = where(*self._tag_list eq fname, cnt)
    if cnt gt 1 then message, 'Elablib error. 2 rec with the same name saved in a aomultiton_adsecstructs'
    if cnt ne 0 then begin
        obj = self._obj_list->Get(pos=pos)
        if not obj_valid(obj) then message, 'Elablib error. aoadsec_struct object not valid '+fname
        return, obj
    endif
    
    oo = obj_new('AOadsec_struct', fname)
    if not obj_valid(oo) then message, 'Could not initialize aoadsec_struct '+fname
    
    tags = *self._tag_list
    ptr_free, self._tag_list
    self._tag_list = ptr_new([fname, tags])
    self._obj_list->add, oo, pos=0 
    return, self._obj_list->Get(pos=0)
end

; for debug
pro aomultiton_adsecstructs::debug
    print, *self._tag_list
    print, self._obj_list->Get(/all)
end

pro aomultiton_adsecstructs__define
    struct = { aomultiton_adsecstructs, $
        _tag_list   : ptr_new() ,$
        _obj_list   : obj_new()  $
    }
end

function getadsecstructs, fname
    defsysv, "!aomultiton_adsecstructs", EXISTS=exists
    if not exists then begin
        aomultiton_adsecstructs = obj_new('aomultiton_adsecstructs')
        defsysv, "!aomultiton_adsecstructs", aomultiton_adsecstructs
    endif
    return, !aomultiton_adsecstructs->getobj(fname) 
end

