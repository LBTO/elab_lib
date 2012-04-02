
function aomultiton_badpixelmap::Init
    self._tag_list = ptr_new([''])
    self._obj_list = obj_new('IDL_Container')
    return, 1
end

function aomultiton_badpixelmap::getobj, fname
    if fname eq "" then return, obj_new()
    if (n_elements(*self._tag_list)-1) ne self._obj_list->Count() then message, 'FATAL ERROR'

    pos = where(*self._tag_list eq fname, cnt)
    if cnt gt 1 then message, '2 badpixelmap with the same name saved in a aomultiton_badpixelmap'
    if cnt ne 0 then begin
        obj = self._obj_list->Get(pos=pos)
        if not obj_valid(obj) then message, 'aobadpixelmap object not valid '+fname
        return, obj
    endif
    
    oo = obj_new('AObadpixelmap', fname)
    if not obj_valid(oo) then message, 'Could not initialize aobadpixelmap '+fname
    
    tags = *self._tag_list
    ptr_free, self._tag_list
    self._tag_list = ptr_new([fname, tags])
    self._obj_list->add, oo, pos=0 
    return, self._obj_list->Get(pos=0)
end

; for debug
pro aomultiton_badpixelmap::debug
    print, *self._tag_list
    print, self._obj_list->Get(/all)
end

pro aomultiton_badpixelmap__define
    struct = { aomultiton_badpixelmap, $
        _tag_list   : ptr_new() ,$
        _obj_list   : obj_new()  $
    }
end

function getbadpixelmap, tag
    defsysv, "!aomultiton_badpixelmap", EXISTS=exists
    if not exists then begin
        aomultiton_badpixelmap = obj_new('aomultiton_badpixelmap')
        defsysv, "!aomultiton_badpixelmap", aomultiton_badpixelmap
    endif
    return, !aomultiton_badpixelmap->getobj(tag) 
end

