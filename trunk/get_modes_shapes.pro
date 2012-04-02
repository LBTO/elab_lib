
function aomultiton_modeshapes::Init
    self._tag_list = ptr_new([''])
    self._obj_list = obj_new('IDL_Container')
    return, 1
end

function aomultiton_modeshapes::getobj, fname
    if fname eq "" then return, obj_new()
    if (n_elements(*self._tag_list)-1) ne self._obj_list->Count() then message, 'FATAL ERROR'

    pos = where(*self._tag_list eq fname, cnt)
    if cnt gt 1 then message, 'Elablib error. 2 modes shapes with the same name saved in a aomultiton_modeshapes'
    if cnt ne 0 then begin
        obj = self._obj_list->Get(pos=pos)
        if not obj_valid(obj) then message, 'Elablib error. AOmodeShapes object not valid '+fname
        return, obj
    endif

    oo = obj_new('AOmodeShapes', fname)
    if not obj_valid(oo) then begin
        message, 'Could not initialize AOmodeShapes '+fname, /info
        return, obj_new()
    endif

    tags = *self._tag_list
    ptr_free, self._tag_list
    self._tag_list = ptr_new([fname, tags])
    self._obj_list->add, oo, pos=0
    return, self._obj_list->Get(pos=0)
end

; for debug
pro aomultiton_modeshapes::debug
    print, *self._tag_list
    print, self._obj_list->Get(/all)
end

pro aomultiton_modeshapes__define
    struct = { aomultiton_modeshapes, $
        _tag_list   : ptr_new() ,$
        _obj_list   : obj_new()  $
    }
end

function get_modes_shapes, tag
    defsysv, "!aomultiton_modeshapes", EXISTS=exists
    if not exists then begin
        aomultiton_modeshapes = obj_new('aomultiton_modeshapes')
        defsysv, "!aomultiton_modeshapes", aomultiton_modeshapes
    endif
    return, !aomultiton_modeshapes->getobj(tag)
end

