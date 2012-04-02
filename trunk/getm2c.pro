
;+
;
;-

function aomultiton_m2c::Init
    self._tag_list = ptr_new([''])
    self._obj_list = obj_new('IDL_Container')
    return, 1
end

function aomultiton_m2c::getobj, fname, recompute=recompute
    if fname eq "" then return, obj_new()
    if (n_elements(*self._tag_list)-1) ne self._obj_list->Count() then message, 'FATAL ERROR'

    pos = where(*self._tag_list eq fname, cnt)

    ; object is already in the list
    if cnt ne 0 then begin
        obj = self._obj_list->Get(pos=pos)

        ;; recompute: remove the old one, destroy it,
        ;; make a new one and add it to the list
        if keyword_set(recompute) then begin
            self._obj_list->Remove, pos=pos
            obj_destroy, obj
            obj = obj_new('AOm2c', fname, recompute=recompute)
            if not obj_valid(obj) then begin
                ; remove fname from the tag list
                tags = *self._tag_list
                ptr_free, self._tag_list
                self._tag_list = ptr_new([tags[0:pos-1], tags[pos+1:*]])
                return, obj_new()
            endif else begin
                self._obj_list->add, obj, pos=pos
            endelse
        endif

        if obj_valid(obj) then return, obj
    endif

    oo = obj_new('AOm2c', fname, recompute=recompute)
    if not obj_valid(oo) then return, obj_new()

    tags = *self._tag_list
    ptr_free, self._tag_list
    self._tag_list = ptr_new([fname, tags])
    self._obj_list->add, oo, pos=0
    return, self._obj_list->Get(pos=0)
end

pro aomultiton_m2c__define
    struct = { aomultiton_m2c, $
        _tag_list   : ptr_new() ,$
        _obj_list   : obj_new()  $
    }
end

function getm2c, tag, recompute=recompute
    defsysv, "!aomultiton_m2c", EXISTS=exists
    if not exists then begin
        aomultiton_m2c = obj_new('aomultiton_m2c')
        defsysv, "!aomultiton_m2c", aomultiton_m2c
    endif
    return, !aomultiton_m2c->getobj(tag, recompute=recompute)
end

