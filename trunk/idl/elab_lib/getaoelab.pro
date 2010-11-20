function aomultiton_elab::Init
    self._tag_list = ptr_new([''])
    self._obj_list = obj_new('IDL_Container')
    return, 1
end

;
;
function aomultiton_elab::getobj, tn, recompute=recompute, _extra=ex
    if tn eq "" then return, obj_new()

    tags = *self._tag_list
    pos = where(tags eq tn, cnt)
    if cnt gt 1 then message, 'Elablib error. 2 aoelab with the same name saved in a aomultiton_elab'
    if cnt ne 0 then begin
        ; if recompute is set, remove from the list the previous reference to this tn
        ; (client that already got reference to this tn will not see the recomputed values)
        if keyword_set(recompute) then begin
            nobj = n_elements(tags)
            case pos of
             0:         tags = tags[1:*] 
             nobj-1:    tags = tags[0:nobj-2]
             else:      tags = [ tags[0:pos-1], tags[pos+1:*] ]
            endcase
        endif else begin
            obj = self._obj_list->Get(pos=pos)
            if not obj_valid(obj) then message, 'Elablib error. aoelab object not valid '+tn
            return, obj
        endelse
    endif
    
    message, 'Getting tracknum '+tn, /info
    oo = obj_new('aoelab', tn, recompute=recompute, _extra=ex)
    if not obj_valid(oo) then begin
        message, 'Could not initialize aoelab '+tn, /info
        return, obj_new()
    endif
    
    ptr_free, self._tag_list
    self._tag_list = ptr_new([tn, tags])
    self._obj_list->add, oo, pos=0 
    return, self._obj_list->Get(pos=0)
end

pro aomultiton_elab::free
    objs = self._obj_list->Get(/all)
    for i=0,self._obj_list->Count()-1 do objs[i]->free
end

; for debug
pro aomultiton_elab::debug
    print, *self._tag_list
    print, self._obj_list->Get(/all)
end

pro aomultiton_elab__define
    struct = { aomultiton_elab, $
        _tag_list   : ptr_new() ,$
        _obj_list   : obj_new()  $
    }
end


function getaoelab, tracknum, $
            recompute = recompute, $
            dark_fname=dark_fname, $
            modal_reconstructor_file=modal_reconstructor_file       ; this is used in case of kalman filter

    ;on_error, 2
    defsysv, "!ao_env", EXISTS=exists
    if not exists then message, 'Call ao_init first!'
    defsysv, "!aomultiton_elab", EXISTS=exists
    if not exists then begin
        aomultiton_elab = obj_new('aomultiton_elab')
        defsysv, "!aomultiton_elab", aomultiton_elab
    endif
    return, !aomultiton_elab->getobj(tracknum, modal_rec=modal_reconstructor_file, recompute=recompute, dark_fname=dark_fname) 
;    return, obj_new('AOelab', tracknum, modal_rec=modal_reconstructor_file, recompute=recompute, dark_fname=dark_fname)
end


