;+
;
; A list of heap variables (including reference to object)
; 
; NOTES
;   IDL8.0 introduced the object list that make this one obsolete...
;
;-

function aolist::Init
    self._nelems = 0
    return,1
end

function aolist::count
    return, self._nelems
end

function aolist::Get, pos=pos, all=all
    if self->count() eq 0 then return, obj_new()
    if n_elements(pos) ne 0  then begin 
        if max(pos) ge self->count() then begin 
            message, 'AOLIST::Get: Index is out of range', /info
            return, obj_new()
        endif
        return, (*self._values)[pos]
    endif
    return, *self._values  
end


pro aolist::Add, value
    if self->count() gt 0 then begin
        values = self->Get()
        ptr_free, self._values
        self._values = ptr_new( [values, value], /no_copy)
    endif else begin
        self._values = ptr_new([value], /no_copy)
    endelse
    self._nelems += 1
end


function aolist::Remove, idx
    vals = self->Get()
    nobj = self->count()
    if idx ge nobj then message, 'AOLIST::REMOVE: Index is out of range', /info
    res = vals[idx]
    ptr_free, self._values
    case idx of
        0:        newvals = vals[1:*]
        nobj-1:   newvals = vals[0:nobj-2]
        else:     newvals = [ vals[0:idx-1], vals[idx+1:*] ]
    endcase
    self._values = ptr_new( newvals, /no_copy)
    ; decrement counter
    self._nelems -= 1
    return, res
end

pro aolist::Cleanup
    ptr_free, self._values
end

pro aolist__define
    struct = { aolist, $
        _values         : ptr_new() , $
        _values_type    : "", $
        _nelems         : 0L, $
        _allocated_size : 0L  $
        ;_multiple       : 0B $
    }
end