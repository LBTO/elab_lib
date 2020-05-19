function M::Init
    self->Compute
    return, 1
end

pro M::Compute
    self._dati =  self->GetDati()
    ; compute M(value) stuff here
    print, 'value is ', *self._dati
end

function M::GetDati
    message, 'M::GetDati is abstract'
end


pro M__define
    struct = { M, $
        _dati : ptr_new() $
    }
end




function A::Init
    ret=self->M::Init()
    return, 1
end

; to be implemented in M subclasses
function A::GetDati
    if not ptr_valid(self._value) then self._value=ptr_new(ulong(systime(/sec)))
    return, self._value
end

; if you want to release dati resource
pro A::FreeDati
    ptr_free, self._value
end

pro A::Cleanup
    ptr_free, self._value
    ; altro cleanup per la classe A
end

pro A__define
    struct = { A,   $
        _value :  ptr_new(),$
        INHERITS M $
    }
end




