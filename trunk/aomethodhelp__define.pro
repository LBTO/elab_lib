

function AOmethodhelp::Init, syntax, descr
    self._syntax = syntax
    self._descr  = descr
    return, 1
end

function AOmethodhelp::syntax
    return, self._syntax
end

function AOmethodhelp::descr
    return, self._descr
end

pro AOmethodhelp::Cleanup
end

pro AOmethodhelp__define
    struct = { AOmethodhelp, $
        _syntax : "" , $
        _descr  : ""   $
    }
end
