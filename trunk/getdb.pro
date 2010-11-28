;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function aosingleton_db::Init
    self._obj_db = obj_new('aodb')
    return, 1
end

function aosingleton_db::getobj
    return, self._obj_db
end

; for debug
pro aosingleton_db::debug
    print, self._obj_db
end

pro aosingleton_db__define
    struct = { aosingleton_db, $
        _obj_db : obj_new() $
    }
end

function getdb
    defsysv, "!aosingleton_db", EXISTS=exists
    if not exists then begin
        aosingleton_db = obj_new('aosingleton_db')
        defsysv, "!aosingleton_db", aosingleton_db
    endif
    return, !aosingleton_db->getobj() 
end
