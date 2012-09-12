;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function aosingleton_db::Init, SYSTEM_ID
    self._obj_db = obj_new('aodb', SYSTEM_ID)
    if n_elements(SYSTEM_ID) ne 0 then self._SYSTEM_ID = SYSTEM_ID
    return, 1
end

function aosingleton_db::getobj
    return, self._obj_db
end

function aosingleton_db::system_id
	return, self._system_id
end

; for debug
pro aosingleton_db::debug
    print, self._obj_db
end

pro aosingleton_db__define
    struct = { aosingleton_db, $
        _obj_db 	: obj_new(), $
        _SYSTEM_ID	: 0L $
    }
end

function getdb, SYSTEM_ID
    defsysv, "!aosingleton_db", EXISTS=exists
    if not exists then begin
        aosingleton_db = obj_new('aosingleton_db', SYSTEM_ID)
        defsysv, "!aosingleton_db", aosingleton_db
    endif else begin
    	if !aosingleton_db->system_id() ne SYSTEM_ID then message, 'getdb() e fatta a caso per il testina di LBU!!!!'
    endelse
    return, !aosingleton_db->getobj()
end
