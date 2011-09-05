

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,,,

function aodict::Init, k_type, v_type; , multiple=multiple ;, real=real, stringa=stringa
    case k_type of
        'integer': self._keys_type = 'integer'
        'real'   : self._keys_type = 'real'
        'string' : self._keys_type = 'string'
        else: message, 'unknown key type'
    endcase
    case v_type of
        'integer': self._values_type = 'integer'
        'real'   : self._values_type = 'real'
        'string' : self._values_type = 'string'
        else: message, 'unknown values type'
    endcase
    self._nelems = 0
    ;self._multiple = keyword_set(multiple)

;    case k_type of
;        'integer': self._keys = ptr_new(0L)
;        'real'   : self._keys = ptr_new(0d)
;        'string' : self._keys = ptr_new('')
;        else: message, 'unknown key type'
;    endcase
;    case v_type of
;        'integer': self._values = ptr_new(0L)
;        'real'   : self._values = ptr_new(0d)
;        'string' : self._values = ptr_new('')
;        else: message, 'unknown values type'
;    endcase

    return, 1
end


;+
; :Description:
;    Insert the pair key,value in the dictionary
;
; :Params:
;    key
;    value
;
; :Error:
;     A message is raised if the value is already in the dictionary and the pair is not inserted
;-
pro aodict::insert, key, value
    ; TODO check only allow scalar insertion
    ;if test_type(key, /string) eq 0 then key_obj=obj_new('aostring_obj', key)
    ;if test_type(key, /real)   eq 0 then key_obj=obj_new('aoreal_obj', key)
    if self->count() gt 0 then begin
        ;if not self->allow_multiple_values() then begin
        keys = self->keys()
        idx=where( keys  eq key, cnt)
        if cnt gt 0 then begin
            message, 'Cannot duplicate element '+string(strtrim(key,2))+' : '+string(strtrim(value,2)) ;, /info
            return
        endif
        ;endif
        if self->count() eq self->allocated_size() then begin
            ; allocate new arrays with size increased of a chunk
            chunk_size = self->allocated_size() lt 65536L ? self->allocated_size()  : 65536L
            ; add key
            keys = self->keys()
            ptr_free, self._keys
            self._keys = ptr_new( [keys, replicate(key,chunk_size)], /no_copy )
            ; now add value
            values = self->values()
            ptr_free, self._values
            self._values = ptr_new( [values, replicate(value, chunk_size)], /no_copy)
            self._allocated_size += chunk_size
        endif else begin
            (*self._keys)[self->count()] = key
            (*self._values)[self->count()] = value
        endelse
        ;; add key
        ;ptr_free, self._keys
        ;self._keys = ptr_new( [keys, key], /no_copy )
        ;; now add value
        ;values = self->values()
        ;ptr_free, self._values
        ;self._values = ptr_new( [values, value], /no_copy)
    endif else begin
        self._keys = ptr_new( [key], /no_copy )
        self._values = ptr_new([value], /no_copy)
        self._allocated_size = 1
    endelse
    self._nelems += 1
end

function aodict::where, op, key_value, count
    return, self->where_generic(self->keys(), op, key_value, count)
end

function aodict::where_value, op, value, count
    catch, error
    if error eq 0 then begin
        return, self->where_generic(self->values(), op, value, count)
    endif else begin
        catch, /cancel
        count = 0
        return, -1
    endelse
end

function aodict::where_generic, set, op, value, count
    if n_elements(set) eq 0 then begin
        count = 0
        return, -1
    endif
    case op of
        'eq' : idx=where( set eq value, count)
        'ne' : idx=where( set ne value, count)
        'le' : idx=where( set le value, count)
        'lt' : idx=where( set lt value, count)
        'ge' : idx=where( set ge value, count)
        'gt' : idx=where( set gt value, count)
        'in' : begin
            idx1=where( set le value[1], count1)
            idx2=where( set ge value[0], count2)
            if ( (count1 eq 0) or (count2 eq 0) ) then begin
                count = 0
            endif else begin
                dum = intersection(idx1, idx2, idx)
                count = n_elements(idx)
            endelse
            end
        'like' : begin
            if  test_type(set[0], /string) ne 0 then begin
                count = 0
                return, -1
            endif
            idx = where(strmatch(set, "*"+value+"*", /fold_case) eq 1, count)
            end
        else : begin
            message, 'undefined operator '+op, /info
            count = 0
            return, -1
            end
    endcase
    if count gt 0 then return, idx else return, -1
end



;+
; :Description:
;   Retrieves values matching conditions specified by op and key_value. See aodict::where
;
; :Params:
;    op
;    key_value
;    count
;
;  :Return:
;    Array of keys whose value is 'op' to 'value'
;
; :Author: lbusoni
;-
function aodict::select, op, value, count
    idx = self->where_value(op, value, count)
    if count eq 0 then begin
        return, -1
    end
    return, (self->keys())[idx]
end

;+
; :Description:
;    Return the values of the dictionary
;
;
; :Author: lbusoni
;-
function aodict::values
    if self->count() eq 0 then message, 'empty dictionary'
    return, (*self._values)[0:self->count()-1]
end

function aodict::keys
    if self->count() eq 0 then message, 'empty dictionary'
    return, (*self._keys)[0:self->count()-1]
end

function aodict::values_type
    return, self._values_type
end

function aodict::keys_type
    return, self._keys_type
end

function aodict::count
    return, self._nelems
end

function aodict::allocated_size
    return, self._allocated_size
end

pro aodict::remove, key
    if self->count() eq 0 then return
    for j=0L, n_elements(key)-1 do begin
        idx=self->where('eq', key[j], count)
        if count gt 0 then begin
            ; remove key
            keys = self->keys()
            ptr_free, self._keys
            if self->count() gt 1 then begin
                if idx eq 0 then newkeys = keys[1:*] else if idx eq self->count()-1 then newkeys = keys[0:self->count()-2] else newkeys = [ keys[0:idx-1], keys[idx+1:*] ]
                self._keys = ptr_new( newkeys, /no_copy )
            endif
            ; now remove value
            vals = self->values()
            ptr_free, self._values
            if self->count() gt 1 then begin
                if idx eq 0 then newvals = vals[1:*] else if idx eq self->count()-1 then newvals = vals[0:self->count()-2] else newvals = [ vals[0:idx-1], vals[idx+1:*] ]
                self._values = ptr_new( newvals, /no_copy)
            endif
            ; decrement counter
            self._nelems -= 1
            ; update allocated size
            self._allocated_size = self._nelems
        endif
    endfor
end

pro aodict::remove_value, value
    if self->count() eq 0 then return
    for j=0L, n_elements(value)-1 do begin
        idx=self->where_value('eq', value[j], count)
        if count gt 0 then begin
            ; remove key
            keys = self->keys()
            ptr_free, self._keys
            if idx eq 0 then newkeys = keys[1:*] else if idx eq self->count()-1 then newkeys = keys[0:self->count()-1] else newkeys = [ keys[0:idx-1], keys[idx+1:*] ]
            self._keys = ptr_new( newkeys, /no_copy )
            ; now remove value
            vals = self->values()
            ptr_free, self._values
            if idx eq 0 then newvals = vals[1:*] else if idx eq self->count()-1 then newvals = vals[0:self->count()-1] else newvals = [ vals[0:idx-1], vals[idx+1:*] ]
            self._values = ptr_new( newvals, /no_copy)
            ; decrement counter
            self._nelems -= 1
        endif
    endfor

end

;function aodict::allow_multiple_values
;    return, self._multiple
;end

pro aodict::Cleanup
    ptr_free, self._keys
    ptr_free, self._values
end

pro aodict__define
    struct = { aodict, $
        _keys           : ptr_new() , $
        _values         : ptr_new() , $
        _keys_type      : "", $
        _values_type    : "", $
        _nelems         : 0L, $
        _allocated_size : 0L  $
        ;_multiple       : 0B $
    }
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro test_aodict
    dict=obj_new('aodict', 'real', 'integer')
    nelem = 10000L
    randi = randomu(seed, nelem)*10
    ;randi = findgen(nelem)*10
    t0=systime(/sec)
    for i=0L, nelem-1 do begin
        if  (i+1) mod 10000 eq 0  then print, i
        dict->insert, randi[i] ,i
    endfor
    t1=systime(/sec)
    print, format='(%"insert took %9.6f sec (%9.3g record/sec)")', t1-t0, double(nelem)/(t1-t0)
    ; select
    t0=systime(/sec)
    subset= dict->select('in',[0,0.1])
    t1=systime(/sec)
    print, format='(%"select took %9.6f sec (tested %9.3g record/sec)")', t1-t0, double(nelem)/(t1-t0)
    ;print, dict->keys()
    ;print, dict->values()
    stop
    obj_destroy, dict
end

