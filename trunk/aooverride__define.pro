
;+
; 
;
;
; USAGE
; catch, errOver
; if errOver ne 0 then catch, /cancel else self._operation_mode = (self->override())->overriden_value('operation_mode')
;
; INTERNALS
; .sav file contains an 'override_str' variable that is a struct containing structures each having a tag 'k' [string] 
;  and a tag 'v' [whatever type]
;-

function AOoverride::Init, fname
    ;if not file_test(fname) then begin
    ;    return,0
    ;endif
    self._fname = fname

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOoverride', 'Represent human-overriden values') then return, 0
    self->addMethodHelp, "fname()", "file name of .sav file containing the overriden values (string)"
    self->addMethodHelp, "overriden_value(property)", "return overriden value for property."
    self->addMethodHelp, "overriden_properties()", "return an array of all the overriden properties"
    self->addMethodHelp, "set_overriden_value, property, overriden_value", "override value for property and save file"
    self->addMethodHelp, "summary", "Summary of override"
    ;self->addMethodHelp, "overriden_operation_mode()", "return operation mode (ON_SKY or RR)"
    return,1
end

pro AOoverride::lazy_init
    if not file_test(self->fname()) then begin
        message, 'not existing override file '+self->fname()
    endif
    ; file must contain an override_str variable
    save_obj = obj_new('IDL_Savefile', self->fname()) 
    dum=where(save_obj->names() eq 'OVERRIDE_STR', cnt)
    if cnt eq 0 then message, 'override file '+self->fname()+' does not contain an override_str variable'
    ; restore
    save_obj->restore, 'OVERRIDE_STR'
    ; check type of override_str
    if size(override_str, /typ) ne 8 then message, 'override file '+self->fname()+' contains unexpected data'
    self._str = ptr_new(override_str, /no_copy) 
end

; getter
function AOoverride::overriden_properties
    catch, errOver
    if errOver ne 0 then begin
        catch, /cancel
        return, ''
    endif else begin
        if not ptr_valid(self._str) then self->lazy_init
        ss = *self._str
        ret = strarr(n_tags(ss))
        for i=0, n_tags(ss)-1 do begin
            ret[i]= ss.(i).k
        endfor
    endelse
    return, ret 
end

function AOoverride::overriden_value, property
    if not ptr_valid(self._str) then self->lazy_init
    ; still unvalid? 
    ss = *self._str
    
    for i=0, n_tags(ss)-1 do begin
        if strcmp(ss.(i).k, property, /fold_case) then return, ss.(i).v
    endfor    
    message, 'property '+property+' is not overriden in file '+self->fname()
end

pro AOoverride::set_overriden_value, property, overriden_value
    if ptr_valid(self._str) then ss = *self._str $ 
    else ss=create_struct('S0', create_struct('k','dummy_property', 'v', 'dummy_value'))

    for i=0, n_tags(ss)-1 do begin
        if strcmp(ss.(i).k, property, /fold_case) then begin 
            ; Check type unconsistencies
            if total(size(ss.(i).v)-size(overriden_value)) ne 0 then begin 
                ; Get rid of bad value, then add a new one 
                newss=create_struct('S0', create_struct('k','dummy_property', 'v', 'dummy_value'))
                count=1
                for  j=1, n_tags(ss)-1 do begin
                    if not strcmp(ss.(j).k, property, /fold_case) then $ 
                    newss=create_struct(newss, 'S'+strtrim(count,2), create_struct('K',ss.(j).k, 'V',ss.(j).v)) 
                    count +=1
                endfor
                newss=create_struct(newss, 'S'+strtrim(count,2), create_struct('K',property, 'V',overriden_value)) 
                ss=newss
            endif else begin
                 ss.(i).v=overriden_value
            endelse
            break
        endif
    endfor 
    ; if properties doesn't exist add it to the struct
    if i eq n_tags(ss) then $
        override_str=create_struct(ss, 'S'+strtrim(n_tags(ss),2), create_struct('k',property, 'V',overriden_value)) $
        else override_str=ss

    message, 'property '+property+' overriden with value '+strjoin(overriden_value, ' ',/sin)+' in file '+$
        file_basename(self->fname())+' You need to recompute the affected TN', /info
    ptr_free, self._str
    self._str = ptr_new(override_str)
    save, override_str, file=self->fname()
end

function AOoverride::fname
    return, self._fname
end

pro AOoverride::summary, COMPREHENSIVE=COMPREHENSIVE
    catch, errOver
    if errOver ne 0 then begin
        catch, /cancel
        return
    endif else begin
        if not ptr_valid(self._str) then self->lazy_init
        ss = *self._str
        ; start from 1, 0 is dummy
        for i=1, n_tags(ss)-1 do begin
            print, string(format='(%"%-30s %s")', ss.(i).k, strjoin(ss.(i).v, ' ',/sin) )
        endfor
    endelse
end

pro AOoverride::Free
end
;
pro AOoverride::Cleanup
    ptr_free, self._str
    self->AOhelp::Cleanup
end

pro AOoverride__define
    struct = { AOoverride, $
        _fname : "",    $
        _str : ptr_new(), $
        INHERITS AOhelp $
    }
end


