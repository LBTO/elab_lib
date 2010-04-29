
function AOhelp::Init, objname, objdescr
    self._objname       = objname
    self._objdescr      = objdescr
    self._leafs         = obj_new('IDL_Container')
    self._methods_help  = obj_new('IDL_Container')
    return, 1
end

pro AOhelp::printhelp, syntax, descr, indent, style=style
    if n_elements(style) ne 0 then usestyle=style else usestyle='method'
    case usestyle of
        'leaf'   : begin & spacer='***' & col2pos=50  & end
        'method' : begin & spacer='---' & col2pos=60  & end
    endcase 
    cmd1=strjoin([ indent gt 0 ? replicate(spacer,indent) : "", " ", syntax])
    cmd2=strjoin( [": ", descr])
    cmd = strjoin(replicate(" ", 130))
    strput, cmd, cmd1, 0
    strput, cmd, cmd2, col2pos
    print, cmd
end

pro AOhelp::info
    help, self, /object
end

pro AOhelp::help, indent=indent
    if n_elements(indent) eq 0 then indent=1
    
    ;cmd1=strjoin([ '*** ', self._objname,' ***'])
    ;cmd2=strjoin( [": ", self._objdescr])
    ;cmd = strjoin(replicate(" ", 130))
    ;strput, cmd, cmd1, 0
    ;strput, cmd, cmd2, 50
    ;print, cmd
    self->AOhelp::printhelp, self._objname, self._objdescr, indent, style='leaf'
    ; print methods description
    if obj_valid(self._methods_help) then begin
        for i=0L, self._methods_help->Count()-1 do begin
            meth_help = self._methods_help->Get(pos=i)
            self->AOhelp::printhelp, meth_help->syntax(), meth_help->descr(), indent 
        endfor
    endif

    ; go down in tree
    if obj_valid(self._leafs) then begin
        for i=0L, self._leafs->Count()-1 do begin
            (self._leafs->Get(pos=i))->AOhelp::help, indent=indent+1
        endfor
    endif
end

pro AOhelp::addMethodHelp, syntax, description
    tmp = obj_new('AOmethodhelp', syntax, description) 
    self._methods_help->add, tmp
end

pro AOhelp::addleaf, leaf
    if obj_isa(leaf, 'AOhelp') then self._leafs->add, leaf
end

pro AOhelp::Cleanup
    obj_destroy, self._leafs
    obj_destroy, self._methods_help
end

pro AOhelp__define
    struct = { AOhelp,               $
        _objname  :       "",        $
        _objdescr :       "",        $
        _leafs :          obj_new(), $
        _methods_help:    obj_new()  $
    }
end
