
function AOhelp::Init, objname, objdescr
    self._objname       = objname
    self._objdescr      = objdescr
    self._leafs         = obj_new('IDL_Container')
    self._methods_help  = obj_new('IDL_Container')
    self->setHowDoTheyCallMe, 'ee'
    return, 1
end

pro AOhelp::setHowDoTheyCallMe, leaf_call
    self._objcall = leaf_call
end

function AOhelp::howDoTheyCallMe
    return, self._objcall
end

function AOhelp::methodsHelp, leafs=leafs
    if not keyword_set(leafs) or (self._leafs->Count() eq 0) then return, self._methods_help

    ret = obj_new('IDL_Container')
    for i=0L, self._leafs->Count()-1 do begin
        cmdleaf = (self._leafs->Get(pos=i))->methodsHelp(/leafs)
        help,cmdleaf
        help,cmdleaf->Count()
        for j=0L, cmdleaf->Count()-1 do begin
            ret->add, cmdleaf->Get(pos=j)
        endfor
    endfor
    return, ret
end


function AOhelp::fmthelp, syntax, descr, indent, style=style, root=root
    if n_elements(style) ne 0 then usestyle=style else usestyle='method'
    case usestyle of
        'leaf'   : begin & spacer='***' & col2pos=50  & end
        'method' : begin & spacer='---' & col2pos=60  & end
    endcase
    if keyword_set(root) then begin
	if root eq 'ee' and self->howDoTheyCallMe() eq 'ee' then cmd1 = self->howDoTheyCallMe()+"->"+syntax else $
        cmd1="("+root+"->"+self->howDoTheyCallMe()+")->"+syntax
    endif else begin
        cmd1=strjoin([ indent gt 0 ? replicate(spacer,indent) : "", " ", syntax])
    endelse
    cmd2=strjoin( [": ", descr])
    cmd = strjoin(replicate(" ", 130))
    strput, cmd, cmd1, 0
    strput, cmd, cmd2, col2pos
    return, cmd
end

pro AOhelp::printhelp, syntax, descr, indent, style=style
    stringa = self->AOhelp::fmthelp(syntax, descr, indent, style=style)
    print, stringa
end

function AOhelp::cmdlist,root=root, noleafs=noleafs
    ;if not keyword_set(root) then root="ee"
    cmdlista = ['']
    if obj_valid(self._methods_help) then begin
        for i=0L, self._methods_help->Count()-1 do begin
            meth_help = self._methods_help->Get(pos=i)
	    sroot=  n_elements(root) ne 0  ? root : self->howDoTheyCallMe()
            cmdlista = [temporary(cmdlista), self->AOhelp::fmthelp(meth_help->syntax(), meth_help->descr(), 0, root=sroot)]
        endfor
    endif

    ; go down in tree
    if not keyword_set(noleafs) then begin
        if obj_valid(self._leafs) then begin
            for i=0L, self._leafs->Count()-1 do begin
    	    sroot=  n_elements(root) ne 0  ? "("+root+"->"+self->howDoTheyCallMe()+")" : self->howDoTheyCallMe()
                cmdlista = [temporary(cmdlista), (self._leafs->Get(pos=i))->AOhelp::cmdlist(root=sroot)]
            endfor
        endif
    endif
    return, cmdlista
end

pro AOhelp::info
    help, self, /object
end

pro AOhelp::help, keyword, indent=indent

    if n_params() eq 1 and (test_type(keyword, /string) eq 0 ) then begin
        lista = self->cmdlist()
        matched = where(stregex(strlowcase(lista), strlowcase(keyword), /bool) eq 1, cnt)
        if cnt gt 0 then print, lista[matched]
        return
    endif

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

pro AOhelp::addleaf, leaf, leaf_call
    ;leaf->setHowDoTheyCallMe, "("+self->howDoTheyCallMe()+")->"+leaf_call+"()"
    leaf->setHowDoTheyCallMe, leaf_call+"()"
    if obj_isa(leaf, 'AOhelp') then self._leafs->add, leaf
end

pro AOhelp::Cleanup
    ; IMPORTANT: remove leafs from container before destroying it, to avoid destroying objects!!
    if obj_valid(self._leafs) then self._leafs->remove, /all
    obj_destroy, self._leafs
    obj_destroy, self._methods_help
end

pro AOhelp__define
    struct = { AOhelp,               $
        _objname  :       "",        $
        _objdescr :       "",        $
        _objcall  :       "",        $
        _leafs :          obj_new(), $
        _methods_help:    obj_new()  $
    }
end
