function AOadsec_struct::Init, adsec_struct_fname
  if not file_test(adsec_struct_fname) then begin
        message, adsec_struct_fname + ' not found', /info
        return, 0
  end
  self._fname = adsec_struct_fname
  return, 1
end

function AOadsec_struct::adsec
    if not ptr_valid(self._adsec) then begin
        savObj = obj_new('IDL_Savefile', self._fname)
        savObj->restore, 'myadsec'
        obj_destroy, savObj
        self._adsec = ptr_new(myadsec)
    endif
    return, *self._adsec
end

function AOadsec_struct::adsec_shell
    if not ptr_valid(self._adsec_shell) then begin
        savObj = obj_new('IDL_Savefile', self._fname)
        savObj->restore, 'myadsec_shell'
        obj_destroy, savObj
        self._adsec_shell = ptr_new(myadsec_shell)
    endif
    return, *self._adsec_shell
end

function AOadsec_struct::gr
    if not ptr_valid(self._gr) then begin
        savObj = obj_new('IDL_Savefile', self._fname)
        savObj->restore, 'mygr'
        obj_destroy, savObj
        self._gr = ptr_new(mygr)
    endif
    return, *self._gr
end

function AOadsec_struct::sc
    if not ptr_valid(self._sc) then begin
        savObj = obj_new('IDL_Savefile', self._fname)
        savObj->restore, 'mysc'
        obj_destroy, savObj
        self._sc = ptr_new(mysc)
    endif
    return, *self._sc
end

function AOadsec_struct::act_coordinates
    stru = self->adsec()
    return, stru.act_coordinates
end

function AOadsec_struct::act_wo_cl
    stru = self->adsec()
    return, stru.act_wo_cl
end

function AOadsec_struct::act_w_cl
  stru = self->adsec()
  return, stru.act_w_cl
end

pro AOadsec_struct::free
    if ptr_valid(self._adsec) then ptr_free, self._adsec
    if ptr_valid(self._adsec_shell) then ptr_free, self._adsec_shell
    if ptr_valid(self._gr) then ptr_free, self._gr
    if ptr_valid(self._sc) then ptr_free, self._sc
end


pro AOadsec_struct__define
    struct = { AOadsec_struct, $
        _fname               : "",        $
        _adsec               : ptr_new(), $
        _adsec_shell         : ptr_new(), $
        _gr                  : ptr_new(), $
        _sc                  : ptr_new(),  $
        INHERITS AOhelp $
    }
end
