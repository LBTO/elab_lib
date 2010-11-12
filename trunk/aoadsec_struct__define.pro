function AOadsec_struct::Init, adsec_struct_fname
  if not file_test(adsec_struct_fname) then begin
        message, adsec_struct_fname + ' not found', /info
        return, 0
  end
  restore, adsec_struct_fname
  self._adsec = ptr_new(adsec)
  self._adsec_shell = ptr_new(adsec_shell)
  self._gr = ptr_new(gr)
  self._sc = ptr_new(sc)
  return, 1
end

function AOadsec_struct::adsec
  return, *self._adsec
end

function AOadsec_struct::adsec_shell
  return, *self._adsec_shell
end

function AOadsec_struct::gr
  return, *self._gr
end

function AOadsec_struct::sc
  return, *self._sc
end

function AOadsec_struct::act_coordinates
  return, (*self._adsec).act_coordinates
end

pro AOadsec_struct__define::free
    if ptr_valid(self._adsec) then ptr_free, self._adsec
    if ptr_valid(self._adsec_shell) then ptr_free, self._adsec_shell
    if ptr_valid(self._gr) then ptr_free, self._gr
    if ptr_valid(self._sc) then ptr_free, self._b0_a_fitsheader
end


pro AOadsec_struct__define
    struct = { AOadsec_struct, $
        _adsec               : ptr_new(), $
        _adsec_shell         : ptr_new(), $
        _gr                  : ptr_new(), $
        _sc                  : ptr_new(),  $
        INHERITS AOhelp $
    }
end