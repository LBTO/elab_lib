
;+
;
;-

function AOcontrol::Init, root_obj, b0_a_fname, a_delay_fname, b_delay_a_fname, c_fname, gain_fname
    self._root_obj = root_obj
    self._b0_a_fname = b0_a_fname
    self._a_delay_fname = a_delay_fname
    self._b_delay_a_fname = b_delay_a_fname
    self._c_fname  = c_fname
    self._gain_fname  = gain_fname

	if self->b0_a_fname() ne "" then begin
    	header = headfits(ao_datadir()+path_sep()+self->b0_a_fname(), /SILENT)
    	self._b0_a_fitsheader = ptr_new(header, /no_copy)
    endif
    if self->c_fname() ne "" then begin
    	header = headfits(ao_datadir()+path_sep()+self->c_fname(), /SILENT)
    	self._c_fitsheader = ptr_new(header, /no_copy)
    endif
    if self->gain_fname() ne "" then begin
    	header = headfits(ao_datadir()+path_sep()+self->gain_fname(), /SILENT)
    	self._gain_fitsheader = ptr_new(header, /no_copy)
    endif

    self._kalman     = aoget_fits_keyword(self->b0_a_header(), 'KFMODES') eq '' ? 0B : 1B

    if self->isKalman() then begin
        self._intmat_fname = aoget_fits_keyword(self->b0_a_header(), 'INTMAT')
        self._m2c_fname    = aoget_fits_keyword(self->b0_a_header(), 'M2C')
    endif else begin
    	temp = strsplit(self._b0_a_fname,'Rec_', /extract, /regex)
		if n_elements(temp) eq 2 then $
        	self._intmat_fname = temp[0]+'Intmat_'+temp[1]
        self._m2c_fname    = self._c_fname
    endelse

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOcontrol', 'Represents AO control filter') then return, 0
    self->addMethodHelp, "b0_a_fname()", "b0_a filename (string)"
    self->addMethodHelp, "b_delay_a_fname()", "b_delay_a filename (string)"
    self->addMethodHelp, "a_delay_fname()", "a_delay filename (string)"
    self->addMethodHelp, "c_fname()", "c filename (string)"
    self->addMethodHelp, "gain_fname()", "gain filename (string)"
    self->addMethodHelp, "b0_a()", "b0_a matrix"
    self->addMethodHelp, "b_delay_a()", "b_delay_a matrix"
    self->addMethodHelp, "a_delay()", "a_delay matrix"
    self->addMethodHelp, "m2c_fname()", "m2c matrix filename (tipically = c_fname) (string)"
    self->addMethodHelp, "m2c()", "m2c matrix (modal -> zonal) "
    self->addMethodHelp, "c2m()", "c2m matrix (zonal -> modal) "
    self->addMethodHelp, "gain()", "gain vector"
    self->addMethodHelp, "nmodes()", "number of non-null row in b0_a matrix"
    self->addMethodHelp, "modes_idx()", "index vector of non-null row in b0_a matrix"
    self->addMethodHelp, "rec()", "reconstructor matrix (not b0_a() in case of Kalman filter)"
    self->addMethodHelp, "b0_a_header()",     "header of b0_a fitsfile (strarr)"
    self->addMethodHelp, "c_header()",      "header of c fitsfile (strarr)"
    ;self->addMethodHelp, "m2c_header()", "header of m2c fitsfile (tipically = c_header) (strarr)"
    self->addMethodHelp, "gain_header()",     "header of gain fitsfile (strarr)"
    self->addMethodHelp, "isKalman()",     "tell if Kalman filter is used"
    self->addMethodHelp, "intmat_fname()",     "interaction matrix filename (from which b0_a has been derived)"
    return, 1
end

;function AOcontrol::FitsHeader
;   if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, 0d
;end

function AOcontrol::b0_a_fname
    return, self._b0_a_fname
end

function AOcontrol::a_delay_fname
    return, self._a_delay_fname
end

function AOcontrol::b_delay_a_fname
    return, self._b_delay_a_fname
end

function AOcontrol::c_fname
    return, self._c_fname
end

function AOcontrol::m2c_fname
    return, self._m2c_fname
end

function AOcontrol::gain_fname
    return, self._gain_fname
end

function AOcontrol::b0_a
    _b0_a = readfits(ao_datadir()+path_sep()+self->b0_a_fname(), /SILENT)
    if not ptr_valid(self._modes_idx) then self._modes_idx = ptr_new(where(total(_b0_a,1) ne 0, t_nmodes), /no_copy)
    if t_nmodes eq 0 then message, 'Matrix b0_a is null'
    self._nmodes = t_nmodes
    return, _b0_a
end

function AOcontrol::a_delay
    return, readfits(ao_datadir()+path_sep()+self->a_delay_fname(), header, /SILENT)
end

function AOcontrol::b_delay_a
    return, readfits(ao_datadir()+path_sep()+self->b_delay_a_fname(), header, /SILENT)
end

function AOcontrol::m2c
    m2cobj = getm2c(self->m2c_fname(), recompute= self._root_obj->recompute() )
    if OBJ_VALID(m2cobj) then return, m2cobj->m2c() else return, -1
    ;return, readfits(ao_datadir()+path_sep()+self->m2c_fname(), header, /SILENT)
end

function AOcontrol::c2m
    m2cobj = getm2c(self->m2c_fname(), recompute= self._root_obj->recompute() )
    if OBJ_VALID(m2cobj) then return, m2cobj->c2m() else return, -1
end

function AOcontrol::gain
    if not ptr_valid(self._gain) then begin
        gain_v = readfits(ao_datadir()+path_sep()+self->gain_fname(), header, /SILENT)
        if max(gain_v)-min(gain_v) eq 0. then gain_v = gain_v[0]
        self._gain = ptr_new(gain_v, /no_copy)
    endif
    return, *self._gain
end

; number of non-null rows in b0_a matrix
function AOcontrol::nmodes
    if (self._nmodes eq 0L) then r=self->b0_a()
    return, self._nmodes
end

; indexes of non-null rows in rec matrix
function AOcontrol::modes_idx
    if not ptr_valid(self._modes_idx) then r=self->b0_a()
    if (PTR_VALID(self._modes_idx)) THEN return, *(self._modes_idx) else return, 0d
end

; alias
function AOcontrol::rec
    return, self->b0_a()
end

;function AOadsec_status::gain, header=header
;    return, readfits(!ao_env.root+path_sep()+self->g_gain_a_fname(), header, /SILENT)
;    ;; eventually store it into self._g_gain_a
;end

function AOcontrol::b0_a_header
    if ptr_valid(self._b0_a_fitsheader) then return, *(self._b0_a_fitsheader) else return, ""
end

function AOcontrol::c_header
    if ptr_valid(self._c_fitsheader) then return, *(self._c_fitsheader) else return, ""
end

function AOcontrol::gain_header
    if ptr_valid(self._gain_fitsheader) then return, *(self._gain_fitsheader) else return, ""
end

function AOcontrol::isKalman
    return, self._kalman
end

function AOcontrol::intmat_fname
    return, self._intmat_fname
end

pro AOcontrol::Cleanup
    ptr_free, self._b0_a_fitsheader
    ptr_free, self._c_fitsheader
    ptr_free, self._gain_fitsheader
    ptr_free, self._modes_idx
    ptr_free, self._gain
    self->AOhelp::Cleanup
end

pro AOcontrol__define
    struct = { AOcontrol, $
        _root_obj                 : obj_new(), $
        _b0_a_fname               : "", $
        _a_delay_fname            : "", $
        _b_delay_a_fname          : "", $
        _c_fname                  : "", $
        _m2c_fname                : "", $
        _gain_fname               : "", $
        _b0_a_fitsheader         : ptr_new(), $
        _c_fitsheader            : ptr_new(), $
        _gain_fitsheader         : ptr_new(), $
        _nmodes                  : 0L, $
        _modes_idx               : ptr_new(), $
        _gain                    : ptr_new(), $
        _kalman                  : 0B, $
        _intmat_fname            : "", $
        INHERITS AOhelp $

    }
end

