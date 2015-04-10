
;+
;
;-

function AOm2c::Init, fname, recompute=recompute

    self._m2c_fname  = ao_datadir()+path_sep()+fname

    if not file_test(self._m2c_fname) then begin
        message, 'M2C file '+self._m2c_fname+' not existing', /info
        return, 0
    endif

    header = headfits(self._m2c_fname, /SILENT, errmsg=errmsg)
    if errmsg ne '' then message, self._m2c_fname+ ': '+ errmsg, /info
    self._m2c_fname_fitsheader = ptr_new(header, /no_copy)

    store_c2m_dir = filepath(root=ao_elabdir(), sub=[file_dirname(fname)], '')
    if file_test(store_c2m_dir, /dir) eq 0 then begin
        file_mkdir,  store_c2m_dir
        file_chmod,  store_c2m_dir, /a_read, /a_write, /g_read, /g_write
    endif
    self._store_c2m_dir = store_c2m_dir

; [TODO] we need a different recompute flag as oppsed to the global one
;    if keyword_set(recompute) then begin
;        c2m_files = file_search(store_c2m_dir, 'c2m*.sav', count=c2m_files_count)
;        if c2m_files_count gt 0 then file_delete, c2m_files, /allow_nonexistent
;    endif

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOm2c', 'Represent an object to convert from modal to zonal basis') then return, 0
    self->addMethodHelp, "fname()",   "fitsfile name (string)"
    self->addMethodHelp, "header()",  "header of fitsfile (strarr)"
    self->addMethodHelp, "m2c()",     "m2c matrix (modal->zonal))"
;    self->addMethodHelp, "c2m()",     "c2m matrix (zonal->modal))"
    return, 1
end

function AOm2c::fname
    return, self._m2c_fname
end

function AOm2c::m2c
    if not ptr_valid (self._m2c) then self._m2c = ptr_new(readfits(self._m2c_fname, /SILENT), /no_copy)
    return, *self._m2c
end

function AOm2c::c2m, act_w_cl=act_w_cl
    if n_elements(act_w_cl) eq 0 then act_w_cl = indgen(672)  ;per retrocompatibilita'....
    checksum32, act_w_cl, c2m_ID
    self._store_c2m_fname = filepath(root=self._store_c2m_dir,'c2m_'+strtrim(c2m_ID,2)+'.sav')
    if file_test(self._store_c2m_fname) then begin
        restore, self._store_c2m_fname
    endif else begin
        m2c = self->m2c()
        idx_valid_modes = where(total(m2c,2) ne 0, nmodes_max)
        m2c = m2c[idx_valid_modes,*]
        sz = size(m2c,/dim)
        c2m = make_array(sz[1],sz[0],type=size(m2c,/type))
        
        m2c = m2c[*,act_w_cl]
        c2m_temp = pseudo_invert(m2c)
        c2m[act_w_cl,*] = c2m_temp
        
        save, c2m, file=self._store_c2m_fname
    endelse
    if not ptr_valid (self._c2m) then self._c2m = ptr_new(c2m, /no_copy)
    return, *self._c2m
end

function AOm2c::header
    if ptr_valid(self._m2c_fname_fitsheader) then return, *(self._m2c_fname_fitsheader) else return, ""
end

pro AOm2c::free
    ;if ptr_valid(self._m2c_fname_fitsheader) then ptr_free, self._m2c_fname_fitsheader
    if ptr_valid(self._m2c) then ptr_free, self._m2c
    if ptr_valid(self._c2m) then ptr_free, self._c2m
end

pro AOm2c::Cleanup
    ptr_free, self._m2c_fname_fitsheader
    ptr_free, self._m2c
    ptr_free, self._c2m
    self->AOhelp::Cleanup
end

pro AOm2c__define
    struct = { AOm2c, $
        _m2c_fname                     : "", $
        _store_c2m_fname               : "", $
        _store_c2m_dir                 : "", $
        _m2c_fname_fitsheader          : ptr_new(), $
        _m2c                           : ptr_new(), $
        _c2m                           : ptr_new(), $
        INHERITS AOhelp $
    }
end


