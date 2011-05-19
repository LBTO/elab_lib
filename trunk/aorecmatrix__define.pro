
;+
;
;-

function AOrecmatrix::Init, fname
    self._rec_file  = fname

    header = headfits(ao_datadir()+path_sep()+self->fname() ,/SILENT, errmsg=errmsg)
    if errmsg ne '' then message, ao_datadir()+path_sep()+self->fname()+ ': '+ errmsg, /info

    self._nmodes = -1
    self._lastmode = -1
	self._filt_modes_svd = long(aoget_fits_keyword(header, 'CT_MODES'))

    self._rec_file_fitsheader = ptr_new(header, /no_copy)

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOrecmatrix', 'Represent a reconstruction matrix R') then return, 0
    self->addMethodHelp, "fname()",   "fitsfile name (string)"
    self->addMethodHelp, "header()",     "header of fitsfile (strarr)"
    self->addMethodHelp, "rec()", "reconstruction matrix"
    self->addMethodHelp, "nmodes()", "number of non-null row in rec matrix"
    self->addMethodHelp, "lastmode()", "index number of last reconstructed mode"
    self->addMethodHelp, "modes_idx()", "index vector of non-null row in rec matrix"
    self->addMethodHelp, "nslopes()", "number of non-null columns in rec matrix"
    self->addMethodHelp, "slopes_idx()", "index vector of non-null columns in rec matrix"
    self->addMethodHelp, "num_svd_filt_modes()", "number of filtered modes in the SVD inversion"
    return, 1
end

function AOrecmatrix::fname
    return, self._rec_file
end

function AOrecmatrix::rec
    rec = readfits(ao_datadir()+path_sep()+self->fname(), /SILENT)
    if not ptr_valid(self._modes_idx) then begin
    	self._modes_idx = ptr_new(where(total(rec^2.,1) ne 0, t_nmodes), /no_copy)
    	if t_nmodes eq 0 then message, 'Null rec matrix '+self->fname()
    	self._nmodes   = t_nmodes
    	self._lastmode = max(*self._modes_idx)
    endif
    if not ptr_valid(self._slopes_idx) then begin
       	self._slopes_idx = ptr_new(where(total(rec^2.,2) ne 0, t_nslopes), /no_copy)
    	if t_nslopes eq 0 then message, 'Null im matrix '+self->fname()
    	self._nslopes = t_nslopes
    endif
    return, rec
end

; number of non-null rows (modes) in rec matrix
function AOrecmatrix::nmodes
    if (self._nmodes eq -1) then r=self->rec()
    return, self._nmodes
end

; index of last reconstructed mode
; NOTE: It may be the case that nmodes < lastmode if there are particular modes
;       removed from the rec matrix.
function AOrecmatrix::lastmode
	if (self._lastmode eq -1) then r=self->rec()
	return, self._lastmode
end

; indexes of non-null rows (modes) in rec matrix
function AOrecmatrix::modes_idx
    if not ptr_valid(self._modes_idx) then r=self->rec()
    if (PTR_VALID(self._modes_idx)) THEN return, *(self._modes_idx) else return, 0d
end

; number of non-null columns (slopes) in rec matrix
function AOrecmatrix::nslopes
    if (self._nslopes eq -1) then r=self->rec()
    return, self._nslopes
end

; indexes of non-null columns (slopes) in rec matrix
function AOrecmatrix::slopes_idx
    if not ptr_valid(self._slopes_idx) then r=self->rec()
    if (PTR_VALID(self._slopes_idx)) THEN return, *(self._slopes_idx) else return, 0d
end


function AOrecmatrix::header
    if ptr_valid(self._rec_file_fitsheader) then return, *(self._rec_file_fitsheader) else return, ""
end

function AOrecmatrix::num_svd_filt_modes
	return, self._filt_modes_svd
end

pro AOrecmatrix::free
    if ptr_valid(self._modes_idx) then ptr_free, self._modes_idx
    if ptr_valid(self._slopes_idx) then ptr_free, self._slopes_idx
end

pro AOrecmatrix::Cleanup
    if ptr_valid(self._modes_idx) then ptr_free, self._modes_idx
    if ptr_valid(self._slopes_idx) then ptr_free, self._slopes_idx
    if ptr_valid(self._rec_file_fitsheader) then ptr_free, self._rec_file_fitsheader
    self->AOhelp::Cleanup
end

pro AOrecmatrix__define
    struct = { AOrecmatrix, $
        _rec_file                          : ""			, $
        _rec_file_fitsheader               : ptr_new()	, $
        _nmodes                            : 1L			, $
        _lastmode						   : 1L			, $
        _modes_idx                         : ptr_new()	, $
        _nslopes						   : 1L			, $
        _slopes_idx						   : ptr_new()	, $
        _filt_modes_svd					   : 1L			, $
        INHERITS AOhelp $
    }
end


