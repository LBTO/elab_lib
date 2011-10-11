;+
;
;-

function AOmodeShapes::Init, fname
	self._sav_file = fname
	if not file_test(fname) then return,0
	self._Dpix   = -1L
	self._nmodes = -1L

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOmodeShapes', 'Represent the shapes of a set of modes') then return, 0
    self->addMethodHelp, "fname()",   "mode shape matrix file name (string)"
    self->addMethodHelp, "nmodes()", "number of modal shapes"
    self->addMethodHelp, "modemat([mode_idx=mode_idx, anglerot=anglerot, shiftval=shiftval])", "matrix of modal shapes [npix x nmodes]"
    self->addMethodHelp, "idx_mask()", "index vector of pupil mask points  [npix]"
    self->addMethodHelp, "mask()", "pupil mask [npix x npix]"
    self->addMethodHelp, "xx()", "x-coord of pupil mask points  [npix]"
    self->addMethodHelp, "yy()", "y-coord of pupil mask points  [npix]"
    self->addMethodHelp, "Dpix()", "Diameter in pixels of modal shapes"
	return, 1
end

pro AOmodeShapes::restore_sav
	restore, self->fname()
	self._modemat  = ptr_new(float(KLmatrix), /no_copy)
	self._idx_mask = ptr_new(idx_mask, /no_copy)
	self._xx   = ptr_new(float(XX), /no_copy)
	self._yy   = ptr_new(float(YY), /no_copy)
	self._Dpix = Dpix
	self._nmodes = (size(*self._modemat,/dim))[0]
end


;+
; This function returns the matrix of modes.
; If requested, the phase maps of the modes can be rotated and/or shifted.
;
; mode_idx  index vector to wanted modes
; anglerot	angle of rotation (degrees clockwise)
; shiftval  vector of two elements [shift_x,shift_y] in percent of pupil size
;
;-
function AOmodeShapes::modemat, mode_idx=mode_idx, anglerot=anglerot, shiftval=shiftval
	if not ptr_valid(self._modemat) then self->restore_sav

	;subset of modes:
	if n_elements(mode_idx) ne 0 then begin
		if max(mode_idx) gt self->nmodes()-1 then message, 'Mode index out of range'
		modemat = (*self._modemat)[mode_idx, *]
	endif else modemat = *self._modemat

	;shifts and anglerot handling
	if n_elements(anglerot) eq 0 then anglerot = 0.
	if n_elements(shiftval) eq 0 then shiftval = [0., 0.]
    if n_elements(shiftval) ne 2 then message, 'SHIFTVAL must be of the form: [xshift,yshift]'
	shiftPix  = shiftval/100.*self->Dpix()		;convert shifts in number of pixels.
	if keyword_set(anglerot) or (total(shiftval NE fltarr(2))) then begin
        nbmot = (size(modemat,/dim))[0]
        map = make_array(size=size(self->mask()))
        for kkk=0, nbmot-1 do begin
            map[self->idx_mask()] = modemat[kkk,*]
            map = rot_and_shift_image(map, anglerot, shiftPix, mask=self->mask(), /interp)
            modemat[kkk,*] = map[self->idx_mask()]
        endfor
	endif
	return, modemat
end

function AOmodeShapes::fname
	return, self._sav_file
end

function AOmodeShapes::idx_mask
	if not ptr_valid(self._idx_mask) then self->restore_sav
	return, *self._idx_mask
end

function AOmodeShapes::xx
	if not ptr_valid(self._xx) then self->restore_sav
	return, *self._xx
end

function AOmodeShapes::yy
	if not ptr_valid(self._yy) then self->restore_sav
	return, *self._yy
end

function AOmodeShapes::Dpix
	if self._Dpix eq -1L then self->restore_sav
	return, self._Dpix
end

function AOmodeShapes::nmodes
	if self._nmodes eq -1L then self->restore_sav
	return, self._nmodes
end

function AOmodeShapes::mask
	mask = fltarr(self->Dpix(),self->Dpix())
	mask[self->idx_mask()] = 1.
	return, mask
end

pro AOmodeShapes::free
	if ptr_valid(self._modemat) then ptr_free, self._modemat
	if ptr_valid(self._idx_mask) then ptr_free, self._idx_mask
	if ptr_valid(self._xx) then ptr_free, self._xx
	if ptr_valid(self._yy) then ptr_free, self._yy
end

pro AOmodeShapes::Cleanup
	if ptr_valid(self._modemat) then ptr_free, self._modemat
	if ptr_valid(self._idx_mask) then ptr_free, self._idx_mask
	if ptr_valid(self._xx) then ptr_free, self._xx
	if ptr_valid(self._yy) then ptr_free, self._yy
    self->AOhelp::Cleanup
end


pro AOmodeShapes__define
    struct = { AOmodeShapes, $
        _sav_file                       : ""			, $
        _nmodes                         : 1L			, $
        _Dpix							: 1L			, $
        _modemat						: ptr_new()		, $
        _idx_mask						: ptr_new()		, $
        _xx								: ptr_new()		, $
        _yy								: ptr_new()		, $
        INHERITS AOhelp $
    }
end
