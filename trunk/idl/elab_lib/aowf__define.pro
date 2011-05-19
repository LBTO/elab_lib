;+
;
;-

function AOwf::Init, root_obj, modeShapes, modeCoeffs
	if obj_valid(modeShapes) then self._modeShapes = modeShapes else $
		return,0
	if obj_valid(modeCoeffs) then self._modeCoeffs = modeCoeffs else $
		return, 0
	self._reflcoef = root_obj->reflcoef()

	;sav file
	savfile = strmid(strlowcase(obj_class(self._modeCoeffs)),2)+'_wf.sav'
    self._store_fname = filepath(root=root_obj->elabdir(), savfile)

	return,1
end

;+
; Computes the shapes in m surf
;-
function AOwf::surfmat
	if file_test(self._store_fname) then begin
		restore, self._store_fname
	endif else begin
		nmodes  = self._modeCoeffs->nmodes()
		shapes  = self._modeShapes->modemat(mode_idx=lindgen(nmodes)) ## *(self._modeCoeffs->getdati())
		save, shapes, filename=self._store_fname, /compress
	endelse
	return, shapes
end

;+
; Computes the wf for a given lambda
;-
function AOwf::wfmat, lambda=lambda
	if not keyword_set(lambda) then lambda=750e-9	;WFS central wavelength
	fact = self._reflcoef * (2*!PI) / lambda 	;to convert meters surface to radians wf.
	wfmat = temporary(self->surfmat()) * fact
	return, wfmat
end


pro AOwf__define
    struct = { AOwf, $
        _modeShapes				: obj_new()		, $
        _modeCoeffs				: obj_new()		, $
        _reflcoef			    : 0.			, $
        _store_fname			: ""			, $
        INHERITS AOhelp $
    }
end
