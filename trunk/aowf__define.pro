;+
;
;-

function AOwf::Init, root_obj, modeShapes   ;, modeCoeffs
	if obj_valid(modeShapes) then self._modeShapes = modeShapes else $
		return,0
	;if obj_valid(modeCoeffs) then self._modeCoeffs = modeCoeffs else $
	;	return, 0
	self._reflcoef = root_obj->reflcoef()

	;sav file
	savfile = strmid(strlowcase(obj_class(self)),2)+'_wf.sav'
    self._wf_store_fname = filepath(root=root_obj->elabdir(), savfile)
    if root_obj->recompute() eq 1B then begin
        file_delete, self._wf_store_fname, /allow_nonexistent
    endif

    ; initialize help object and add methods and leafs
    ;if not self->AOhelp::Init('AOwf', 'Represent a WF') then return, 0
    ;self->addMethodHelp, "wfmat(lambda=lambda)", "Matrix containing WFs [npix x niter] [rad] (float). Default lambda: 750nm"
	return,1
end

;+
; Computes the shapes in m surf
;-
function AOwf::surfmat
	if file_test(self._wf_store_fname) then begin
		restore, self._wf_store_fname
	endif else begin
        dati = self->GetDati()
        if test_type(dati, /pointer) ne 0 then message, 'AOwf subclass::GetDati must return a pointer to float 1/2D array'

        ; compute all AOtime_series stuff here
        ss = size(*dati)
        niter   = ss[1]
        nmodes  = ss[0] eq 1 ? 1 : ss[2]
		shapes  = self._modeShapes->modemat(mode_idx=lindgen(nmodes)) ## *(dati) ;; TODO lindgen(nmodes) seems bad: if modes are not consecutives?
		save, shapes, filename=self._wf_store_fname, /compress
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

pro AOwf::addHelp, obj
    obj->addMethodHelp, "wfmat(lambda=lambda)", "Wavefront matrix [niter, npix] [rad]. Default lambda: 750nm"
end

pro AOwf::free
end

pro AOwf::Cleanup
end

pro AOwf__define
    struct = { AOwf, $
        _modeShapes				: obj_new()		, $
        ;_modeCoeffs				: obj_new()		, $
        _reflcoef			    : 0.			, $
        _wf_store_fname			: ""			  $
    ;    INHERITS AOhelp $
    }
end
