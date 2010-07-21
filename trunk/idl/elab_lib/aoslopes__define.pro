
;+
;
;-

function AOslopes::Init, root_obj, slopes_file, fc_obj
	if not file_test(slopes_file) then return,0
    self._file = slopes_file
    self._fc_obj = fc_obj
	self._wfs_status = root_obj->wfs_status()
    self._fitsheader = ptr_new(headfits(self._file ,/SILENT), /no_copy)
    self._store_fname = filepath(root=root_obj->elabdir(), 'slopes.sav')
    self._store_psd_fname = filepath(root=root_obj->elabdir(), 'slopes_psd.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._store_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
    endif

    if not self->AOtime_series::Init(fc_obj->deltat(), fftwindow="hamming", nwindows=root_obj->n_periods()) then return,0
	self._spectra_units = textoidl('[slope units Hz^{-1/2}]')
	self._plots_title = root_obj->tracknum()

    ;self->datiProducer
    ;self->AOtime_series::Compute

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOslopes', 'Represent measured slopes') then return, 0
    self->addMethodHelp, "fname()", "fitsfile name (string)"
    self->addMethodHelp, "header()", "header of fitsfile (strarr)"
    self->addMethodHelp, "slopes()", "return slopes matrix [nslopes,niter]"
    self->addMethodHelp, "nslopes()", "return number of slopes"
    self->addMethodHelp, "niter()", "return number of iteration (eventually after reforming using lost frames infos)"

    return, 1
end

pro AOslopes::datiProducer
    if file_test(self._store_fname) then begin
        restore, self._store_fname
    endif else begin
        slopes = readfits(self._file, /SILENT)
        slopes = transpose(temporary(slopes))
        slopes  = interpolate_with_frames_counter(slopes, self._fc_obj)

        save, slopes, file=self._store_fname
    endelse
    self._slopes = ptr_new(slopes, /no_copy)

end


function AOslopes::fname
    return, self._file
end

function AOslopes::Header
    if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, 0d
end

function AOslopes::Slopes
    slopesPtr = self->GetDati()
    return, *(slopesPtr)
end

function AOslopes::NSlopes
    return, self->AOtime_series::nseries()
end

function AOslopes::NIter
    return, self->AOtime_series::niter()
end

; to be implemented in AOtime_series subclasses
function AOslopes::GetDati
    if not ptr_valid(self._slopes) then self->datiProducer
    return, self._slopes
end

function AOslopes::wfs_status
	return, self._wfs_status
end

; returns Sx
function AOslopes::sx, slvec
	nsub = ((self->wfs_status())->pupils())->nsub()
	sx = slvec[0:nsub*2-1]
	sx = sx[0:*:2]
	return, sx
end

; returns Sy
function AOslopes::sy, slvec
	nsub = ((self->wfs_status())->pupils())->nsub()
	sy = slvec[0:nsub*2-1]
	sy = sy[1:*:2]
	return, sy
end

; return remapped signal vector
function AOslopes::remap2d, slvec

	mypup = 0	;use this pupil info to remap signals
	indpup = ((self->wfs_status())->pupils())->indpup()
	nsub   = ((self->wfs_status())->pupils())->nsub()
	fr_sz =80/((self->wfs_status())->ccd39())->binning()		;pixels

	cx  = (((self->wfs_status())->pupils())->cx())[mypup]
	cy  = (((self->wfs_status())->pupils())->cy())[mypup]
	rad = (((self->wfs_status())->pupils())->radius())[mypup]
	xr = [floor(cx-rad),ceil(cx+rad)]
	yr = [floor(cy-rad),ceil(cy+rad)]
	im2d_w = xr[1]-xr[0]+1
	im2d_h = yr[1]-yr[0]+1

	sx = self->sx(slvec)
	sy = self->sy(slvec)
	s2d = fltarr(fr_sz,fr_sz)
	im_2d = fltarr(im2d_w*2,im2d_h)
	s2d[indpup[*,mypup]] = sx
	s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	s2d[indpup[*,mypup]] = sy
	s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
	im_2d = [s2d_tmpA,s2d_tmpB]

	return, im_2d
end


pro AOslopes::Free
    ptr_free, self._slopes
end

pro AOslopes::Cleanup
    ptr_free, self._slopes
    ptr_free, self._fitsheader
    self->AOhelp::Cleanup
end

pro AOslopes__define
    struct = { AOslopes, $
        _file             : "", $
        _fitsheader       :  ptr_new(), $
        _slopes           :  ptr_new(), $
        _fc_obj           :  obj_new(), $
        _wfs_status		  :  obj_new(), $
        _store_fname      : "", $
        INHERITS    AOtime_series, $
        INHERITS    AOhelp $
    }
end

