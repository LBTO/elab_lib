
function AOpsfcentroid::Init, psf_fname, pixelscale, dark_fname=dark_fname

	if not file_test(psf_fname) then return,0
    self._fname = psf_fname
    self._fitsheader = ptr_new(headfits(self._fname, /SILENT), /no_copy)

    naxis = long(aoget_fits_keyword(self->header(), 'NAXIS'))
    self._frame_w  = long(aoget_fits_keyword(self->header(), 'NAXIS1'))
    self._frame_h  = long(aoget_fits_keyword(self->header(), 'NAXIS2'))
    self._nframes = (naxis eq 2) ? 1 :  long(aoget_fits_keyword(self->header(), 'NAXIS3')) ;TODO

    ; read pixelscale from fits TODO
    self._pixelscale = pixelscale ;   13./1800

    ; TODO WARNING THIS IS GOOD ONLY FOR IRTC, NOT FOR THE CCD47
;    if n_elements(dark_fname) eq 0 then dark_fname   = aoget_fits_keyword(self->header(), 'IRTC.DARK_FILENAME')
;	dark_subdir 		= ['wfs_calib_'+(root_obj->wfs_status())->wunit(),'irtc','backgrounds','bin1'] ; TODO is it always bin1?
;	self._dark_fname = filepath(root=ao_datadir(), sub=dark_subdir,  dark_fname)
	self._dark_fname = dark_fname
    if not file_test(self._dark_fname) then begin
    	message, 'Dark file not existing', /info
    endif else begin
    	dark_fitsheader = headfits(self._dark_fname, /SILENT)
    	naxis = long(aoget_fits_keyword(dark_fitsheader, 'NAXIS'))
    	self._dark_frame_w  = long(aoget_fits_keyword(dark_fitsheader, 'NAXIS1'))
    	self._dark_frame_h  = long(aoget_fits_keyword(dark_fitsheader, 'NAXIS2'))
    	self._dark_nframes = (naxis eq 2) ? 1 :  long(aoget_fits_keyword(dark_fitsheader, 'NAXIS3')) ;TODO
	endelse

	dt = float(aoget_fits_keyword(self->header(), 'EXPTIME'))*1e-6	;in seconds
	if dt ne 0 then self._exptime = dt else begin
		self._exptime = 1
		message, 'PSF image exposure time not known', /info
	endelse

    self._store_psd_fname = filepath(root=getenv('IDL_TMPDIR'), 'psfcentroid_psd.sav')
    if not self->AOtime_series::Init(dt, fftwindow="hamming") then return,0
;	self._norm_factor   = 1e9 * root_obj->reflcoef()	;nm wf
	self._spectra_units = textoidl('[pix Hz^{-1/2}]')
	self._plots_title = 'centroid'

return,1
end

function AOpsfcentroid::fname
    return, self._fname
end

function AOpsfcentroid::dark_fname
    if not file_test(self._dark_fname) then message, 'Dark file not existing', /info
    return, self._dark_fname
end

function AOpsfcentroid::header
    if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, 0d
end

function AOpsfcentroid::nframes
	return, self._nframes
end

function AOpsfcentroid::frame_w
	return, self._frame_w
end

function AOpsfcentroid::frame_h
	return, self._frame_h
end

function AOpsfcentroid::pixelscale
    return, self._pixelscale
end

function AOpsfcentroid::image
    if not (PTR_VALID(self._image)) THEN self->compute
    return, *(self._image)
end

function AOpsfcentroid::dark_image
    if not (PTR_VALID(self._dark_image)) THEN $
    	if file_test(self->dark_fname()) then begin
        	dark = float(readfits( self->dark_fname(), dark_header, /SILENT))
        	if self._dark_nframes gt 1 then self._dark_image = ptr_new(total(dark,3)/self._dark_nframes) else $
        		self._dark_image = ptr_new(dark)
        endif
    return, *(self._dark_image)
end

pro AOpsfcentroid::compute
    psf_fname = self->fname()
    psf = float(readfits( psf_fname, header, /SILENT))
        ; todo check size dark
     dark = self->dark_image()
        for i=0L, self->nframes()-1 do begin ; TODO this is ok for tv?
            psf[*,*,i] = psf[*,*,i]-dark
        endfor
;    endif else message, 'No dark subtracted', /info
    self._image = ptr_new(psf, /no_copy)
end

pro AOpsfcentroid::compute_centroid
	if not (PTR_VALID(self._image)) THEN self->compute
	centr = fltarr(self->nframes(),2)
	for ii=0L, self->nframes()-1 do centr[ii,*] = calc_centroid((*self._image)[*,*,ii])
	self._centroid = ptr_new(centr, /no_copy)
end

function AOpsfcentroid::centroid
    if not (PTR_VALID(self._centroid)) THEN self->compute_centroid
	return, *(self._centroid)
end

; to be implemented in AOtime_series subclassess
function AOpsfcentroid::GetDati
    if not (PTR_VALID(self._centroid)) THEN self->compute_centroid
  	return, self._centroid
end


pro AOpsfcentroid::show, waitms=waitms
	if n_elements(waitms) eq 0 then waitms=0.01
    if not (PTR_VALID(self._centroid)) THEN self->compute_centroid
    loadct,3,/silent
    print, 'Type "s" to stop!'
	for ii=0, self->nframes()-1 do begin
		image_show, (*self._image)[*,*,ii], /as, title='frame '+strtrim(ii,2)
		xyouts, (*self._centroid)[ii,0], (*self._centroid)[ii,1], '+', align=0.5, charsize=1.5
		wait, waitms
		key = get_kbrd(0.01)
		if STRLOWCASE(key) eq 's' then break
	endfor
end

pro AOpsfcentroid__define
    struct = { AOpsfcentroid, $
        _fname         :  "", $
        _dark_fname    :  "", $
        _fitsheader    :  ptr_new(), $
        _image         :  ptr_new(), $
        _dark_image    :  ptr_new(), $
        _pixelscale    :  0d,  $  ;[arcsec/pixel]
        _exptime	   :  0.,  $
        _nframes       :  0L			, $
        _frame_w       :  0L			, $
        _frame_h       :  0L			, $
        _dark_nframes  :  0L			, $
        _dark_frame_w  :  0L			, $
        _dark_frame_h  :  0L			, $
;        _gaussfit      :  obj_new(), $
		_centroid	   :  ptr_new(), $
        INHERITS AOtime_series, $
        INHERITS AOhelp $
    }
end
