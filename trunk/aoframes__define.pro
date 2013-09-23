
;+
;
;-

function AOframes::Init, root_obj, frames_file, antidrift_fname

    if file_test(frames_file) eq 0 then begin
        message, 'Cannot find frames  file: '+frames_file, /inform
        return, 0
    endif

    self._nph_per_int_av	= -1.0
    self._nph_per_int_rms   = -1.0
    self._nph_per_sec_av    = -1.0
    self._nphsub_per_int_av = -1.0
	self._ron				= -1.0

	; number of photons per ADU
	self._photons_per_ADU = 0.5

	; filename
    self._filename = frames_file

	; antidrift data
	if file_test(antidrift_fname) then self._antidrift_fname = antidrift_fname
	self._antidrift_status = -1

	; pointer to wfs status
	self._wfs_status = ptr_new(root_obj->wfs_status())

	; get frames parameters
    self._header = ptr_new(headfits(self._filename, /SILENT), /no_copy)
    self._frame_w  = long(aoget_fits_keyword(self->header(), 'NAXIS1'))
    self._frame_h  = long(aoget_fits_keyword(self->header(), 'NAXIS2'))
    self._nframes  = long(aoget_fits_keyword(self->header(), 'NAXIS3'))

	self._fluxdata_fname = filepath(root=root_obj->elabdir(), 'flux_data.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._fluxdata_fname, /allow_nonexistent
    endif

	self._rondata_fname = filepath(root=root_obj->elabdir(), 'ron_data.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._rondata_fname, /allow_nonexistent
    endif

	self._pupils_fname = filepath(root=root_obj->elabdir(), 'pupils_data.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._pupils_fname, /allow_nonexistent
    endif


    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOframes', 'Represent WFS frames') then return, 0
    self->addMethodHelp, "frames_fname()", "frames file name (string)"
    self->addMethodHelp, "header()", "header of frames fits file (strarr)"
    self->addMethodHelp, "frames(/dark_subtracted)", "WFS frames, eventually dark subtracted (long[frame_w, frame_h, nframes])"
    self->addMethodHelp, "nframes()", "number of WFS frames saved (long)"
    self->addMethodHelp, "frame_w()", "frame width [px] (long)"
    self->addMethodHelp, "frame_h()", "frame height [px] (long)"
    self->addMethodHelp, "nph_per_int()", "total number of photons per frame vs time"
    self->addMethodHelp, "nph_per_int_av()", "time-average of total number of photons per frame"
    self->addMethodHelp, "nph_per_int_rms()", "time-stddev of total number of photons per frame"
    self->addMethodHelp, "nph_per_sec_av()",  "total number of photons per second, time-averaged"
    self->addMethodHelp, "nphsub_per_int_av()", "number of photons per frame per subaperture, time-averaged"
    self->addMethodHelp, "adu_per_quadrant()", "ADU per quadrant"
    self->addMethodHelp, "antidrift_fname()", "AntiDrift correction file name (string)"
    self->addMethodHelp, "antidrift_values()", "AntiDrift correction history (float)"
    self->addMethodHelp, "antidrift_status()", "Returns 1 if AntiDrift is activated, 0 otherwise (integer)"
    self->addMethodHelp, "ron()", "Estimate of Read-Out Noise"
    self->addMethodHelp, "center_separation()", 'Estimates 4 center separations (float)"
    self->addMethodHelp, "mean_center_separation()", 'Mean of 4 center separations (float)"
    self->addMethodHelp, "pup_diameter()", 'Estimates 4 pupil diameters (float)"
    self->addMethodHelp, "mean_pup_diameter()", "Mean of 4 pupil diameters (float)"
    self->addMethodHelp, "pup_image()", "Image used for realtime pupil acquisition"
    self->addMethodHelp, "replay, wait=wait, zoom=zoom", "Replay the CCD frames"
    return, 1
end

function AOframes::frames_fname
	return, self._filename
end

function AOframes::nframes
	return, self._nframes
end

function AOframes::frame_w
	return, self._frame_w
end

function AOframes::frame_h
	return, self._frame_h
end

function AOframes::adu_per_quadrant
    if not ptr_valid(self._adu_per_quadrant) then self->calc_number_photons
	return, *(self._adu_per_quadrant)
end

function AOframes::nph_per_int
    if not ptr_valid(self._nph_per_int) then self->calc_number_photons
	return, *(self._nph_per_int)
end

function AOframes::nph_per_int_av
    if self._nph_per_int_av eq -1.0 then self->calc_number_photons
	return, self._nph_per_int_av
end

function AOframes::nph_per_int_rms
    if self._nph_per_int_rms eq -1.0 then self->calc_number_photons
	return, self._nph_per_int_rms
end

function AOframes::nph_per_sec_av
    if self._nph_per_sec_av eq -1.0 then self->calc_number_photons
	return, self._nph_per_sec_av
end

function AOframes::nphsub_per_int_av
    if self._nphsub_per_int_av eq -1.0 then self->calc_number_photons
	return, self._nphsub_per_int_av
end

function AOframes::header
    if (PTR_VALID(self._header)) THEN return, *(self._header) else return, 0d
end

function AOframes::antidrift_fname
	return, self._antidrift_fname
end

function AOframes::antidrift_values
	fname = self->antidrift_fname()
	if strtrim(fname,2) eq "" then return, fltarr(self->nframes()) else $
	return, readfits(fname,/SILENT)
end

function AOframes::antidrift_status
	if self._antidrift_status eq -1 then begin
		ad = self->antidrift_values()
		if total(abs(ad)) eq 0. then self._antidrift_status = 0 else $
								self._antidrift_status = 1
	endif
	return, self._antidrift_status
end

function AOframes::dark, dark_ok=dark_ok
	dark_ok = 1B
	dark_fname = (*self._wfs_status->ccd39())->dark_fname()

	if not file_test(dark_fname) then begin
		message, 'CCD39 dark file not found: '+dark_fname, /info
		dark_ok = 0B
		return, -1
	endif

	;;;;;First implementation of Antidrift: DARK was overwritten!!!
	if file_basename(dark_fname) eq 'darktmp.fits' then begin
		dark_dir = file_dirname(dark_fname)
		ad_fname = self->antidrift_fname()
		if strtrim(ad_fname,2) ne "" then begin
			ad_hdr = headfits(ad_fname,/SILENT)
			dark_fname =filepath(root=dark_dir,  aoget_fits_keyword(ad_hdr, 'DARK'))
			dark = float(readfits(dark_fname, /silent))
			ad_values = self->antidrift_values()
			dark += ad_values[0]
		endif else begin
			dark_ok = 0B
			dark = -1
		endelse
	endif else dark = float(readfits(dark_fname, /silent))

	return, dark
end

;+
; RESTORES THE FRAMES DATA
;-
function AOframes::frames, DARK_SUBTRACTED=DARK_SUBTRACTED
	if keyword_set(DARK_SUBTRACTED) then begin
;		dark = (*self._wfs_status->ccd39())->dark()
		dark = self->dark(dark_ok=dark_ok)
		if not dark_ok then return,-1
        endif
	fr = float(readfits(self._filename, /SILENT))
	if n_elements(fr) eq 1 then return,-1
	if keyword_set(DARK_SUBTRACTED) then for ii=0L, self->nframes()-1 do fr[*,*,ii] = fr[*,*,ii] - dark
	return, fr
end


;+
; ESTIMATES THE NUMBER OF PHOTONS
;-
pro AOframes::calc_number_photons

	if file_test(self._fluxdata_fname) then begin
		restore, self._fluxdata_fname
	endif else begin
		fr = self->frames(/DARK_SUB)
		if n_elements(fr) eq 1 then return
		if obj_valid(*self._wfs_status->pupils()) then $
			indpup = (*self._wfs_status->pupils())->indpup() else return

		; Estimate the number of counts in the pupil per integration time
		adu_per_int      = fltarr(self._nframes)
		adu_per_quadrant = fltarr(4,self._nframes)
		for ii=0L, self._nframes-1 do begin
        	adu_per_int[ii] = total((fr[*,*,ii])[indpup])
        	adu_per_quadrant[0,ii] = total(fr[0:self._frame_w/2-1, 0:self._frame_h/2-1,ii])
        	adu_per_quadrant[1,ii] = total(fr[self._frame_w/2:*, 0:self._frame_h/2-1,ii])
        	adu_per_quadrant[2,ii] = total(fr[0:self._frame_w/2-1, self._frame_h/2:*,ii])
        	adu_per_quadrant[3,ii] = total(fr[self._frame_w/2:*, self._frame_h/2:*,ii])
    	endfor
		save, adu_per_int, adu_per_quadrant, filename=self._fluxdata_fname
	endelse

   	self._nph_per_int      = ptr_new(adu_per_int * self._photons_per_ADU)
   	self._adu_per_quadrant = ptr_new(adu_per_quadrant)

	; Estimate the averaged and rms number of photons in the pupil per integration time
	self._nph_per_int_av  =   mean(adu_per_int) * self._photons_per_ADU
	self._nph_per_int_rms = stddev(adu_per_int) * self._photons_per_ADU

	; Estimate the averaged number of photons in the pupil per second
	self._nph_per_sec_av = self._nph_per_int_av * (*self._wfs_status->ccd39())->framerate()

	; Estimate the averaged number of photons per subaperture per integration time
	nsub   = (*self._wfs_status->pupils())->nsub()
	self._nphsub_per_int_av = self._nph_per_int_av / nsub
end

;+
; ESTIMATES THE READ-OUT NOISE (RON)
;-
pro AOframes::calc_ron
	if file_test(self._rondata_fname) then begin
		restore, self._rondata_fname
	endif else begin
		fr  = self->frames(/DARK_SUB)
		if n_elements(fr) eq 1 then return
		roi = self->ron_roi()
		np = n_elements(roi)
		ronmes = fltarr(np,self->nframes())
		for i=0, self->nframes()-1 do ronmes[*,i] = (fr[*,*,i])[roi]
		ron_vals = fltarr(np)
		for j=0, np-1 do ron_vals[j] = stddev(ronmes[j,*]) * self._photons_per_ADU
		save, ron_vals, roi, filename=self._rondata_fname
	endelse
	self._ron = mean(ron_vals)
end

function AOframes::ron_roi
	frw = self->frame_w()
	frh = self->frame_h()
	roi = lonarr(frw,frh)
	bin = (*self._wfs_status->ccd39())->binning()
	px = 1 ;pixels from border
	if bin eq 1 then begin
		roi[px:px+1,px:px+1] = 1
		roi[frw-px-2:frw-px-1,px:px+1] = 1
		roi[frw-px-2:frw-px-1,frh-px-2:frh-px-1] = 1
		roi[px:px+1,frh-px-2:frh-px-1] = 1
	endif else begin
		roi[px,px] = 1
		roi[frw-px-1,px] = 1
		roi[frw-px-1,frh-px-1] = 1
		roi[px,frh-px-1] = 1
	endelse
	return, where(roi)
end

function AOframes::ron
	if self._ron eq -1.0 then self->calc_ron
	return, self._ron
end


;+
; ESTIMATES FOUR CENTER SEPARATIONS
;-
pro AOframes::calc_pupils
	if file_test(self._pupils_fname) then begin
		restore, self._pupils_fname
	endif else begin
		f = self->frames(/dark)
                if n_elements(f) eq 1 then begin
		    message, 'Cannot calculate pupils'
                    return
                endif
		fm = total(f,3) / n_elements(f[0,0,*])
		distanza_centri, fm, m, w, dl, md, /SPLIT, TH1=0.3, TH2=0.3, /QUIET
		save, m, w, dl, md, filename=self._pupils_fname
        endelse
	self._center_separation = w
        self._pup_diameter = md
end

function AOframes::center_separation
	if total(self._center_separation) eq 0 then self->calc_pupils
	return, self._center_separation
end

function AOframes::pup_diameter
	if total(self._pup_diameter) eq 0 then self->calc_pupils
	return, self._pup_diameter
end

function AOframes::pup_image
    f = total(self->frames(/dark),3)
    dimx = n_elements(f[*,0])
    dimy = n_elements(f[0,*])
    binning = 80/dimx
    cx = dimx/2
    cy = dimy/2
    cx1 = cx+2/binning
    cy1 = cy+0/binning
    dx = cx1-cx
    dy = cy1-cy
    dim = min([cx-abs(dx), cy-abs(dy)])
    a = f[cx1-dim:cx1-1, cy1-dim:cy1-1]
    b = f[cx1:cx1+dim-1, cy1-dim:cy1-1]
    c = f[cx1-dim:cx1-1, cy1:cy1+dim-1]
    d = f[cx1:cx1+dim-1, cy1:cy1+dim-1]
    side=36/binning

    a += shift( b, dim-side, 0)
    a += shift( c, 0, dim-side)
    a += shift( d, dim-side, dim-side)

    return, a

end

function AOframes::mean_center_separation
	return, mean(self->center_separation())
end

function AOframes::mean_pup_diameter
	return, mean(self->pup_diameter())
end

;Replay the ccd frames
pro AOframes::replay, wait=wait, zoom=zoom
	if n_elements(wait) eq 0 then wait=0.01
	if not keyword_set(zoom) then zoom=1
	fr = self->frames(/dark) > 0
	dim = (size(fr,/dim))[0:1]
	rr = minmax(fr)

    window,/free, xsize=dim[0]*zoom, ysize=dim[1]*zoom
    print, 'Type "s" to stop!'
;	for ii=0, self->nframes()-1 do begin
		for ii=0, 2000 do begin
		tv, rebin(bytscl(fr[*,*,ii], min=rr[0], max=rr[1]), dim[0]*zoom, dim[1]*zoom)
;		cgImage, rebin(bytscl(fr[*,*,ii], min=-1., max=1.), dim[0]*zoom, dim[1]*zoom)
		wait, wait
		key = get_kbrd(0.01)
		if STRLOWCASE(key) eq 's' then break
	endfor
end


pro AOframes::free
    if ptr_valid(self._header) then ptr_free, self._header
    ;if ptr_valid(self._wfs_status) then ptr_free, self._wfs_status
    if ptr_valid(self._adu_per_quadrant) then ptr_free, self._adu_per_quadrant
    if ptr_valid(self._nph_per_int) then ptr_free, self._nph_per_int
end

pro AOframes::Cleanup
    if ptr_valid(self._header) then ptr_free, self._header
    if ptr_valid(self._wfs_status) then ptr_free, self._wfs_status
    if ptr_valid(self._adu_per_quadrant) then ptr_free, self._adu_per_quadrant
    if ptr_valid(self._nph_per_int) then ptr_free, self._nph_per_int
    self->AOhelp::Cleanup
end

pro AOframes__define
    struct = { AOframes					, $
        _filename          : ""			, $
        _header            : ptr_new()  , $
		_wfs_status		   : ptr_new()	, $
        _nframes           : 0L			, $
        _frame_w           : 0L			, $
        _frame_h           : 0L			, $
        _photons_per_ADU   : 0.0		, $
        _adu_per_quadrant  : ptr_new()	, $
        _nph_per_int	   : ptr_new()	, $
        _nph_per_int_av	   : 0.0		, $
        _nph_per_int_rms   : 0.0		, $
        _nph_per_sec_av    : 0.0		, $
        _nphsub_per_int_av : 0.0		, $
        _fluxdata_fname	   : ""			, $
        _antidrift_fname   : ""			, $
        _antidrift_status  : 0			, $
        _rondata_fname	   : ""			, $
        _pupils_fname	   : ""			, $
        _ron			   : 0.0		, $
        _center_separation : fltarr(4)  , $
        _pup_diameter      : fltarr(4)  , $
        INHERITS AOhelp 				  $
    }
end
