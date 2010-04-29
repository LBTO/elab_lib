
;+
;
;-

function AOframes::Init, root_obj, frames_file

    if file_test(frames_file) eq 0 then begin
        message, 'Cannot find frames  file: '+frames_file, /inform
        return, 0
    endif

    self._nph_per_int_av	= -1.0
    self._nph_per_int_rms   = -1.0
    self._nph_per_sec_av    = -1.0
    self._nphsub_per_int_av = -1.0

	; number of photons per ADU
	self._photons_per_ADU = 0.5

	; filename
    self._filename = frames_file

	; pointer to wfs status
	self._wfs_status = ptr_new(root_obj->wfs_status())

	; get frames parameters
    self._header = ptr_new(headfits(self._filename, /SILENT), /no_copy)
    ;frames = readfits(frames_file, header, /SILENT)
    ;ss = size(frames)
    self._frame_w  = long(aoget_fits_keyword(self->header(), 'NAXIS1'))
    self._frame_h  = long(aoget_fits_keyword(self->header(), 'NAXIS2'))
    self._nframes  = long(aoget_fits_keyword(self->header(), 'NAXIS3'))

	self._fluxdata_fname = filepath(root=root_obj->elabdir(), 'flux_data.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._fluxdata_fname, /allow_nonexistent
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

pro AOframes::Cleanup
    ptr_free, self._header
    ptr_free, self._nph_per_int
    self->AOhelp::Cleanup
end


;+
; RESTORES THE FRAMES DATA
;-
function AOframes::frames, DARK_SUBTRACTED=DARK_SUBTRACTED
	fr = float(readfits(self._filename, /SILENT))
	if keyword_set(DARK_SUBTRACTED) then begin
		dark = (*self._wfs_status->ccd39())->dark()
		if n_elements(dark) eq 1 then print, 'CCD39 DARK FILE NOT FOUND -> DARK NOT SUBTRACTED' else $
			for ii=0L, self._nframes-1 do fr[*,*,ii] = fr[*,*,ii] - dark
	endif
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
		indpup = (*self._wfs_status->pupils())->indpup()

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
        INHERITS AOhelp 				  $
    }
end
