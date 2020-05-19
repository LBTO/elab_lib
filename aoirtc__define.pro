
;+
; AOIRTC object initialization
;-

function AOIRTC::Init, root_obj, psf_fname, dark_fname
    if psf_fname eq '' then return,0

    if not file_test(psf_fname) then begin
        message, psf_fname + ' not found', /info
        return,0
    endif
    fitsheader = headfits(psf_fname, /SILENT, errmsg=errmsg)
    if errmsg ne ''  then message, psf_fname+ ': '+ errmsg, /info

	;Camera Temperature (K)
	self._irtc_temp = float(aoget_fits_keyword(fitsheader, 'CAMTEM'))
    if self._irtc_temp gt 260. then begin
    	msg_temp = 'Warning: IRTC temperature > 260K'
        message, msg_temp, /info
	    self._irtc_err_msg += ' - ' + msg_temp
	endif

    ; Pixelscale:
    catch, err
    if err ne 0 then begin 
        apertnr = long(aoget_fits_keyword(fitsheader, 'APERTNR'))
        valid_pixscale = 1B
	    CASE apertnr of
    	    1: pixelscale = 0.010
            2: pixelscale = 0.020
            3: pixelscale = 0.100
        else: begin
     		msg_temp = 'Unknown IRTC pixelscale'
            message, msg_temp, /info
	        self._irtc_err_msg += ' - ' + msg_temp
	        valid_pixscale = 0B
	        pixelscale = !VALUES.F_NAN
           end
        ENDCASE
        catch, /cancel 
    endif else  begin
        valid_pixscale = 1B
        pixelscale = (root_obj->override())->overriden_value('irtc.pixelscale')
        catch, /cancel
    endelse

    ; Detect filter:
    filter_number = long(aoget_fits_keyword(fitsheader, 'FILTRNR'))
    valid_filt_number = 1B
    CASE filter_number OF
    	1: begin
            lambda = 1.30e-6	;BROADBAND (central wavelength (?))
            self._filter_name = 'EMPTY'
           end
    	2: begin
    	    lambda = 1.07e-6	;J
            self._filter_name = 'J'
           end
    	3: begin
    	    lambda = 1.60e-6	;H
            self._filter_name = 'H'
           end
     else: begin
     		;lambda = 1.
     		lambda = !VALUES.F_NAN
     		msg_temp = 'Unknown IRTC filter'
            message, msg_temp, /info
	        self._irtc_err_msg += ' - ' + msg_temp
    		valid_filt_number = 0B
           end
    ENDCASE


    ; Exposure time:
	self._exptime = float(aoget_fits_keyword(fitsheader, 'EXPTIME'))*1e-6	;in seconds
	valid_exptime = 1B
	if self._exptime eq 0 then begin
		msg_temp = 'Unknown IRTC exposure time'
		message, msg_temp, /info
        self._irtc_err_msg += ' - ' + msg_temp
        valid_exptime = 0B
    endif


    ;Framerate:
	framerate = float(aoget_fits_keyword(fitsheader, 'FR-RATE'))
    frame_w  = long(aoget_fits_keyword(fitsheader, 'NAXIS1'))
    frame_h  = long(aoget_fits_keyword(fitsheader, 'NAXIS2'))
    ; We know that full frame cannot be faster then 66Hz
    if frame_w eq 320 and frame_h eq 256 then begin
	    framerate = framerate < 66.
    endif
	if framerate eq 0 then begin
		msg_temp = 'IRTC frame rate unknown'
		message, msg_temp, /info
        self._irtc_err_msg += ' - ' + msg_temp
    endif
	framerate =  framerate < 1./self._exptime		    ; rate cannot be faster than 1/exptime !!!!


	;Dark Frame:
    dark_subdir = ['wfs_calib_'+(root_obj->wfs_status())->wunit(),'irtc','backgrounds','bin1']
    if (n_elements(dark_fname) eq 0) then begin
    	if valid_exptime and valid_filt_number then begin
    		thisJulday = (root_obj->obj_tracknum())->JulDay()
    		full_dark_fname = self->find_dark(thisJulday, dark_subdir, self._exptime, filter_number, frame_w, frame_h, err_msg=dark_err_msg)
   			if strtrim(dark_err_msg,2) ne '' then self._irtc_err_msg += dark_err_msg
   		endif else begin
			msg_temp = 'IRTC dark cannot be searched: ('
			if not valid_exptime then msg_temp += 'exptime '
			if not valid_filt_number then msg_temp += 'filter '
			msg_temp += 'unknown)'
			message, msg_temp, /info
        	self._irtc_err_msg += ' - ' + msg_temp
        endelse
    endif else begin
		full_dark_fname = filepath(root=ao_datadir(), sub=dark_subdir,  dark_fname)
		if not file_test(full_dark_fname) then begin
			msg_temp = 'Overidden IRTC dark file does not exist'
			message, msg_temp, /info
        	self._irtc_err_msg += ' - ' + msg_temp
        endif
	endelse

	;Badpixelmap filename:
	badpixelmap_fname = filepath(root=ao_datadir(), sub=dark_subdir, 'badpixelmap.sav')
	self._badpixelmap_fname = badpixelmap_fname
	badpixelmap_obj = keyword_set(badpixelmap_fname) ? getbadpixelmap(badpixelmap_fname) : 0

    ; subframe
    str = aoget_fits_keyword(fitsheader, 'DETSEC')
    temp = strsplit( strmid(str,1,strlen(str)-2), ",", /ext)
    xra = strsplit( temp[0], ":", /ext)
    yra = strsplit( temp[1], ":", /ext)
	self._subframe[0] = xra[0]-1 ; xmin
	self._subframe[1] = xra[1]-1 ; xmax
	self._subframe[2] = yra[0]-1 ; ymin
	self._subframe[3] = yra[1]-1 ; ymax


    ; File names
    ;self._centroid_fname   = filepath(root=root_obj->elabdir(), 'psfcentroid.sav')
    ;self._store_psd_fname  = filepath(root=root_obj->elabdir(), 'psfcentroid_psd.sav')
    ;self._store_peaks_fname  = filepath(root=root_obj->elabdir(), 'psfcentroid_peaks.sav')
	;self._psf_le_fname     = filepath(root=root_obj->elabdir(), 'psf_le.sav')
	;self._psf_elab_fname   = filepath(root=root_obj->elabdir(), 'psfcube_elab.sav')
	;self._sr_se_fname      = filepath(root=root_obj->elabdir(), 'sr_se.sav')
	;self._profile_fname    = filepath(root=root_obj->elabdir(), 'psf_profile.sav')
	;self._enc_ene_fname    = filepath(root=root_obj->elabdir(), 'psf_enc_ene.sav')

	; initialize PSF object
    if not self->AOpsfAbstract::Init(psf_fname, full_dark_fname, pixelscale, lambda, framerate, $
    	badpixelmap_obj=badpixelmap_obj, label=root_obj->tracknum(),$
        store_radix= filepath(root=root_obj->elabdir(), 'irtc'), $
        recompute=root_obj->recompute()) then return,0

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOIRTC', 'IRTC image') then return, 0
    self->addMethodHelp, "temp()", "IRTC temperature (K)"
    self->addMethodHelp, "subframe()", "subframe of IRTC detector read [xmin, xmax, ymin, ymax]"
    self->addMethodHelp, "exptime()", "exposure time (s)"
    self->AOpsfAbstract::addHelp, self

    return, 1
end


;Searches an IRTC dark closest in time to the IRTC image with the same set of parameters
;------------------------------------------------------------------------------------------
function AOIRTC::find_dark, thisJulday, dark_subdir, exptime, filter_number, frame_w, frame_h, err_msg=err_msg

	err_msg = ""

	; Verify that the dark and the psf image were taken with the same exposure time!
	all_darks_search = filepath(root=ao_datadir(), sub=dark_subdir,  '*_cube.fits')
	all_darks_fname  = file_search(all_darks_search, count=ndarks)

	if ndarks gt 0 then begin
		all_darks_julday = dblarr(ndarks)
		for ii=0, ndarks-1 do begin
			dark_tracknum = strmid(file_basename(all_darks_fname[ii]), 0, 15)
   			y  = fix(strmid(dark_tracknum,  0, 4))
   			m  = fix(strmid(dark_tracknum,  4, 2))
   			d  = fix(strmid(dark_tracknum,  6, 2))
   			hh = fix(strmid(dark_tracknum,  9, 2))
   			mm = fix(strmid(dark_tracknum, 11, 2))
   			ss = fix(strmid(dark_tracknum, 13, 2))
   			all_darks_julday[ii] = julday(m, d, y, hh, mm, ss)
		endfor
		idx_closest = sort(abs(all_darks_julday - thisjulday))

		dark_found = 0B & dd=0
		while (dark_found eq 0B) and (dd lt ndarks) do begin
			;Retrieve header info of averaged dark frame because the file with the cube didn't have any info saved in the header...
			closest_av_dark_fname = filepath(root=ao_datadir(), sub=dark_subdir, file_basename(all_darks_fname[idx_closest[dd]], '_cube.fits'))
			dark_header = headfits(closest_av_dark_fname)
			dark_exptime = float(aoget_fits_keyword(dark_header, 'EXPTIME'))*1e-6	;in seconds
			dark_filter_number = long(aoget_fits_keyword(dark_header, 'FILTRNR'))
			dark_frame_w = long(aoget_fits_keyword(dark_header, 'NAXIS1'))
			dark_frame_h = long(aoget_fits_keyword(dark_header, 'NAXIS2'))
			if (dark_exptime eq exptime) and (filter_number eq dark_filter_number) and  $
			   (dark_frame_w eq frame_w) and (dark_frame_h eq frame_h) then dark_found=1B else dd+=1
		endwhile
		if dark_found then begin
			dark_fname = all_darks_fname[idx_closest[dd]]
		 	time_elapsed = abs(all_darks_julday[idx_closest[dd]] - thisjulday)
		 	max_time_elapsed = julday(01, 01, 2010, 01, 00, 00) -  julday(01, 01, 2010, 00, 00, 00)
		 	if time_elapsed gt max_time_elapsed then begin
		 		msg_temp = 'Warning: Selected IRTC dark +'+strtrim(round(time_elapsed/max_time_elapsed),2)+'h old!'
		 		message, msg_temp, /info
		 		err_msg += ' - ' + msg_temp
		 	endif
		endif else begin
			msg_temp = 'No compatible IRTC dark found (i.e. same exposure time or filter or dimensions)'
			message, msg_temp, /info
			err_msg += ' - ' + msg_temp
			return, ""
		endelse
	endif else begin
		msg_temp = 'No IRTC darks found matching '+all_darks_search
		message, msg_temp, /info
 		err_msg += ' - ' + msg_temp
		return, ""
	endelse

	return, dark_fname
end

;This function overrides the dark_image() method in AOpsfAbstract.
function AOIRTC::dark_image
    if not (PTR_VALID(self._dark_image)) then begin
    	cube_fname = self->dark_fname()
    	saved_dark_fname = (filepath(root=ao_elabdir(), subdir='irtc_darks', $
    		strsplit(file_basename(cube_fname), '_cube.fits', /extract, /regex)))[0]
		if file_test(saved_dark_fname) then self._dark_image = ptr_new(readfits(saved_dark_fname,/SILENT)) else begin
			dark = self->AOpsfAbstract::dark_image()
			if not file_test(file_dirname(saved_dark_fname), /dir) then file_mkdir, file_dirname(saved_dark_fname)
       		writefits, saved_dark_fname, dark
		endelse
	endif
	return, *(self._dark_image)
end

; IRTC temperature (K)
function AOIRTC::temp
	return, self._irtc_temp
end

function AOIRTC::subframe
	return, self._subframe
end

function AOIRTC::filter_name
	return, self._filter_name
end

function AOIRTC::exptime
	return, self._exptime
end


;Returns the error messages
;-----------------------------------------------------
function AOIRTC::isok, cause=cause
	isok=1B
    isok *= self->AOpsfAbstract::isok(cause=cause)
	if strtrim(self._irtc_err_msg,2) ne '' then begin
		isok*=0B
		cause += self._irtc_err_msg
	endif
	return, isok
end



pro AOIRTC__define
    struct = { AOIRTC					, $
        _filter_name        : ""           , $
    	_irtc_err_msg		: ""		, $
    	_irtc_temp			: 0.0		, $
    	_exptime 			: 0.0		, $
		_subframe           : [0,0,0,0]    , $
        INHERITS    AOpsfAbstract,  $
        INHERITS    AOhelp  $
    }
end
