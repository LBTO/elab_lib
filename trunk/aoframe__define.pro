
;+
; AOframe object initialization
;
; An AOframe represent an image (or cube of images)
;
; You can access frames at variuos stages during the cleanup process:
; from raw-data to dark_subtracted, badpixel interpolated, common-mode subtracted.
; You can access ROI of the frame(s)
;
; Raw frames can be cleaned subtracting a dark image
; If a badpixel map is provided, the missing pixels are reconstructed using triangular interpolation
; Raw data and dark frame must be provided in a fits files [N,W,H] or [W,H], with positive values (integer or float)
; Badpixelmap must be provided in a sav file created using make_bad_pixel_map
;
; ROI is expressed as [xmin,xmax,ymin,ymax]
;
; The AOframe does NOT care of lambda, pixelscale, exposure time and framerate
;
; INPUT
;   frames_fname         images cube fits file name (full path)
;
; KEYWORD
;   dark_fname           dark fits file name (full path)
;	badpixelmap_obj      bad pixel map object (AObadpixelmap).
;	line_noise           set this keyword to correct the line noise. Use it if only one object is present in the frame
;                        Keyword value has to be the typical width of the feature in the frame [px].
;   roi                  [xmin, xmax, ymin, ymax]
;   recompute            set to force recomputing of stored data
;   stored_le_fname      where to save the longexposure  (full path)
;   stored_cube_fname    where to save the cube (full path)
;-

function AOframe::Init, frames_fname, dark_fname=dark_fname, badpixelmap_obj=badpixelmap_obj,line_noise=line_noise, roi=roi, $
            recompute=recompute, stored_le_fname=stored_le_fname, stored_cube_fname=stored_cube_fname

    if not file_test(frames_fname) then begin
        message, frames_fname + ' not found', /info
        return,0
    endif
    self._fname = frames_fname
    self._fitsheader = ptr_new(headfits(self._fname, /SILENT), /no_copy)

    if keyword_set(dark_fname) then begin
        self._dark_fname = dark_fname
   	    if not file_test(self->dark_fname()) then message, self->dark_fname() + ' Dark file does not exist', /info
    endif

	if keyword_set(badpixelmap_obj) then self._badpixelmap_obj = badpixelmap_obj

    if keyword_set(line_noise) then self._object_size = line_noise

    if keyword_set(stored_le_fname)   then self._stored_le_fname    = stored_le_fname
    if keyword_set(stored_cube_fname) then self._stored_cube_fname  = stored_cube_fname

    naxis = long(aoget_fits_keyword(self->header(), 'NAXIS'))
    self._frame_w  = long(aoget_fits_keyword(self->header(), 'NAXIS1'))
    self._frame_h  = long(aoget_fits_keyword(self->header(), 'NAXIS2'))
    self._nframes = (naxis eq 2) ? 1 :  long(aoget_fits_keyword(self->header(), 'NAXIS3')) ;TODO

    ; ROI
    if not keyword_set(roi) then roi = [0, self._frame_w-1, 0, self._frame_h-1]
    self._roi = roi


    if keyword_set(recompute) eq 1B then begin
        file_delete, self._stored_le_fname, /allow_nonexistent   ;Long-exposure PSF
        file_delete, self._stored_cube_fname, /allow_nonexistent ;PSF cube elaborated (dark-subtracted, badpixel corrected)
    endif

    return, 1
end

pro AOframe::addHelp, obj
    ; initialize help object and add methods and leafs
    obj->addMethodHelp, "longExposure(/fullframe)",   "long exposure corrected for dark, badpixel, line-noise [roi_w, roi_h]"
    obj->addMethodHelp, "imageCube()", 		"psf frames each individually corrected for dark and badpixels [roi_w, roi_h, nframes]"
    obj->addMethodHelp, "rawImage(/dark_corr, /badpixel, /linenoise)",       "long exposure rawImage with optional correction [frame_w, frame_h]"
    obj->addMethodHelp, "dark_image()", 	"psf dark image (float) [frame_w, frame_h]"
    obj->addMethodHelp, "badpixelmap()", 	"bad pixel mask image [roi_w, roi_h]"
    obj->addMethodHelp, "fname()",      	"psf file name(s) [string or strarr]"
    obj->addMethodHelp, "dark_fname()", 	"psf dark file name(s) [string]"
    obj->addMethodHelp, "set_dark_image, dark ", "set dark image. This overrides the dark_fname [frame_w, frame_h]"
    obj->addMethodHelp, "header()",  		"fits file headers [nfilesstrarr]"
    obj->addMethodHelp, "dark_header()", 	"psf dark fits file header  [strarr]"
    obj->addMethodHelp, "badpixelmap_fname()", 	"psf bad pixel map file name [string]"
    obj->addMethodHelp, "badpixelmap_obj()", 	"psf bad pixel map object [aobadpixelmap]"
    obj->addMethodHelp, "nframes()", 		"number of frames saved (long)"
    obj->addMethodHelp, "frame_w()", 		"frame width [px] (long)"
    obj->addMethodHelp, "frame_h()", 		"frame height [px] (long)"
    obj->addMethodHelp, "roi()", 		    "subframe used to return images [xmin,xmax,ymin,ymax]"
    obj->addMethodHelp, "roi_w()", 		    "roi width [px] (long)"
    obj->addMethodHelp, "roi_h()", 		    "roi height [px] (long)"
    obj->addMethodHelp, "setroi, roi", 		"set subframe used to return images [xmin,xmax,ymin,ymax]"
end


pro aoframe::summary
    print, string(format='(%"%-30s %s")','File name', self->fname() )
    print, string(format='(%"%-30s %s")','Dark name', self->dark_fname() )
    print, string(format='(%"%-30s %d")','N frames', self->nframes() )
    print, string(format='(%"%-30s %d")','Width [px]', self->frame_w() )
    print, string(format='(%"%-30s %d")','Height [px]', self->frame_h() )
    print, string(format='(%"%-30s [%d:%d, %d:%d]")','ROI [px]', self->roi() )
end


;===================================================================================
; 		P S F   I M A G E   A N D   D A R K   I M A G E   P R O C E S S I N G
;===================================================================================

pro AOframe::process_cube
    current_dark_fname        = file_basename(self->dark_fname())
    current_badpixelmap_fname = file_basename(self->badpixelmap_fname())
    current_object_size       = self._object_size
    if file_test(self._stored_cube_fname) then begin
        restore, self._stored_cube_fname
        ;Check whether the dark used to compute the saved LE PSF was the same....
        if used_dark_fname ne current_dark_fname then begin
            message, 'WARNING: The dark used to compute the saved LE-frame is not the same as the current dark', /info
            file_delete, self._stored_cube_fname, /allow_nonexistent
            self->process_cube
            return
        endif
        ;Check whether the badpixelmap used to compute the saved LE PSF was the same....
        if used_badpixelmap_fname ne current_badpixelmap_fname then begin
            message, 'WARNING: The badpixelmap used to compute the saved LE-frame is not the same as the current badpixelmap', /info
            file_delete, self._stored_cube_fname, /allow_nonexistent
            self->process_cube
            return
        endif
        ;Check whether the object size used to compute the saved LE PSF was the same....
        if used_object_size ne current_object_size then begin
            message, 'WARNING: The object size used to compute the saved LE-frame is not the same as the current object size', /info
            file_delete, self._stored_cube_fname, /allow_nonexistent
            self->process_cube
            return
        endif
    endif else begin
        psf = float(readfits(self->fname(), header, /SILENT))
        for i=0L, self->nframes()-1 do begin
            if (i+1) mod 10 eq 0 then print, string(format='(%"%d of %d ")', i+1, self->nframes() )
        	if self._dark_fname ne "" then           psf[*,*,i] -= self->dark_image()         ; subtract dark
        	if obj_valid(self._badpixelmap_obj) then psf[*,*,i] = self->maneggiaFrame(psf[*,*,i]) ; trigrid bad pixels
			;if self._object_size ne 0 then           psf[*,*,i] = self->pulisceFrame(psf[*,*,i])  ; remove line noise
        endfor
        print, 'saving data...'
        used_dark_fname         = current_dark_fname
        used_badpixelmap_fname  = current_badpixelmap_fname
        used_object_size        = current_object_size
        save, psf, used_dark_fname, used_badpixelmap_fname, used_object_size, filename=self._stored_cube_fname ;, /compress
        print, 'done'
    endelse
    roi=self->roi()
    self._imagecube = ptr_new(psf[roi[0]:roi[1], roi[2]:roi[3], *], /no_copy)
end


function AOframe::maneggiaFrame, psf
    ; remove bad pixels interpolating with neighbours
    if obj_valid(self._badpixelmap_obj) then begin  ; TODO use catch!!!
        trstr = self._badpixelmap_obj->triangulation()
        if trstr.np gt 0 then begin
            sz=size(psf, /dim)
            szbpm = [self._badpixelmap_obj->frame_w(), self._badpixelmap_obj->frame_h()]
            if total(szbpm - sz) ne 0 then begin
            	subframe = self->subframe()
            	psf1 = make_array(szbpm, /float)
            	psf1[subframe[0]:subframe[1]-1,subframe[2]:subframe[3]-1] = psf[0:sz[0]-2,0:sz[1]-2] ;seems that right border of subframe is always shitty...
    	    	psf1 = TRIGRID(float(trstr.x), float(trstr.y), psf1[trstr.idx], trstr.tr, xout=findgen(szbpm[0]), yout=findgen(szbpm[1]))
				psf = psf1[subframe[0]:subframe[1],subframe[2]:subframe[3]]
            endif else $
    	    	psf = TRIGRID(float(trstr.x), float(trstr.y), psf[trstr.idx], trstr.tr, xout=findgen(sz[0]), yout=findgen(sz[1]))
        endif
    endif
    return, psf
end

; in case of a single object you can identify the background and
; clean for the  line noise.
function AOframe::pulisceFrame, psf_in
	;Mask out the PSF
	;ima_fit = gauss2dfit(double(psf_in),coeff) ; LB this seems too much, you already subtracted the background
	;xc = coeff[4]
	;yc = coeff[5]
    res=max(psf_in, idxmax)
    xc = idxmax mod n_elements(psf_in[*,0])
    yc = idxmax / n_elements(psf_in[*,0])
	mask = self->background_mask(xc,yc,mask_ok=mask_ok)
	if ~mask_ok then begin
		message, 'Warning: not possible to clean the PSF',/info
		return, psf_in
	endif
	psf = psf_in
	; Clean image column by column
	;-----------------------------
    for j=0L, self._frame_w-1 do begin
    	colidx = where(mask[j,*],count)
		if count ne 0 then psf[j,*] -= median((psf[j,*])[colidx])
	endfor
	; Clean image row by row
	;-----------------------------
    for j=0L, self._frame_h-1 do begin
    	rowidx = where(mask[*,j],count)
		if count ne 0 then psf[*,j] -= median((psf[*,j])[rowidx])
	endfor
	return, psf
end


; in case of a single/few objects you can identify the background pixels
; and create a background mask with circular holes
function AOframe::background_mask, xc, yc,  nsup=nsup, borderpix=borderpix, mask_ok=mask_ok
	if n_params() ne 2 then message, 'Syntax: ...->background_mask(xc,yc)'
	;if not keyword_set(nsup) then nsup = 40.	;maximum radius to ensure all star light is in.
	;dsup = ao_pupil_diameter() / nsup
    ;if not finite(self->pixelscale()) or not finite(self->lambda()) then begin
    ; 	mask_ok=0B
    ;	return, -1
    ;endif
	;control_diam_pix = self->lambda() / dsup / 4.85e-6 / self->pixelscale()
	np = max([self._frame_w, self._frame_h])
	;diaratio = control_diam_pix/float(np)
	diaratio = self._object_size/float(np)
	if diaratio gt 1 then begin
		mask_ok=0B
		return, -1
	endif
	xcr = 2*xc/float(np-1) - 1.0
	ycr = 2*yc/float(np-1) - 1.0
	mask = make_mask(np, diaratio=diaratio, xc=xcr, yc=ycr, /inverse)
	mask = mask[0:self._frame_w-1,0:self._frame_h-1]
	if keyword_set(borderpix) then begin
		mask1 = self->striscia_mask(borderpix,0,/inverted)
		mask *= mask1
	endif
	mask_ok=1B
	return, mask
end


;+
; len	: striscia length
; npr	: number of pixels from the border to omit
;-
function AOframe::striscia_mask, len, npr, inverted=inverted
	if n_params() ne 2 then message, 'Syntax: ...->striscia_mask(len,npr)'
	npx = self._frame_w
	npy = self._frame_h
	mask1 = fltarr(npx,npy)
	mask1[npr:npx-npr-1,npr:npr+len] = 1.
	mask1[npr:npx-npr-1,npy-npr-len:npy-npr-1] = 1.
	mask1[npr:npr+len,npr+len:npy-npr-len-1] = 1.
	mask1[npx-npr-len:npx-npr-1,npr+len:npy-npr-len-1] = 1.
	if keyword_set(inverted) then mask1 = float(~mask1)
	return, mask1
end

; return the badpixelmap
function AOframe::badPixelMap
    if not obj_valid(self._badpixelmap_obj) then message, 'badPixelMap not available'
    roi=self->roi()
    return, (self._badpixelmap_obj->badpixelmap())[roi[0]:roi[1], roi[2]:roi[3]]
end

function AOframe::dark_image
    if ptr_valid(self._tmp_dark_image) then return, *(self._tmp_dark_image)
    if not (PTR_VALID(self._dark_image)) then begin
    	cube_fname = self->dark_fname()
   		if file_test(cube_fname) then begin
       		dark = float(readfits(cube_fname, dark_header, /SILENT))
    		naxis = long(aoget_fits_keyword(dark_header, 'NAXIS'))

            ; check sizes
   			dark_frame_w = long(aoget_fits_keyword(dark_header, 'NAXIS1'))
   			dark_frame_h = long(aoget_fits_keyword(dark_header, 'NAXIS2'))
   			if (dark_frame_w ne self._frame_w) or (dark_frame_h ne self._frame_h) then begin
   				message, 'Dark and images do not have the same dimensions!! Skip dark correction', /info
   				self._dark_image = ptr_new(fltarr(self._frame_w, self._frame_h))
   			endif else begin

   			    dark_nframes = (naxis eq 2) ? 1 : long(aoget_fits_keyword(dark_header, 'NAXIS3'))
       		    self._dark_image = dark_nframes gt 1 ? ptr_new( median(dark, dim=3) ) :  ptr_new(dark)
       		endelse
       	endif else begin
       		message, 'Dark file not existing. Assuming it zero', /info
       		self._dark_image = ptr_new(fltarr(self._frame_w, self._frame_h))
       	endelse
    endif
    return, *(self._dark_image)
end

pro AOframe::set_dark_image, dark_image
    self->free
    if self._stored_le_fname ne '' then file_delete, self._stored_le_fname, /allow_nonexistent
    if self._stored_cube_fname ne '' then file_delete, self._stored_cube_fname, /allow_nonexistent
    ; TODO check size
    if ptr_valid(self._tmp_dark_image) then ptr_free, self._tmp_dark_image
    self._tmp_dark_image=ptr_new(dark_image)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           API  Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function AOframe::longExposure, fullframe=fullframe
	if (not PTR_VALID(self._longexposure)) or keyword_set(fullframe) then begin
		current_dark_fname        = file_basename(self->dark_fname())
        current_badpixelmap_fname = file_basename(self->badpixelmap_fname())
        current_object_size       = self._object_size
		if file_test(self._stored_le_fname) then begin
			restore, self._stored_le_fname
			;Check whether the dark used to compute the saved LE PSF was the same....
			if used_dark_fname ne current_dark_fname then begin
				message, 'WARNING: The dark used to compute the saved LE-frame is not the same as the current dark', /info
                file_delete, self._stored_le_fname, /allow_nonexistent
                return, self->AOframe::longExposure(fullframe=fullframe)
            endif
			;Check whether the badpixelmap used to compute the saved LE PSF was the same....
			if used_badpixelmap_fname ne current_badpixelmap_fname then begin
				message, 'WARNING: The badpixelmap used to compute the saved LE-frame is not the same as the current badpixelmap', /info
                file_delete, self._stored_le_fname, /allow_nonexistent
                return, self->AOframe::longExposure(fullframe=fullframe)
            endif
			;Check whether the object size used to compute the saved LE PSF was the same....
			if used_object_size ne current_object_size then begin
				message, 'WARNING: The object size used to compute the saved LE-frame is not the same as the current object size', /info
                file_delete, self._stored_le_fname, /allow_nonexistent
                return, self->AOframe::longExposure(fullframe=fullframe)
            endif
		endif else begin
        	psf = float(readfits(self->fname(), header, /SILENT))
    		psf_le = (self->nframes() gt 1) ? total(psf, 3) / self->nframes() : psf
        	if self._dark_fname ne "" then           psf_le -= self->dark_image()         ; subtract dark
        	if obj_valid(self._badpixelmap_obj) then psf_le = self->maneggiaFrame(psf_le) ; trigrid bad pixels
			if self._object_size ne 0 then           psf_le = self->pulisceFrame(psf_le)  ; remove line noise
        	used_dark_fname         = current_dark_fname
        	used_badpixelmap_fname  = current_badpixelmap_fname
        	used_object_size        = current_object_size
    		save, psf_le, used_dark_fname, used_badpixelmap_fname, used_object_size, filename=self._stored_le_fname, /compress
    	endelse
        if keyword_set(fullframe) then return, psf_le
        roi=self->roi()
		self._longexposure = ptr_new(psf_le[roi[0]:roi[1], roi[2]:roi[3]] ,/no_copy)
	endif
    return, *(self._longexposure)
end

;
;
function AOframe::rawImage, dark_correct=dark_correct, badpixel_correct=badpixel_correct, linenoise_correct=linenoise_correct
    psf = float(readfits(self->fname(), header, /SILENT))
    psf_le = (self->nframes() gt 1) ? total(psf, 3) / self->nframes() : psf
    if keyword_set(dark_correct) then $
        if self._dark_fname ne "" then           psf_le -= self->dark_image()         ; subtract dark
    if keyword_set(badpixel_correct) then $
        if obj_valid(self._badpixelmap_obj) then psf_le = self->maneggiaFrame(psf_le) ; trigrid bad pixels
    if keyword_set(linenoise_correct) then $
		if self._object_size ne 0 then           psf_le = self->pulisceFrame(psf_le)  ; remove line noise
    return, psf_le
end

function AOframe::imagecube
    if not (PTR_VALID(self._imagecube)) THEN self->process_cube
    return, *(self._imagecube)
end

;===================================================================================
;					A O F R A M E   P R O P E R T I E S   R E Q U E S T S
;===================================================================================

function AOframe::fname
    return, self._fname
end

function AOframe::dark_fname
    return, self._dark_fname
end

;pro AOframe::set_dark_fname, dark_fname
;    self._dark_fname = dark_fname
;    self->free
;end

function AOframe::badpixelmap_fname
	return, obj_valid(self._badpixelmap_obj) ? self._badpixelmap_obj->fname() : ""
end

function AOframe::badpixelmap_obj
	return, obj_valid(self._badpixelmap_obj) ? self._badpixelmap_obj : obj_new()
end

function AOframe::header
    if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, ""
end

function AOframe::dark_header
   	if not file_test(self->dark_fname()) then begin
        message, self->dark_fname() + ' Dark file does not exist', /info
        return, ""
    endif
    if (PTR_VALID(self._dark_fitsheader)) then ptr_free, self._dark_fitsheader
    self._dark_fitsheader = ptr_new(headfits(self->dark_fname(), /SILENT), /no_copy)
    if (PTR_VALID(self._dark_fitsheader)) THEN return, *(self._dark_fitsheader) else return, ""
end

function AOframe::nframes
	return, self._nframes
end

function AOframe::frame_w
	return, self._frame_w
end

function AOframe::frame_h
	return, self._frame_h
end

function AOframe::roi
	return, self._roi
end

function AOframe::roi_w
	return, self._roi[1]-self._roi[0]+1
end

function AOframe::roi_h
	return, self._roi[3]-self._roi[2]+1
end

pro AOframe::setRoi, roi
    self._roi = roi
    self->free
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns the error messages

function AOframe::isok, cause=cause
	isok=1B
	if strtrim(self._frame_err_msg,2) ne '' then begin
		isok*=0B
		cause += self._frame_err_msg
	endif
	return, isok
end


pro AOframe::free
    if ptr_valid(self._imagecube)       then ptr_free, self._imagecube
    if ptr_valid(self._dark_image)      then ptr_free, self._dark_image
    if ptr_valid(self._longexposure)    then ptr_free, self._longexposure
    ;if ptr_valid(self._tmp_dark_image)  then ptr_free, self._tmp_dark_image ; DONT FREE IT
end

pro AOframe::Cleanup
    ptr_free, self._fitsheader
    if ptr_valid(self._dark_fitsheader) then ptr_free, self._dark_fitsheader
    if ptr_valid(self._imagecube)       then ptr_free, self._imagecube
    if ptr_valid(self._dark_image)      then ptr_free, self._dark_image
    if ptr_valid(self._longexposure)    then ptr_free, self._longexposure
    if ptr_valid(self._tmp_dark_image)  then ptr_free, self._tmp_dark_image
end

pro AOframe__define
    struct = { AOframe                  , $
        _fname          :  ""			, $
        _dark_fname     :  ""			, $
        _fitsheader     :  ptr_new()	, $
        _dark_fitsheader:  ptr_new()	, $
        _imagecube      :  ptr_new()	, $
        _longexposure	:  ptr_new()	, $
        _dark_image     :  ptr_new()	, $
        _tmp_dark_image :  ptr_new()	, $
        _badpixelmap_obj:  obj_new()	, $
        _nframes        :  0L			, $
        _frame_w        :  0L			, $
        _frame_h        :  0L			, $
        _object_size    :  0L			, $
		_roi            :  [0,0,0,0]    , $
        _stored_le_fname   :  ""	    , $
        _stored_cube_fname :  ""		, $
        _frame_err_msg     :  ""		  $
    }
end


