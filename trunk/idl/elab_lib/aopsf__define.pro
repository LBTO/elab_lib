
;+
; AOpsf object initialization
;
; INPUT
;   root_obj
;   psf_fname            images cube fits file name (absolute path)
;   dark_fname           dark fits file name (absolute path)
;   pixelscale           [arcsec/px]
;   lambda               [m]
;   exptime              [s]
;   framerate            [Hz]
;
; KEYWORD
;   binning              default = 1
;   roi                  array [xmin, xmax, ymin, ymax] starting from 0, boundaries included, default=entire frame
;	badpixelmap_fname	(string) full path to the bad pixel map frame.
;-

;function AOpsf::Init, root_obj, psf_fname, dark_fname, pixelscale=pixelscale
function AOpsf::Init, root_obj, psf_fname, dark_fname, pixelscale, lambda, exptime, framerate, $
        binning=binning, roi=roi, badpixelmap_fname=badpixelmap_fname

	if not file_test(psf_fname) then begin
        message, psf_fname + ' not found', /info
        return,0
    endif
    self._fname = psf_fname
    self._fitsheader = ptr_new(headfits(self._fname, /SILENT), /no_copy)

    self._dark_fname = dark_fname
   	if not file_test(self->dark_fname()) then message, self->dark_fname() + ' Dark file does not exist', /info

	if keyword_set(badpixelmap_fname) then self._badpixelmap_fname = badpixelmap_fname

    naxis = long(aoget_fits_keyword(self->header(), 'NAXIS'))
    self._frame_w  = long(aoget_fits_keyword(self->header(), 'NAXIS1'))
    self._frame_h  = long(aoget_fits_keyword(self->header(), 'NAXIS2'))
    self._nframes = (naxis eq 2) ? 1 :  long(aoget_fits_keyword(self->header(), 'NAXIS3')) ;TODO
    self._pixelscale = pixelscale
    self._lambda_im = lambda
	self._exptime = exptime
    self._framerate = framerate

    ; binning
    if not keyword_set(binning) then binning = 1
    self._binning = binning

    ; ROI
    if not keyword_set(roi) then roi = [0, self->frame_w()-1, 0, self->frame_h()-1]
    self._roi = roi

	self._pixelscale_lD = self._pixelscale / ((self->lambda()/ao_pupil_diameter())/4.848d-6)	;l/D per pixel

	; Time series for psf centroid analysis
    ; centroid is compute as PIXELS from the long-exposure PSF center
	if self._framerate eq 0 then dt=1. else dt=1./self._framerate
    if not self->AOtime_series::Init(dt, fftwindow="") then return,0
    if finite(self->pixelscale()) then begin
 		self._norm_factor   = self->pixelscale()*1e3
		self._spectra_units = 'mas'
	endif else begin
     	self._norm_factor   = 1.0
		self._spectra_units = 'pix'
	endelse
	self._plots_title = root_obj->tracknum()

    if root_obj->recompute() eq 1B then begin
        file_delete, self._centroid_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
        file_delete, self._store_peaks_fname, /allow_nonexistent
    endif

	;Long-exposure PSF
    if root_obj->recompute() eq 1B then begin
        file_delete, self._psf_le_fname, /allow_nonexistent
    endif

	;PSF cube elaborated (dark-subtracted, badpixel corrected)
    if root_obj->recompute() eq 1B then begin
        file_delete, self._psf_elab_fname, /allow_nonexistent
    endif

    ;SR estimation from the PSF:
	self._sr_se = -1.
    if root_obj->recompute() eq 1B then begin
        file_delete, self._sr_se_fname, /allow_nonexistent
    endif

	;PSF profile evaluation:
	self._prof_binsize = 0.5
    if root_obj->recompute() eq 1B then begin
        file_delete, self._profile_fname, /allow_nonexistent
    endif

	;Encircled energy computation:
    if root_obj->recompute() eq 1B then begin
        file_delete, self._enc_ene_fname, /allow_nonexistent
    endif

    return, 1
end

pro AOpsf::addHelp, obj
    ; initialize help object and add methods and leafs
    obj->addMethodHelp, "fname()",      	"psf file name(s) [string or strarr]"
    obj->addMethodHelp, "imageCube()", 		"psf frames [frame_w, frame_h, nframes]"
    obj->addMethodHelp, "longExposure()",   "long exposure [frame_w, frame_h]"
    obj->addMethodHelp, "header()",  		"fits file headers [nfilesstrarr]"
    obj->addMethodHelp, "dark_fname()", 	"psf dark file name(s) [string]"
    obj->addMethodHelp, "dark_header()", 	"psf dark fits file header  [strarr]"
    obj->addMethodHelp, "dark_image()", 	"psf dark image (float)"
    obj->addMethodHelp, "badpixelmap_fname()", 	"psf bad pixel map file name [string]"
    obj->addMethodHelp, "badpixelmap()", 	"bad pixel mask image [frame_w, frame_h]"
    obj->addMethodHelp, "nframes()", 		"number of frames saved (long)"
    obj->addMethodHelp, "frame_w()", 		"frame width [px] (long)"
    obj->addMethodHelp, "frame_h()", 		"frame height [px] (long)"
    obj->addMethodHelp, "lambda()", 		"central filter wavelength [m]"
    obj->addMethodHelp, "pixelscale()",  	"pixelscale [arcsec/px]"
    obj->addMethodHelp, "exptime()",		"psf exposure time [s]"
    obj->addMethodHelp, "binning()",		"ccd binning"
    obj->addMethodHelp, "gaussfit()",  		"return reference to psf gaussfit object (AOgaussfit)"
    obj->addMethodHelp, "sr_se([/PLOT][ima=ima])", 	"Strehl ratio estimated from the image. Ima allows to pass an external image on which compute the SR"
    obj->addMethodHelp, "profile()", 		"radially averaged PSF profile"
    obj->addMethodHelp, "profvar()", 		"radially-computed variance of PSF image"
    obj->addMethodHelp, "prof_dist()", 		"profile distance vector in arcsec"
    obj->addMethodHelp, "prof_dist_lD()", 	"profile distance vector in lambda/D units"
    obj->addMethodHelp, "prof_binsize()", 	"radial bin size [in pixels] used in the computation of the PSF profile"
    obj->addMethodHelp, "set_binsize, binsize", "set the bin size [in pixels] used in the computation of the PSF profile"
    obj->addMethodHelp, "sym_psf()",		"radial-symmetrical PSF image"
    obj->addMethodHelp, "show_profile, [/SHOW_RMS, _EXTRA=EX]", "show PSF profile"
    obj->addMethodHelp, "enc_ene()",		"encircled energy"
    obj->addMethodHelp, "enc_ene_dist()", 	"circle radius in arcsec"
    obj->addMethodHelp, "enc_ene_dist_lD()","circle radius in arcsec in lambda/D units"
    obj->addMethodHelp, "centroid()", 		"returns centroids of psf images in PIXELS from longExposure PSF center"
    obj->addMethodHelp, "show_psf,WAIT=WAIT", "shows the PSF images and the centroid location. WAIT: wait in s"
    obj->AOtime_series::addHelp, obj
end

;===================================================================================
; 		P S F   I M A G E   A N D   D A R K   I M A G E   P R O C E S S I N G
;===================================================================================

pro AOpsf::process_cube
	if file_test(self._psf_elab_fname) then begin
		restore, self._psf_elab_fname
    endif else begin
        psf = float(readfits(self->fname(), header, /SILENT))
        for i=0L, self->nframes()-1 do begin
            if (i+1) mod 10 eq 0 then print, string(format='(%"%d of %d ")', i+1, self->nframes() )
            psf[*,*,i] = self->maneggiaFrame(psf[*,*,i])
        endfor
        print, 'saving data...'
    	save, psf, filename=self._psf_elab_fname ;, /compress
        print, 'done'
    endelse
    self._imagecube = ptr_new(psf, /no_copy)
end


function AOpsf::imagecube
    if not (PTR_VALID(self._imagecube)) THEN self->process_cube
    return, *(self._imagecube)
end


function AOpsf::maneggiaFrame, psf_in
	psf = psf_in - self->dark_image()
    ; remove bad pixels interpolating with neighbours
    ;psf = mad_correct_bad_pixel(~badpixelmap, psf)
    trstr = self->triangulate()
    if trstr.np lt self->frame_w()*self->frame_h() then $
    	psf = TRIGRID(float(trstr.x), float(trstr.y), psf[trstr.idx], trstr.tr, xout=findgen(self->frame_w()), yout=findgen(self->frame_h()))
    return, psf
end


function AOpsf::pulisceFrame, psf_in
	;Mask out the PSF
	ima_fit = gauss2dfit(double(psf_in),coeff)
	xc = coeff[4]
	yc = coeff[5]
	mask = self->background_mask(xc,yc,mask_ok=mask_ok)
	if ~mask_ok then begin
		message, 'Warning: not possible to clean the PSF',/info
		return, psf_in
	endif
	psf = psf_in
	; Clean image column by column
	;-----------------------------
    for j=0L, self->frame_w()-1 do begin
    	colidx = where(mask[j,*],count)
		if count ne 0 then psf[j,*] -= median((psf[j,*])[colidx])
	endfor
	; Clean image row by row
	;-----------------------------
    for j=0L, self->frame_h()-1 do begin
    	rowidx = where(mask[*,j],count)
		if count ne 0 then psf[*,j] -= median((psf[*,j])[rowidx])
	endfor
	return, psf
end


function AOpsf::background_mask, xc, yc, nsup=nsup, borderpix=borderpix, mask_ok=mask_ok
	if n_params() ne 2 then message, 'Syntax: ...->background_mask(xc,yc)'
	if not keyword_set(nsup) then nsup = 40.	;maximum radius to ensure all star light is in.
	dsup = ao_pupil_diameter() / nsup
    if not finite(self->pixelscale()) or not finite(self->lambda()) then begin
    	mask_ok=0B
    	return, -1
    endif
	control_diam_pix = self->lambda() / dsup / 4.85e-6 / self->pixelscale()
	np = max([self->frame_w(), self->frame_h()])
	diaratio = control_diam_pix/float(np)
	if diaratio gt 1 then begin
		mask_ok=0B
		return, -1
	endif
	xcr = 2*xc/float(np-1) - 1.0
	ycr = 2*yc/float(np-1) - 1.0
	mask = make_mask(np, diaratio=diaratio, xc=xcr, yc=ycr, /inverse)
	mask = mask[0:self->frame_w()-1,0:self->frame_h()-1]
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
function AOpsf::striscia_mask, len, npr, inverted=inverted
	if n_params() ne 2 then message, 'Syntax: ...->striscia_mask(len,npr)'
	npx = self->frame_w()
	npy = self->frame_h()
	mask1 = fltarr(npx,npy)
	mask1[npr:npx-npr-1,npr:npr+len] = 1.
	mask1[npr:npx-npr-1,npy-npr-len:npy-npr-1] = 1.
	mask1[npr:npr+len,npr+len:npy-npr-len-1] = 1.
	mask1[npx-npr-len:npx-npr-1,npr+len:npy-npr-len-1] = 1.
	if keyword_set(inverted) then mask1 = float(~mask1)
	return, mask1
end


function AOpsf::badPixelMap
    if not PTR_VALID(self._badpixelmap) then begin

    	if file_test(self->badpixelmap_fname()) then begin

            badmap = readfits(self->badpixelmap_fname(), bpm_header, /SILENT)
            bmp_frame_w = long(aoget_fits_keyword(bpm_header, 'NAXIS1'))
            bmp_frame_h = long(aoget_fits_keyword(bpm_header, 'NAXIS2'))

            if (bmp_frame_w ne self->frame_w()) or (bmp_frame_h ne self->frame_h()) then begin
                message, 'BadPixelMap and PSF images do not have the same dimensions!!', /info
                self._badpixelmap = ptr_new(fltarr(self->frame_w(), self->frame_h()), /no_copy)
            endif else begin
    	        self._badpixelmap = ptr_new(badmap,  /no_copy)
            endelse

        endif else begin
        	message, 'BadPixelMap file not existing. Assume all pixel good', /info
            self._badpixelmap = ptr_new(fltarr(self->frame_w(), self->frame_h()), /no_copy)
        endelse

		;for interpolating bad pixels:
		idxvalid = long(where( *(self._badpixelmap) eq 0., nvalid))
		x_valid = idxvalid mod long(self->frame_w())
		y_valid = idxvalid  /  long(self->frame_w())
		TRIANGULATE, float(x_valid), float(y_valid), tr
		bpstr = create_struct('x', x_valid, 'y', y_valid, 'idx', idxvalid, 'np', long(nvalid), 'tr', tr)
		self._triangulate = ptr_new(bpstr, /no_copy)
	endif
    return, *(self._badpixelmap)
end


function AOpsf::triangulate
	if not PTR_VALID(self._triangulate) then badmap = self->badPixelMap()
	return, *(self._triangulate)
end


function AOpsf::dark_image
    if not (PTR_VALID(self._dark_image)) then begin
    	cube_fname = self->dark_fname()
   		if file_test(cube_fname) then begin
       		dark = float(readfits(cube_fname, dark_header, /SILENT))
    		naxis = long(aoget_fits_keyword(dark_header, 'NAXIS'))
   			dark_frame_w = long(aoget_fits_keyword(dark_header, 'NAXIS1'))
   			dark_frame_h = long(aoget_fits_keyword(dark_header, 'NAXIS2'))
   			if (dark_frame_w ne self._frame_w) or (dark_frame_h ne self._frame_h) then begin
   				message, 'Dark and PSF images do not have the same dimensions!!', /info
   				self._dark_image = ptr_new(fltarr(self._frame_w, self._frame_h))
   			endif else begin
   				dark_nframes = (naxis eq 2) ? 1 : long(aoget_fits_keyword(dark_header, 'NAXIS3'))
       			if dark_nframes gt 1 then self._dark_image = ptr_new( median(dark, dim=3) ) else $
       								  self._dark_image = ptr_new(dark)
       		endelse
       	endif else begin
       		message, 'Dark file not existing. Assuming it zero', /info
       		self._dark_image = ptr_new(fltarr(self._frame_w, self._frame_h))
       	endelse
    endif
    return, *(self._dark_image)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Long-exposure PSF
function AOpsf::longExposure
	if not PTR_VALID(self._longexposure) then begin
		current_dark_fname = file_basename(self->dark_fname())
		if file_test(self._psf_le_fname) then begin
			restore, self._psf_le_fname
			;Check whether the dark used to compute the saved LE PSF was the same....
			if used_dark_fname ne current_dark_fname then $
				message, 'WARNING: The dark used to compute the saved LE-PSF is not the same as the current dark', /info
		endif else begin
        	psf = float(readfits(self->fname(), header, /SILENT))
    		psf_le = (self->nframes() gt 1) ? total(psf, 3) / self->nframes() : psf
        	psf_le = self->maneggiaFrame(psf_le)
			psf_le = self->pulisceFrame(psf_le)
        	used_dark_fname = current_dark_fname
    		save, psf_le, used_dark_fname, filename=self._psf_le_fname, /compress
    	endelse
		self._longexposure = ptr_new(psf_le,/no_copy)
	endif
    return, *(self._longexposure)
end


;pro AOpsf::compute_bias, image
;	; Create four boxes close to the corners of the frame
;	; The bias level will be estimated in these ROIs.
;	box_size = 10	;size in pixels
;	pix_away = 2	;pixels away from corners
;	boxes = lonarr(self._frame_w,self._frame_h)
;	lowleft = [[pix_away,pix_away], [pix_away,self._frame_h-1-box_size-pix_away], $
;		[self._frame_w-1-box_size-pix_away,pix_away], [self._frame_w-1-box_size-pix_away,self._frame_h-1-box_size-pix_away] ]
;	for ii=0, 3 do boxes[ lowleft[0,ii]:lowleft[0,ii]+box_size-1, lowleft[1,ii]:lowleft[1,ii]+box_size-1] = 1
;	idx_boxes = where(boxes)
;
;	; Estimate bias (use median instead of mean in case isolated bad pixels exist in these ROIs):
;	bias_level = median(image[idx_boxes])
;	self._bias_level = bias_level
;end

;
;function AOpsf::shiftAndAdd
;    sha = fltarr(self->frame_w(), self->frame_h())
;    cc = ( self->centroid() ) / self->pixelscale()
;    for i=0L, self->nframes()-1 do begin
;        imas = (self->imageCube())[*,*,i]
;        ; TODO implement subpixel shift
;        sha += shift(imas,-cc[i,0], -cc[i,1])
;    endfor
;    sha /= self->nframes()
;    return, sha
;end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2D Gaussian Fit
function AOpsf::gaussfit, debug=debug
    IF not OBJ_VALID(self._gaussfit) THEN  begin
        self._gaussfit = obj_new('AOgaussfit', self->longExposure(), debug=debug)
        self->addleaf, self._gaussfit, 'gaussfit'
    endif
    return, self._gaussfit
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Strehl Ratio
function AOpsf::SR_se, plot=plot, ima=ima
	if (self._sr_se eq -1.) or keyword_set(plot) or keyword_set(ima) then begin
		if file_test(self._sr_se_fname) and (not keyword_set(plot)) and (not keyword_set(ima)) then begin
			restore, self._sr_se_fname
			self._sr_se = sr_se
			if n_elements(sresposito_err_msg) eq 0 then sresposito_err_msg = ''
            if strtrim(sresposito_err_msg,2) ne '' then self._aopsf_err_msg += ' - ' + sresposito_err_msg
		endif else begin
            if not keyword_set(ima) then ima1 = self->longExposure() else ima1 = ima
            if not finite(self->pixelscale()) or not finite(self->lambda()) then begin
            	message, 'pixelscale() and/or lambda() not known. SR cannot be computed',/info
            	return, 0.
            endif else begin
    			psf_dl_fname = filepath( root=ao_elabdir(), $
                	'psf_dl_'+strtrim(round(self->lambda()*1e9),2)+'_scale'+strtrim(round(self->pixelscale()*1e3),2)+'.sav')
    			if file_test(psf_dl_fname) then begin
        			restore, psf_dl_fname
    			endif else begin
        			psf_dl_ima = psf_dl_esposito(self->lambda(), self->pixelscale(), oc=ao_lbt_oc(), Dpup=ao_pupil_diameter()) ; wl [m] and scala [arcsec/pixel]
        			save, psf_dl_ima, file=psf_dl_fname
    			endelse
    			sr_se = sr_esposito(ima1, psf_dl_ima, self->lambda(), self->pixelscale(), plot=plot, errmsg = sresposito_err_msg, /FIX_BG)
				if n_elements(sresposito_err_msg) eq 0 then sresposito_err_msg = ''
            	if strtrim(sresposito_err_msg,2) ne '' then self._aopsf_err_msg += ' - ' + sresposito_err_msg
    			if not keyword_set(ima) then save, sr_se, sresposito_err_msg, filename=self._sr_se_fname
    			self._sr_se = sr_se
    		endelse
    	endelse
    endif
    return, self._sr_se
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PSF averaged profile
pro AOpsf::compute_profile
	if file_test(self._profile_fname) then begin
		restore, self._profile_fname
		self._prof_binsize = binsize
	endif else begin
		psf1 = self->longExposure()
		gauss_center = (self->gaussfit())->center()

		; Discard frame border
		peak = max( psf1[2:self._frame_w-3, 2:self._frame_h-3] )

		; Compute PSF profile
		binsize = self._prof_binsize
		radial_statistics, psf1/peak, CENTRE=gauss_center, MEAN=psfprofile, VAR=psfprofvar, $
			COUNT=histo, BINSIZE=binsize, BIN_RADIUS=bin_radius, SYM_IMAGE=sym_psf

		if finite(self->pixelscale()) then begin
			psfprof_dist    = bin_radius * self->pixelscale()
			psfprof_dist_lD = bin_radius * self->pixelscale_lD()
		endif else begin
			psfprof_dist = bin_radius
			psfprof_dist_lD = bin_radius
		endelse

		save, psfprofile, psfprofvar, psfprof_dist, psfprof_dist_lD, binsize, bin_radius, sym_psf, histo $
			, filename=self._profile_fname, /compress
	endelse
	self._psfprofile      = ptr_new(psfprofile, /no_copy)
	self._psfprofvar      = ptr_new(psfprofvar, /no_copy)
	self._psfprof_dist    = ptr_new(psfprof_dist, /no_copy)
	self._psfprof_dist_lD = ptr_new(psfprof_dist_lD, /no_copy)
	self._sym_psf		  = ptr_new(sym_psf, /no_copy)
end

function AOpsf::profile
    if not (PTR_VALID(self._psfprofile)) THEN self->compute_profile
	return, *(self._psfprofile)
end

function AOpsf::profvar
    if not (PTR_VALID(self._psfprofvar)) THEN self->compute_profile
	return, *(self._psfprofvar)
end

function AOpsf::prof_dist
    if not (PTR_VALID(self._psfprof_dist)) THEN self->compute_profile
	return, *(self._psfprof_dist)
end

function AOpsf::prof_dist_lD
    if not (PTR_VALID(self._psfprof_dist_lD)) THEN self->compute_profile
	return, *(self._psfprof_dist_lD)
end

function AOpsf::sym_psf
    if not (PTR_VALID(self._sym_psf)) THEN self->compute_profile
	return, *(self._sym_psf)
end

function AOpsf::prof_binsize
	return, self._prof_binsize
end

pro AOpsf::set_prof_binsize, binsize
	self._prof_binsize = binsize
    if ptr_valid(self._psfprofile) then ptr_free, self._psfprofile
    if ptr_valid(self._psfprofvar)  then ptr_free, self._psfprofvar
    if ptr_valid(self._psfprof_dist)    then ptr_free, self._psfprof_dist
    if ptr_valid(self._psfprof_dist_lD) then ptr_free, self._psfprof_dist_lD
    if ptr_valid(self._sym_psf)    then ptr_free, self._sym_psf
    file_delete, self._profile_fname, /allow_nonexistent
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Encircled Energy
pro AOpsf::compute_encircled_energy
	if file_test(self._enc_ene_fname) then begin
		restore, self._enc_ene_fname
	endif else begin
		psf1 = self->longExposure()
		gauss_center = (self->gaussfit())->center()

		; Discard frame border
		total_ene = total( psf1[2:self._frame_w-3, 2:self._frame_h-3] )

		; Compute encircled energy
		binsize = 1
		ee = enc_energy(psf1/total_ene, centre=gauss_center, bin_radius=bin_radius, binsize=binsize)
		ee_dist    = bin_radius * self->pixelscale()
		ee_dist_lD = bin_radius * self->pixelscale_lD()
		save, ee, ee_dist, ee_dist_lD, bin_radius, filename=self._enc_ene_fname, /compress
	endelse
	self._enc_ene         = ptr_new(ee, /no_copy)
	self._enc_ene_dist    = ptr_new(ee_dist, /no_copy)
	self._enc_ene_dist_lD = ptr_new(ee_dist_lD, /no_copy)
end

function AOpsf::enc_ene
    if not (PTR_VALID(self._enc_ene)) THEN self->compute_encircled_energy
	return, *(self._enc_ene)
end

function AOpsf::enc_ene_dist
    if not (PTR_VALID(self._enc_ene_dist)) THEN self->compute_encircled_energy
	return, *(self._enc_ene_dist)
end

function AOpsf::enc_ene_dist_lD
    if not (PTR_VALID(self._enc_ene_dist_lD)) THEN self->compute_encircled_energy
	return, *(self._enc_ene_dist_lD)
end


;===================================================================================
; 	P S F   C E N T R O I D   C O M P U T A T I O N   A N D   S T A T I S T I C S
;===================================================================================

pro AOpsf::compute_centroid
	if file_test(self._centroid_fname) then begin
        restore, self._centroid_fname
	endif else begin
        lc = (self->gaussfit())->center()	;center in pixels
		thr = 0.01 * (self->gaussfit())->ampl()
        dx = self->frame_w()
        dy = self->frame_h()
        if ((lc[0] lt 0) or (lc[1] lt 0) or (lc[0] ge dx) or (lc[1] ge dy)) then begin
            message, 'Warning: center coordinates out of range!
        endif
		image = self->imageCube()
		fwhm = fltarr(self->nframes())
		centr = fltarr(self->nframes(),2)
		; Compute size of square used to fit the 2D gaussian
		sz = round((self->gaussfit())->fwhm()*10)
        sz = fix(min([sz, lc[0], lc[1], dx-lc[0], dy-lc[1]]))
		for ii=0L, self->nframes()-1 do begin
			im = image[*,*,ii]
			im[where(im lt thr)] = 0.
			im = im[lc[0]-sz:lc[0]+sz-1, lc[1]-sz:lc[1]+sz-1]
			r = gauss2dfit(im, coeff, /tilt)
			fwhm[ii] = sqrt(coeff[2]*coeff[3]) * 2*SQRT(2*ALOG(2))
            centr[ii,*] = (coeff[4:5]-[sz,sz]) ;* self->pixelscale()
		endfor
		save, centr, fwhm, lc, thr, sz, file=self._centroid_fname
        self->free
	endelse
	self._centroid = ptr_new(centr, /no_copy)
end


;
; centroid of short-exposures in arcsec wrt to the longexp centroid (gaussfit->center())
;
function AOpsf::centroid
    if not (PTR_VALID(self._centroid)) THEN self->compute_centroid
	return, *(self._centroid)
end


; to be implemented in AOtime_series subclasses
function AOpsf::GetDati
  	return, self->centroid()
end

pro AOpsf::plotJitter, from_freq=from_freq, to_freq=to_freq, _extra=ex

    freq = self->freq(from=from_freq, to=to_freq)
    tip  = self->power(0, from=from_freq, to=to_freq, /cum) * self->norm_factor()^2.
    tilt = self->power(1, from=from_freq, to=to_freq, /cum) * self->norm_factor()^2.
    plot, freq, sqrt(tip + tilt), xgridstyle=1, ygridstyle=1, xticklen=1, yticklen=1, $
        title=self._plots_title, xtitle='Freq [Hz]', ytitle='Jitter ['+self._spectra_units+' rms]', _extra=ex
    oplot, freq, sqrt(tip), col='0000ff'x
    oplot, freq, sqrt(tilt), col='00ff00'x
    legend, ['Tilt+Tip', 'Tip', 'Tilt'],linestyle=[0,0,0],colors=[!P.COLOR, '0000ff'x, '00ff00'x],charsize=1.2

    sigmatot2 = max( self->power(0, /cum)+self->power(1, /cum) ) * self->norm_factor()^2. / 2
    ldmas = self->lambda() / ao_pupil_diameter() / 4.848d-6 * 1e3 ; l/D in mas
    print, 'SR attenuation due to TT jitter ', 1. / (1. + (!pi^2 /2 )*( sqrt(sigmatot2)/ ldmas)^2)
end

;===================================================================================
;					P S F   D I S P L A Y   R O U T I N E S
;===================================================================================

pro AOpsf::show_psf, wait=wait
	if n_elements(wait) eq 0 then wait=0.01
    loadct,3,/silent
    print, 'Type "s" to stop!'
    image = self->imageCube()
    maxval = max(image)
    centroid_fr = self->centroid() + transpose(rebin((self->gaussfit())->center(), 2, self->nframes(), /samp))
	for ii=0, self->nframes()-1 do begin
		image_show, (image)[*,*,ii]/maxval > 0.0001, /as, title='frame '+strtrim(ii,2), /log
		oplot, [centroid_fr[ii,0]], [centroid_fr[ii,1]], psym=1, symsize=2, color=0
		wait, wait
		key = get_kbrd(0.01)
		if STRLOWCASE(key) eq 's' then break
	endfor
end

pro AOpsf::show_profile, _extra=ex, show_rms=show_rms
    if not (PTR_VALID(self._psfprofile)) THEN self->compute_profile

	;airy disk:
	airysep_lD = findgen(1000)/(1000.-1.)*100.
	oc = 0.111
	sec2rad = 4.85*1.e-6
	airyprof = psf_dl(airysep_lD, OBS=oc, /PEAK)

	prof_xrange_lD = [0.1,1e2]
	prof_xrange_arcsec = prof_xrange_lD * ((self->lambda()/ao_pupil_diameter())/sec2rad)

	winsize = get_screen_size()/2
	window, /free, xsize=winsize[0], ysize=winsize[1], title='PSF profile'
	!P.MULTI = [0, 1, 2]
	!Y.MARGIN = [4,3]
	plot_oo, *self._psfprof_dist_lD, *self._psfprofile, xtitle='angular separation'+textoidl('(\lambda/D)') $
	, ytitle='normalized intensity', xrange=prof_xrange_lD, yrange=[1e-5,1], xstyle=8, _extra=ex
	if keyword_set(show_rms) then errplot, *self._psfprof_dist_lD, *self._psfprofile-sqrt(*self._psfprofvar), *self._psfprofile+sqrt(*self._psfprofvar)
	oplot, airysep_lD, airyprof, linestyle=1
	axis, xaxis=1, xtitle='angular separation (mas)', /xlog, xrange=prof_xrange_arcsec*1e3, charsize=1.2, xstyle=1
	!Y.MARGIN = [4,2]
	!P.MULTI = [2, 2, 2]
	psf_le = self->longExposure()
	image_show, psf_le/max(psf_le) > 0.0001, /as, /log, /inv, xtitle='pixels', _extra=ex
	image_show, self->sym_psf() > 0.0001, /as, /log, /inv, xtitle='pixels', _extra=ex
	!P.MULTI = 0
end


;===================================================================================
;					A O P S F   P R O P E R T I E S   R E Q U E S T S
;===================================================================================

function AOpsf::fname
    return, self._fname
end

function AOpsf::dark_fname
    return, self._dark_fname
end

function AOpsf::badpixelmap_fname
	return, self._badpixelmap_fname
end

function AOpsf::header
    if (PTR_VALID(self._fitsheader)) THEN return, *(self._fitsheader) else return, ""
end

function AOpsf::dark_header
   	if not file_test(self->dark_fname()) then begin
        message, self->dark_fname() + ' Dark file does not exist', /info
        return, ""
    endif
    self._dark_fitsheader = ptr_new(headfits(self->dark_fname(), /SILENT), /no_copy)
    if (PTR_VALID(self._dark_fitsheader)) THEN return, *(self._dark_fitsheader) else return, ""
end

function AOpsf::nframes
	return, self._nframes
end

function AOpsf::frame_w
	return, self._frame_w
end

function AOpsf::frame_h
	return, self._frame_h
end

function AOpsf::lambda
	return, self._lambda_im
end

function AOpsf::pixelscale
    return, self._pixelscale
end

function AOpsf::pixelscale_lD
    return, self._pixelscale_lD
end

function AOpsf::exptime
	return, self._exptime
end

function AOpsf::framerate
	return, self._framerate
end

function AOpsf::binning
	return, self._binning
end

function AOpsf::roi
	return, self._roi
end

pro AOpsf::free
    if ptr_valid(self._imagecube)       then ptr_free, self._imagecube
    if ptr_valid(self._dark_image)  then ptr_free, self._dark_image
    if ptr_valid(self._longexposure) then ptr_free, self._longexposure
    if ptr_valid(self._badpixelmap) then ptr_free, self._badpixelmap
    if ptr_valid(self._triangulate) then ptr_free, self._triangulate
    if ptr_valid(self._centroid)    then ptr_free, self._centroid
    IF OBJ_VALID(self._gaussfit)   then  self._gaussfit->free
    if ptr_valid(self._psfprofile) then ptr_free, self._psfprofile
    if ptr_valid(self._psfprofvar)  then ptr_free, self._psfprofvar
    if ptr_valid(self._psfprof_dist)    then ptr_free, self._psfprof_dist
    if ptr_valid(self._psfprof_dist_lD) then ptr_free, self._psfprof_dist_lD
    if ptr_valid(self._sym_psf)     then ptr_free, self._sym_psf
    if ptr_valid(self._enc_ene)    then ptr_free, self._enc_ene
    if ptr_valid(self._enc_ene_dist)    then ptr_free, self._enc_ene_dist
    if ptr_valid(self._enc_ene_dist_lD) then ptr_free, self._enc_ene_dist_lD
    self->AOtime_series::free
end

pro AOpsf::Cleanup
    ptr_free, self._fitsheader
    ptr_free, self._dark_fitsheader
    if ptr_valid(self._imagecube)      then ptr_free, self._imagecube
    if ptr_valid(self._dark_image) then ptr_free, self._dark_image
    if ptr_valid(self._longexposure) then ptr_free, self._longexposure
    if ptr_valid(self._badpixelmap) then ptr_free, self._badpixelmap
    if ptr_valid(self._triangulate) then ptr_free, self._triangulate
    if ptr_valid(self._centroid)   then ptr_free, self._centroid
    IF OBJ_VALID(self._gaussfit)   then obj_destroy, self._gaussfit
    if ptr_valid(self._psfprofile) then ptr_free, self._psfprofile
    if ptr_valid(self._psfprofvar)  then ptr_free, self._psfprofvar
    if ptr_valid(self._psfprof_dist)    then ptr_free, self._psfprof_dist
    if ptr_valid(self._psfprof_dist_lD) then ptr_free, self._psfprof_dist_lD
    if ptr_valid(self._sym_psf)    then ptr_free, self._sym_psf
    if ptr_valid(self._enc_ene)    then ptr_free, self._enc_ene
    if ptr_valid(self._enc_ene_dist)    then ptr_free, self._enc_ene_dist
    if ptr_valid(self._enc_ene_dist_lD) then ptr_free, self._enc_ene_dist_lD
    self->AOtime_series::Cleanup
    self->AOhelp::Cleanup
end

;Returns the error messages
;-----------------------------------------------------
function AOpsf::isok, cause=cause
    ; Check if SR calculation is good
    dummy = self->sr_se()
    isok=1B
    if strtrim(self._aopsf_err_msg,2) ne '' then begin
        isok*=0B
        cause += self._aopsf_err_msg
    endif
    return, isok
end


pro AOpsf__define
    struct = { AOpsf					, $
        _aopsf_err_msg  :  ""           , $
        _fname          :  ""			, $
        _dark_fname     :  ""			, $
        _fitsheader     :  ptr_new()	, $
        _dark_fitsheader:  ptr_new()	, $
        _imagecube      :  ptr_new()	, $
        _dark_image     :  ptr_new()	, $
        _badpixelmap_fname : ""			, $
        _badpixelmap    :  ptr_new()	, $
        _triangulate	:  ptr_new()	, $
		_roi            :  [0,0,0,0]    , $
        _lambda_im		:  0.			, $	 ;[meters]
        _pixelscale     :  0d			, $  ;[arcsec/pixel]
        _pixelscale_lD  :  0d			, $  ;[in lambda/D per pixel]
        _nframes        :  0L			, $
        _frame_w        :  0L			, $
        _frame_h        :  0L			, $
        _exptime	    :  0.			, $
        _framerate		:  0.			, $	  ;Hz
        _binning 		:  0.			, $
        _gaussfit       :  obj_new()	, $
        _centroid	    :  ptr_new()	, $
        _centroid_fname :  ""			, $
        _psf_le_fname	:  ""			, $
        _longexposure	:  ptr_new()	, $
        _psf_elab_fname	:  ""			, $
        _sr_se		    :  0.			, $
        _sr_se_fname    :  ""			, $
		_psfprofile	    :  ptr_new()	, $
		_psfprofvar		:  ptr_new()	, $
		_psfprof_dist	:  ptr_new()	, $	  ;in arcsec
		_psfprof_dist_lD:  ptr_new()    , $   ;in lambda/D
		_sym_psf		:  ptr_new()	, $
		_prof_binsize	:  0.			, $
		_profile_fname  :  ""			, $
		_enc_ene		:  ptr_new()	, $
		_enc_ene_dist	:  ptr_new()	, $
		_enc_ene_dist_lD:  ptr_new()	, $
		_enc_ene_fname	:  ""			, $
        INHERITS AOtime_series			  $
    }
end



