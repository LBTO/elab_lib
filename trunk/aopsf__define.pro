
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
;-

;function AOpsf::Init, root_obj, psf_fname, dark_fname, pixelscale=pixelscale
function AOpsf::Init, root_obj, psf_fname, dark_fname, pixelscale, lambda, exptime, framerate, $
        binning=binning, roi=roi

	if not file_test(psf_fname) then begin
        message, psf_fname + ' not found', /info
        return,0
    endif
    self._fname = psf_fname
    self._fitsheader = ptr_new(headfits(self._fname, /SILENT), /no_copy)

    self._dark_fname = dark_fname
   	if not file_test(self->dark_fname()) then message, self->dark_fname() + ' Dark file does not exist', /info

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
    ; centroid is compute as arcsec from the long-exposure PSF center
	if self._framerate eq 0 then dt=1. else dt=1./self._framerate
    if not self->AOtime_series::Init(dt, fftwindow="") then return,0
 	self._norm_factor   = 1.0   ;self->pixelscale()
	self._spectra_units = 'arcsec';
	self._plots_title = root_obj->tracknum()+' centroid'

    self._threshold = -1.
    ;self._centroid_fname     = filepath(root=root_obj->elabdir(), 'psfcentroid.sav')
    ;self._store_psd_fname = filepath(root=root_obj->elabdir(), 'psfcentroid_psd.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._centroid_fname, /allow_nonexistent
        file_delete, self._store_psd_fname, /allow_nonexistent
    endif

	;Long-exposure PSF
	;self._psf_le_fname = filepath(root=root_obj->elabdir(), 'psf_le.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._psf_le_fname, /allow_nonexistent
    endif

	;PSF cube elaborated (dark-subtracted, badpixel corrected)
	;self._psf_elab_fname = filepath(root=root_obj->elabdir(), 'psf_elab.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._psf_elab_fname, /allow_nonexistent
    endif

    ;SR estimation from the PSF:
	self._sr_se = -1.
	;self._sr_se_fname = filepath(root=root_obj->elabdir(), 'sr_se.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._sr_se_fname, /allow_nonexistent
    endif

	;PSF profile evaluation:
	self._prof_binsize = 0.5
	;self._profile_fname = filepath(root=root_obj->elabdir(), 'psf_profile.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._profile_fname, /allow_nonexistent
    endif

	;Encircled energy computation:
	;self._enc_ene_fname = filepath(root=root_obj->elabdir(), 'psf_enc_ene.sav')
    if root_obj->recompute() eq 1B then begin
        file_delete, self._enc_ene_fname, /allow_nonexistent
    endif


    ;for i=0, self._nfiles-1 do begin
    ;    t_psf = readfits( psf_fname[i], header)
    ;    if i eq 0 then totpsf = t_psf * 0
    ;    totpsf += t_psf
    ;endfor
    ;self._image = ptr_new(totpsf, /no_copy)
    ;self._gaussfit = obj_new('AOgaussfit', *self._image, self._pixelscale)

    return, 1
end

pro AOpsf::addHelp, obj
    ; initialize help object and add methods and leafs
    obj->addMethodHelp, "fname()",      	"psf file name(s) [string or strarr]"
    obj->addMethodHelp, "image()",  		"psf frames [frame_w, frame_h, nframes]"
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
    obj->addMethodHelp, "longExposure()",  "long exposure [frame_w, frame_h]"
    obj->addMethodHelp, "bias()",			"bias level of the LE image"
    obj->addMethodHelp, "gaussfit()",  	"return reference to psf gaussfit object (AOgaussfit)"
    obj->addMethodHelp, "sr_se([/PLOT][ima=ima])", 	"Strehl ratio estimated from the image. Ima allows to pass an external image on which compute the SR"
    obj->addMethodHelp, "profile()", 		"radially averaged PSF profile"
    obj->addMethodHelp, "profvar()", 		"radially-computed variance of PSF image"
    obj->addMethodHelp, "prof_dist()", 	"profile distance vector in arcsec"
    obj->addMethodHelp, "prof_dist_lD()", 	"profile distance vector in lambda/D units"
    obj->addMethodHelp, "prof_binsize()", 	"radial bin size [in pixels] used in the computation of the PSF profile"
    obj->addMethodHelp, "set_binsize, binsize", "set the bin size [in pixels] used in the computation of the PSF profile"
    obj->addMethodHelp, "sym_psf()",		"radial-symmetrical PSF image"
    obj->addMethodHelp, "show_profile, [/SHOW_RMS, _EXTRA=EX]", "show PSF profile"
    obj->addMethodHelp, "enc_ene()",		"encircled energy"
    obj->addMethodHelp, "enc_ene_dist()", 	"circle radius in arcsec"
    obj->addMethodHelp, "enc_ene_dist_lD()", "circle radius in arcsec in lambda/D units"
    obj->addMethodHelp, "centroid()", 		"returns centroids of psf images in arcsec from longExposure PSF center"
    obj->addMethodHelp, "threshold()",		"returns the threshold applied to images in the computation of the centroid (float)"
    obj->addMethodHelp, "set_threshold, thr", "Sets the threshold value mentioned above"
    obj->addMethodHelp, "show_psf,WAIT=WAIT", "shows the PSF images and the centroid location. WAIT: wait in s"
    obj->AOtime_series::addHelp, obj
end

;===================================================================================
; 		P S F   I M A G E   A N D   D A R K   I M A G E   P R O C E S S I N G
;===================================================================================

pro AOpsf::compute

	if file_test(self._psf_elab_fname) then begin
		restore, self._psf_elab_fname
    endif else begin
        psf = float(readfits(self->fname(), header, /SILENT))
        for i=0L, self->nframes()-1 do begin
            if (i+1) mod 10 eq 0 then print, string(format='(%"%d of %d ")', i+1, self->nframes() )
            psf[*,*,i] = self->maneggiaFrame(psf[*,*,i], self->dark_image(), self->badPixelMap())
        endfor
        print, 'saving data...'
    	save, psf, filename=self._psf_elab_fname ;, /compress
        print, 'done'
    endelse
    self._image = ptr_new(psf, /no_copy)
end

function AOpsf::maneggiaFrame, psf_in, dark, badpixelmap
    sz = size(psf_in,/dim)
    frame_w = sz[0]
    frame_h = sz[1]
    ; subtract dark from frames
	psf = psf_in-dark
    ; determine mask = points where signal > median(frame)+3*rms(frame)
	threshold = median(psf[badpixelmap]) + 1 * rms(psf[badpixelmap])
    ;;;;self->set_threshold, threshold
    ; subtract from each column its median computed on points outside the mask
    for j=0L, frame_w-1 do begin
        mask = where(psf[j,*] lt threshold, cnt) * (~(badpixelmap[j,*]))
        psf[j,*] -= median(psf[j,mask])
    endfor


    ; subtract from each row its median computed on points outside the mask
    ;for j=0L, self->frame_h()-1 do begin
    ;	mask = where(psf[*,j,i] lt threshold, cnt)
    ;	psf[*,j,i] -= median(psf[mask,j,i])
    ;endfor
    ; remove bad pixels interpolating with neighbours
    ; TODO UNCOMMENT THIS psf = mad_correct_bad_pixel(~badpixelmap, psf)
    return, psf
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
        	;badmap = fltarr(self->frame_w(), self->frame_h())
        endelse
		;roi = self->roi()
    	;self._badpixelmap = ptr_new(badmap[ roi[0]:roi[1], roi[2]:roi[3] ],  /no_copy)
	endif
    return, *(self._badpixelmap)
end

function AOpsf::image
    if not (PTR_VALID(self._image)) THEN self->compute
    return, *(self._image)
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

;===================================================================================
;		 		L O N G - E X P O S U R E   P S F   A N A L Y S I S
;===================================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Long-exposure PSF
function AOpsf::longExposure
	current_dark_fname = file_basename(self->dark_fname())
	if file_test(self._psf_le_fname) then begin
		restore, self._psf_le_fname
		;Check whether the dark used to compute the saved LE PSF was the same....
		if used_dark_fname ne current_dark_fname then $
			message, 'WARNING: The dark used to compute the saved LE-PSF is not the same as the current dark', /info
		if n_elements(bias_level) ne 0 then self._bias_level = bias_level
        if n_elements(threshold) ne 0 then  self._threshold = threshold
	endif else begin
        psf = float(readfits(self->fname(), header, /SILENT))
    	psf_le = (self->nframes() gt 1) ? total(psf, 3) / self->nframes() : psf
        psf_le = self->maneggiaFrame(psf_le, self->dark_image(), self->badPixelMap())

        ; compute bias
        self->compute_bias, psf_le
		psf_le = psf_le - self->bias()
    	bias_level = self->bias()

        ; compute threshold
	    threshold = median(psf_le[self->badpixelmap()]) + 1.5 * rms(psf_le[self->badpixelmap()])
        self->set_threshold, threshold

        ;
        used_dark_fname = current_dark_fname
    	save, psf_le, used_dark_fname, bias_level, threshold, filename=self._psf_le_fname, /compress
    endelse
    return, psf_le
end

pro AOpsf::compute_bias, image
	; Create four boxes close to the corners of the frame
	; The bias level will be estimated in these ROIs.
	box_size = 10	;size in pixels
	pix_away = 2	;pixels away from corners
	boxes = lonarr(self._frame_w,self._frame_h)
	lowleft = [[pix_away,pix_away], [pix_away,self._frame_h-1-box_size-pix_away], $
		[self._frame_w-1-box_size-pix_away,pix_away], [self._frame_w-1-box_size-pix_away,self._frame_h-1-box_size-pix_away] ]
	for ii=0, 3 do boxes[ lowleft[0,ii]:lowleft[0,ii]+box_size-1, lowleft[1,ii]:lowleft[1,ii]+box_size-1] = 1
	idx_boxes = where(boxes)

	; Estimate bias (use median instead of mean in case isolated bad pixels exist in these ROIs):
	bias_level = median(image[idx_boxes])
	self._bias_level = bias_level
end

function AOpsf::bias
	return, self._bias_level
end

function AOpsf::shiftAndAdd
    sha = fltarr(self->frame_w(), self->frame_h())
    cc = ( self->centroid() ) / self->pixelscale() 
    for i=0L, self->nframes()-1 do begin
        imas = (self->image())[*,*,i]
        ; TODO implement subpixel shift
        sha += shift(imas,-cc[i,0], -cc[i,1])
    endfor
    sha /= self->nframes()
    return, sha 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2D Gaussian Fit
function AOpsf::gaussfit, debug=debug
    IF not OBJ_VALID(self._gaussfit) THEN  begin
        self._gaussfit = obj_new('AOgaussfit', self->longExposure(), self._pixelscale, debug=debug)
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
		endif else begin
            if not keyword_set(ima) then ima = self->longExposure()
    		psf_dl_fname = filepath( root=ao_elabdir(), $
                'psf_dl_'+strtrim(round(self->lambda()*1e9),2)+'_scale'+strtrim(round(self->pixelscale()*1e3),2)+'.sav')
    		if file_test(psf_dl_fname) then begin
        		restore, psf_dl_fname
    		endif else begin
        		psf_dl_ima = psf_dl_esposito(self->lambda(), self->pixelscale()) ; wl [m] and scala [arcsec/pixel]
        		save, psf_dl_ima, file=psf_dl_fname
    		endelse
    		sr_se = sr_esposito(ima, psf_dl_ima, self->lambda(), self->pixelscale(), plot=plot, errmsg = sresposito_err_msg)
            if sresposito_err_msg ne '' then self._aopsf_err_msg += sresposito_err_msg

    		save, sr_se, filename=self._sr_se_fname
    		self._sr_se = sr_se
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
		psfprof_dist    = bin_radius * self->pixelscale()
		psfprof_dist_lD = bin_radius * self->pixelscale_lD()
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
        self._threshold = thr
	endif else begin
        long_exp_center = (self->gaussfit())->center()
        longexp = self->longexposure()
        dx = n_elements(longexp[*,0])
        dy = n_elements(longexp[0,*])
        if ((long_exp_center[0] lt 0) or (long_exp_center[1] lt 0) or (long_exp_center[0] ge dx) or (long_exp_center[1] ge dy)) then begin
            long_exp_center[0] = dx/2
            long_exp_center[1] = dy/2
        endif
		image = self->image()
		centr = fltarr(self->nframes(),2)
		; compute centroid in a small (0.6") field centered on the long-exp PSF
		square = 0.6	; arcsec
		sz = fix(square/2./self->pixelscale())
		for ii=0L, self->nframes()-1 do begin
			im = image[*,*,ii]
			im[where(im lt self->threshold())] = 0.
            sz = fix(min([sz, long_exp_center[0], long_exp_center[1], dx-long_exp_center[0], dy-long_exp_center[1]]))
			im = im[long_exp_center[0]-sz:long_exp_center[0]+sz-1, long_exp_center[1]-sz:long_exp_center[1]+sz-1]
			;centr[ii,*] = aocalc_centroid(im) * self->pixelscale()
			r = gauss2dfit(im, coeff, /tilt)
            centr[ii,*] = (coeff[4:5]-[sz,sz]) * self->pixelscale()
		endfor
		thr = self->threshold()
		save, centr, thr, long_exp_center, file=self._centroid_fname
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

function AOpsf::threshold
    if self._threshold eq -1 then longexp = self->LongExposure()
	return, self._threshold
end

pro AOpsf::set_threshold, thr
	self._threshold = thr
    if ptr_valid(self._centroid) then ptr_free, self._centroid
    file_delete, self._centroid_fname, /allow_nonexistent
    file_delete, self._store_psd_fname, /allow_nonexistent
end

; to be implemented in AOtime_series subclasses
function AOpsf::GetDati
    if not (PTR_VALID(self._centroid)) THEN self->compute_centroid
  	return, self._centroid
end

pro AOpsf::plotJitter, from_freq=from_freq, to_freq=to_freq, _extra=ex

    freq = self->freq(from=from_freq, to=to_freq)
    tip  = self->power(0, from=from_freq, to=to_freq, /cum)
    tilt = self->power(1, from=from_freq, to=to_freq, /cum)
    plot, freq, sqrt(tip + tilt), $
        title=self._plots_title, xtitle='Freq [Hz]', ytitle='Jitter [mas]', _extra=ex
    oplot, freq, sqrt(tip), col='0000ff'x
    oplot, freq, sqrt(tilt), col='00ff00'x
    legend, ['Tilt+Tip', 'Tip', 'Tilt'],/fill,psym=[6,6,6],colors=['ffffff'x, '0000ff'x, '00ff00'x]

    sigmatot2 = max ( self->power(0, /cum)+self->power(1, /cum) ) / 2
    ldmas = self->lambda() / ao_pupil_diameter() / 4.848d-6 ; l/D in arcsec
    print, 'SR attenuation due to TT jitter ', 1. / (1. + (!pi^2 /2 )*( sqrt(sigmatot2)/ ldmas)^2)
end

;===================================================================================
;					P S F   D I S P L A Y   R O U T I N E S
;===================================================================================

pro AOpsf::show_psf, wait=wait
	if n_elements(wait) eq 0 then wait=0.01
    if not (PTR_VALID(self._centroid)) THEN self->compute_centroid
    loadct,3,/silent
    print, 'Type "s" to stop!'
    image = self->image()
    maxval = max(image)
    centroid_fr = self->centroid() / self->pixelscale() + transpose(rebin((self->gaussfit())->center(), 2, self->nframes(), /samp))
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
    return, filepath(root=file_dirname(self->dark_fname()) , 'badpixelmap.fits')
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
    if ptr_valid(self._image)       then ptr_free, self._image
    if ptr_valid(self._dark_image)  then ptr_free, self._dark_image
    if ptr_valid(self._badpixelmap) then ptr_free, self._badpixelmap
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
    if ptr_valid(self._image)      then ptr_free, self._image
    if ptr_valid(self._dark_image) then ptr_free, self._dark_image
    if ptr_valid(self._badpixelmap) then ptr_free, self._badpixelmap
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
    if self._aopsf_err_msg ne '' then begin
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
        _image          :  ptr_new()	, $
        _dark_image     :  ptr_new()	, $
        _badpixelmap    :  ptr_new()	, $
		_roi            :  [0,0,0,0]    , $
        _bias_level		:  0.			, $
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
        _threshold      :  0.			, $
        _centroid_fname :  ""			, $
        _psf_le_fname	:  ""			, $
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



