
;+
; AOpsfAbstract object initialization
;
; An AOpsfAbstract represent a ROI of an image containing 1 and only 1 PSF in the field
; You can compute PSF quality parameters (SR, EE, FWHM)
; You can measure spectra of PSF vibrations (provided the image is a cube of images)
;
; The image has a lambda and a pixelscale
; In case of a cube of images it has also a framerate
;
; The FoV is small: the image should contain only one object
;
;
; INPUT
;   psf_fname            images cube fits file name (absolute path)
;   dark_fname           dark fits file name (absolute path)
;   pixelscale           [arcsec/px]
;   lambda               [m]
;   framerate            [Hz]
;
; KEYWORD
;   roi                  Use a subarray [xmin, xmax, ymin, ymax] starting from 0, boundaries included, default=entire frame
;	badpixelmap_fname	 (string) full path to the bad pixel map frame.
;   label                (string) label for plots
;   recompute            set to force recomputing of stored data
;   store_radix          (string) example =filepath(root=root_obj->elabdir(), 'irtc')
;-

function AOpsfAbstract::Init, psf_fname, dark_fname, pixelscale, lambda, framerate, $
        roi=roi, badpixelmap_obj=badpixelmap_obj, label=label, store_radix=store_radix, recompute=recompute

	if not file_test(psf_fname) then begin
        message, psf_fname + ' not found', /info
        return,0
    endif

    self._pixelscale = pixelscale
    self._lambda = lambda
    self._framerate = framerate

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
	self._plots_title = keyword_set(label) ? label : ''

	self._sr_se = -1.  ;SR estimation from the PSF:
	self._prof_binsize = 0.5 ;PSF profile evaluation:

    nsup = 40.	;maximum radius to ensure all star light is in.
	halosizepx = nsup * lambda / ao_pupil_diameter() / 4.85e-6 / pixelscale

  self._enc_ene_binsize = 1.

    ; File names
    if not keyword_set(store_radix) then store_radix = filepath(root=getenv('IDL_TMPDIR'), 'psfabstract')
    self._stored_centroid_fname = store_radix+'_centroid.sav'
    self._store_psd_fname       = store_radix+'_centroid_psd.sav'      ;aotime_series
    self._store_peaks_fname     = store_radix+'_centroid_peaks.sav'    ;aotime_series
	self._stored_le_fname       = store_radix+'_le.sav'                ;aoframe
	self._stored_cube_fname     = store_radix+'_cube.sav'              ;aoframe
	self._stored_sr_se_fname    = store_radix+'_sr_se.sav'
	self._stored_profile_fname  = store_radix+'_profile.sav'
	self._stored_enc_ene_fname  = store_radix+'_enc_ene.sav'



    if keyword_set(recompute) eq 1B then self->deletestoredfiles

    ;initialize AOframe object
    if not self->AOframe::Init(psf_fname, dark_fname=dark_fname, badpixelmap_obj=badpixelmap_obj, roi=roi, $
        line_noise=halosizepx, stored_le=store_le_fname, stored_cube=store_cube_fname, recompute=recompute) then return, 0

    return, 1
end

pro AOpsfAbstract::deletestoredfiles
    if self._store_psd_fname       ne '' then file_delete, self._store_psd_fname, /allow_nonexistent
    if self._store_peaks_fname     ne '' then file_delete, self._store_peaks_fname, /allow_nonexistent
    if self._stored_sr_se_fname    ne '' then file_delete, self._stored_sr_se_fname, /allow_nonexistent
    if self._stored_centroid_fname ne '' then file_delete, self._stored_centroid_fname, /allow_nonexistent
    if self._stored_profile_fname  ne '' then file_delete, self._stored_profile_fname, /allow_nonexistent
    if self._stored_enc_ene_fname  ne '' then file_delete, self._stored_enc_ene_fname, /allow_nonexistent ;Encircled energy computation:
end

pro AOpsfAbstract::addHelp, obj
    obj->AOframe::addHelp, obj
    obj->addMethodHelp, "lambda()", 		"central filter wavelength [m]"
    obj->addMethodHelp, "pixelscale()",  	"pixelscale [arcsec/px]"
    obj->addMethodHelp, "framerate()",  	"framerate [Hz]"
    obj->addMethodHelp, "gaussfit()",  		"return reference to psf gaussfit object (AOgaussfit)"
    obj->addMethodHelp, "sr_se([/PLOT][ima=ima])", 	"Strehl ratio estimated from the image. Ima allows to pass an external image on which compute the SR"
    obj->addMethodHelp, "profile()", 		"radially averaged PSF profile"
    obj->addMethodHelp, "profvar()", 		"radially-computed variance of PSF image"
    obj->addMethodHelp, "prof_dist()", 		"profile distance vector in arcsec"
    obj->addMethodHelp, "prof_dist_lD()", 	"profile distance vector in lambda/D units"
    obj->addMethodHelp, "prof_binsize()", 	"radial bin size [in pixels] used in the computation of the PSF profile"
;    obj->addMethodHelp, "set_binsize, binsize", "set the bin size [in pixels] used in the computation of the PSF profile"
    obj->addMethodHelp, "sym_psf()",		"radial-symmetrical PSF image"
    obj->addMethodHelp, "show_profile, [/SHOW_RMS, _EXTRA=EX]", "show PSF profile"
    obj->addMethodHelp, "enc_ene()",		"encircled energy"
    obj->addMethodHelp, "enc_ene_dist()", 	"circle radius in arcsec"
    obj->addMethodHelp, "enc_ene_dist_lD()","circle radius in lambda/D units"
    obj->addMethodHelp, "enc_ene_binsize()","radial bin size [in pixels] used in the computation of EE"
    obj->addMethodHelp, "centroid()", 		"returns centroids of psf images in PIXELS from longExposure PSF center"
    obj->addMethodHelp, "plotjitter, from_freq=from_freq, to_freq=to_freq",  "Plots TT cum PSDs"
    obj->addMethodHelp, "replay,WAIT=WAIT", "shows the PSF images and the centroid location. WAIT: wait in s"
    obj->AOtime_series::addHelp, obj
end

pro AOpsfAbstract::summary
    self->AOframe::summary
    print, string(format='(%"%-30s %f")','lambda [um]', self->lambda()*1e6 )
    print, string(format='(%"%-30s %f")','pixelscale [arcsec/px]', self->pixelscale() )
end


;===================================================================================
; 		P S F   I M A G E   A N D   D A R K   I M A G E   P R O C E S S I N G
;===================================================================================


; TODO Override AOframe::background_mask
function AOpsfAbstract::background_mask, xc, yc, nsup=nsup, borderpix=borderpix, mask_ok=mask_ok
	if n_params() ne 2 then message, 'Syntax: ...->background_mask(xc,yc)'
	if not keyword_set(nsup) then nsup = 40.	;maximum radius to ensure all star light is in.
 	dsup = ao_pupil_diameter() / nsup
     if not finite(self->pixelscale()) or not finite(self->lambda()) then begin
     	mask_ok=0B
     	return, -1
     endif
 	control_diam_pix = self->lambda() / dsup / 4.85e-6 / self->pixelscale()
 	np = max([self->frame_w(), self->frame_h()])
    if np mod 2 eq 1 then np = np+1
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

;pro AOpsfAbstract::compute_bias, image
;	; Create four boxes close to the corners of the frame
;	; The bias level will be estimated in these ROIs.
;	box_size = 10	;size in pixels
;	pix_away = 2	;pixels away from corners
;	boxes = lonarr(self->frame_w(),self->frame_h())
;	lowleft = [[pix_away,pix_away], [pix_away,self->frame_h()-1-box_size-pix_away], $
;		[self->frame_w()-1-box_size-pix_away,pix_away], [self->frame_w()-1-box_size-pix_away,self->frame_h()-1-box_size-pix_away] ]
;	for ii=0, 3 do boxes[ lowleft[0,ii]:lowleft[0,ii]+box_size-1, lowleft[1,ii]:lowleft[1,ii]+box_size-1] = 1
;	idx_boxes = where(boxes)
;
;	; Estimate bias (use median instead of mean in case isolated bad pixels exist in these ROIs):
;	bias_level = median(image[idx_boxes])
;	self._bias_level = bias_level
;end

;
;function AOpsfAbstract::shiftAndAdd
;    sha = fltarr(self->roi_w(), self->roi_h())
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
function AOpsfAbstract::gaussfit, debug=debug
    IF not OBJ_VALID(self._gaussfit) THEN  begin
        self._gaussfit = obj_new('AOgaussfit', self->longExposure(), debug=debug)
        self->addleaf, self._gaussfit, 'gaussfit'
    endif
    return, self._gaussfit
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Strehl Ratio
function AOpsfAbstract::SR_se, plot=plot, ima=ima, psf_dl_ima=psf_dl_ima
	if (self._sr_se eq -1.) or keyword_set(plot) or keyword_set(ima) then begin
		if file_test(self._stored_sr_se_fname) and (not keyword_set(plot)) and (not keyword_set(ima)) then begin
			restore, self._stored_sr_se_fname
			self._sr_se = sr_se
			if n_elements(sresposito_err_msg) eq 0 then sresposito_err_msg = ''
            if strtrim(sresposito_err_msg,2) ne '' then self._aopsf_err_msg += ' - ' + sresposito_err_msg
		endif else begin
            if not keyword_set(ima) then ima1 = self->longExposure() else ima1 = ima
            if not finite(self->pixelscale()) or not finite(self->lambda()) then begin
            	message, 'pixelscale() and/or lambda() not known. SR cannot be computed',/info
            	return, 0.
            endif else begin
                        if not keyword_set(psf_dl_ima) then begin
    			    psf_dl_fname = filepath( root=ao_elabdir(), $
                	    'psf_dl_'+strtrim(round(self->lambda()*1e9),2)+'_scale'+strtrim(round(self->pixelscale()*1e3),2)+'_oc'+strtrim(round(ao_pupil_oc()*1e3),2)+'.sav')
    			    if file_test(psf_dl_fname) then begin
        			    restore, psf_dl_fname
    			    endif else begin
        			    psf_dl_ima = psf_dl_esposito(self->lambda(), self->pixelscale(), oc=ao_pupil_oc(), Dpup=ao_pupil_diameter()) ; wl [m] and scala [arcsec/pixel]
        			    save, psf_dl_ima, file=psf_dl_fname
    			    endelse
                        endif
                        psf_dl_temp = psf_dl_ima
    			sr_se = sr_esposito(ima1, psf_dl_temp, plot=plot, errmsg = sresposito_err_msg, /FIX_BG)
				if n_elements(sresposito_err_msg) eq 0 then sresposito_err_msg = ''
            	if strtrim(sresposito_err_msg,2) ne '' then self._aopsf_err_msg += ' - ' + sresposito_err_msg
    			if not keyword_set(ima) then save, sr_se, sresposito_err_msg, filename=self._stored_sr_se_fname
    			self._sr_se = sr_se
    		endelse
    	endelse
    endif
    return, self._sr_se
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                  PSF averaged profile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro AOpsfAbstract::compute_profile
	if file_test(self._stored_profile_fname) then begin
		restore, self._stored_profile_fname
		self._prof_binsize = binsize
	endif else begin
		psf1 = self->longExposure()
		gauss_center = (self->gaussfit())->center()

		; Discard frame border
		peak = max( psf1[2:self->roi_w()-3, 2:self->roi_h()-3] )

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
			, filename=self._stored_profile_fname, /compress
	endelse
	self._psfprofile      = ptr_new(psfprofile, /no_copy)
	self._psfprofvar      = ptr_new(psfprofvar, /no_copy)
	self._psfprof_dist    = ptr_new(psfprof_dist, /no_copy)
	self._psfprof_dist_lD = ptr_new(psfprof_dist_lD, /no_copy)
	self._sym_psf		  = ptr_new(sym_psf, /no_copy)
end

function AOpsfAbstract::profile
    if not (PTR_VALID(self._psfprofile)) THEN self->compute_profile
	return, *(self._psfprofile)
end

function AOpsfAbstract::profvar
    if not (PTR_VALID(self._psfprofvar)) THEN self->compute_profile
	return, *(self._psfprofvar)
end

function AOpsfAbstract::prof_dist
    if not (PTR_VALID(self._psfprof_dist)) THEN self->compute_profile
	return, *(self._psfprof_dist)
end

function AOpsfAbstract::prof_dist_lD
    if not (PTR_VALID(self._psfprof_dist_lD)) THEN self->compute_profile
	return, *(self._psfprof_dist_lD)
end

function AOpsfAbstract::sym_psf
    if not (PTR_VALID(self._sym_psf)) THEN self->compute_profile
	return, *(self._sym_psf)
end

function AOpsfAbstract::prof_binsize
	return, self._prof_binsize
end

pro AOpsfAbstract::set_prof_binsize, binsize
	self._prof_binsize = binsize
    if ptr_valid(self._psfprofile) then ptr_free, self._psfprofile
    if ptr_valid(self._psfprofvar)  then ptr_free, self._psfprofvar
    if ptr_valid(self._psfprof_dist)    then ptr_free, self._psfprof_dist
    if ptr_valid(self._psfprof_dist_lD) then ptr_free, self._psfprof_dist_lD
    if ptr_valid(self._sym_psf)    then ptr_free, self._sym_psf
    file_delete, self._stored_profile_fname, /allow_nonexistent
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                    Encircled Energy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro AOpsfAbstract::compute_encircled_energy
	if file_test(self._stored_enc_ene_fname) then begin
		restore, self._stored_enc_ene_fname
	endif else begin
		psf1 = self->longExposure()
		gauss_center = (self->gaussfit())->center()

		; Discard frame border
		total_ene = total( psf1[2:self->roi_w()-3, 2:self->roi_h()-3] )

		; Compute encircled energy
		binsize = self->enc_ene_binsize()
		ee = enc_energy(psf1/total_ene, centre=gauss_center, bin_radius=bin_radius, binsize=binsize)
		ee_dist    = bin_radius * self->pixelscale()
		ee_dist_lD = bin_radius * self->pixelscale_lD()
		save, ee, ee_dist, ee_dist_lD, bin_radius, binsize, filename=self._stored_enc_ene_fname, /compress
	endelse
	self._enc_ene         = ptr_new(ee, /no_copy)
	self._enc_ene_dist    = ptr_new(ee_dist, /no_copy)
	self._enc_ene_dist_lD = ptr_new(ee_dist_lD, /no_copy)
	self._enc_ene_binsize = binsize
end

function AOpsfAbstract::enc_ene
    if not (PTR_VALID(self._enc_ene)) THEN self->compute_encircled_energy
	return, *(self._enc_ene)
end

function AOpsfAbstract::enc_ene_dist
    if not (PTR_VALID(self._enc_ene_dist)) THEN self->compute_encircled_energy
	return, *(self._enc_ene_dist)
end

function AOpsfAbstract::enc_ene_dist_lD
    if not (PTR_VALID(self._enc_ene_dist_lD)) THEN self->compute_encircled_energy
	return, *(self._enc_ene_dist_lD)
end

function AOpsfAbstract::enc_ene_binsize
  return, self._enc_ene_binsize
end

;===================================================================================
; 	P S F   C E N T R O I D   C O M P U T A T I O N   A N D   S T A T I S T I C S
;===================================================================================

pro AOpsfAbstract::compute_centroid
	if file_test(self._stored_centroid_fname) then begin
        restore, self._stored_centroid_fname
	endif else begin
        lc = (self->gaussfit())->center()	;center in pixels
		thr = 0.01 * (self->gaussfit())->ampl()
        dx = self->roi_w()
        dy = self->roi_h()
        if ((lc[0] lt 0) or (lc[1] lt 0) or (lc[0] ge dx) or (lc[1] ge dy)) then begin
            message, 'Warning: center coordinates out of range!'
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
		save, centr, fwhm, lc, thr, sz, file=self._stored_centroid_fname
        self->free
	endelse
	self._centroid = ptr_new(centr, /no_copy)
end


;
; centroid of short-exposures in arcsec wrt to the longexp centroid (gaussfit->center())
;
function AOpsfAbstract::centroid
    if not (PTR_VALID(self._centroid)) THEN self->compute_centroid
	return, *(self._centroid)
end


; to be implemented in AOtime_series subclasses
function AOpsfAbstract::GetDati
    if not (PTR_VALID(self._centroid)) THEN self->compute_centroid
	return, self._centroid
end

pro AOpsfAbstract::plotJitter, from_freq=from_freq, to_freq=to_freq, overplot=overplot, psym=psym, _extra=ex

    freq = self->freq(from=from_freq, to=to_freq)
    tip  = self->power(0, from=from_freq, to=to_freq, /cum) * self->norm_factor()^2.
    tilt = self->power(1, from=from_freq, to=to_freq, /cum) * self->norm_factor()^2.
    if not keyword_set(overplot) then begin
        plot, freq, sqrt(tip + tilt), xgridstyle=1, ygridstyle=1, xticklen=1, yticklen=1, $
        title=self._plots_title, xtitle='Freq [Hz]', ytitle='Jitter ['+self._spectra_units+' rms]', _extra=ex
    endif else begin
        oplot, freq, sqrt(tip + tilt), psym=psym, _extra=ex
    endelse
    oplot, freq, sqrt(tip), col='0000ff'x, psym=psym, _extra=ex
    oplot, freq, sqrt(tilt), col='00ff00'x, psym=psym, _extra=ex
    legend, ['Tilt+Tip', 'Tip', 'Tilt'],linestyle=[0,0,0],colors=[!P.COLOR, '0000ff'x, '00ff00'x],charsize=1.2

    sigmatot2 = max( self->power(0, /cum)+self->power(1, /cum) ) * self->norm_factor()^2. / 2
    ldmas = self->lambda() / ao_pupil_diameter() / 4.848d-6 * 1e3 ; l/D in mas
    print, 'SR attenuation due to TT jitter ', 1. / (1. + (!pi^2 /2 )*( sqrt(sigmatot2)/ ldmas)^2)
end

;===================================================================================
;					P S F   D I S P L A Y   R O U T I N E S
;===================================================================================

pro AOpsfAbstract::replay, wait=wait
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

;+
;
; radius: radius of PSF image to show [arcsec]
; log : to plot logscale image. Log can be used to introduce threshold: e.g. log=1e-4
; peak1 : set this keyword to normalize to peak.
;-
pro AOpsfAbstract::show_psf, radius=radius, _extra=ex, log=log, peak1=peak1, psfroi=psfroi
	ps = self->pixelscale()
	lim = self->lambda()
	lp1 = self->longexposure()
    center = (self->gaussfit())->center()	;center in pixels
	xc = round(center[0])
	yc = round(center[1])

	;crop PSF
	if not keyword_set(radius) then radius = 1.0
	npix = round(radius*2./ps)
	sz  = size(lp1,/dim)
	roi = [Xc-npix/2,Xc+npix/2-1,Yc-npix/2,Yc+npix/2-1]
	while ((where(roi lt 0))[0] ne -1) do begin
		npix-=2
		roi = [Xc-npix/2,Xc+npix/2-1,Yc-npix/2,Yc+npix/2-1]
	endwhile
	psfroi = lp1[roi[0]:roi[1],roi[2]:roi[3]]
	imrange1 = [-npix/2., npix/2.] * ps

	if not keyword_set(log) then log = 0.
	if not keyword_set(peak1) then maxpsf=1. else maxpsf = max(psfroi)
;	image_show, (psfroi/maxpsf) > log, xaxis=imrange1, yaxis=imrange1, /as, /sh, log=log, charsize=1.5, _extra=ex $
;		, xtitle='arcsec', title=tracknum
end


pro AOpsfAbstract::show_profile, _extra=ex, show_rms=show_rms
    if not (PTR_VALID(self._psfprofile)) THEN self->compute_profile

	;airy disk:
	airysep_lD = findgen(1000)/(1000.-1.)*100.
	oc = ao_pupil_oc()
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

function AOpsfAbstract::lambda
	return, self._lambda
end

function AOpsfAbstract::pixelscale
    return, self._pixelscale
end

function AOpsfAbstract::pixelscale_lD
    return, self._pixelscale_lD
end

function AOpsfAbstract::framerate
	return, self._framerate
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro AOpsfAbstract::set_dark_image, dark_image
    self->free
    self->deletestoredfiles
    self->AOframe::set_dark_image, dark_image
end



pro AOpsfAbstract::free
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
    self->AOframe::free
    self->AOtime_series::free
end

pro AOpsfAbstract::Cleanup
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
    self->AOframe::Cleanup
    self->AOtime_series::Cleanup
end

;Returns the error messages
;-----------------------------------------------------
function AOpsfAbstract::isok, cause=cause
    ; Check if SR calculation is good
    dummy = self->sr_se()
    isok=1B
    if strtrim(self._aopsf_err_msg,2) ne '' then begin
        isok*=0B
        cause += self._aopsf_err_msg
    endif
    return, isok
end

pro AOpsfAbstract__define
    struct = { AOpsfAbstract			, $
        _aopsf_err_msg  :  ""           , $
        _badpixelmap_fname : ""			, $
        _lambda 		:  0.			, $	 ;[meters]
        _pixelscale     :  0d			, $  ;[arcsec/pixel]
        _pixelscale_lD  :  0d			, $  ;[in lambda/D per pixel]
        _framerate		:  0.			, $	  ;Hz
        _gaussfit       :  obj_new()	, $
        _centroid	    :  ptr_new()	, $
        _stored_centroid_fname :  ""	, $
        _sr_se		    :  0.			, $
        _stored_sr_se_fname    :  ""	, $
		_psfprofile	    :  ptr_new()	, $
		_psfprofvar		:  ptr_new()	, $
		_psfprof_dist	:  ptr_new()	, $	  ;in arcsec
		_psfprof_dist_lD:  ptr_new()    , $   ;in lambda/D
		_sym_psf		:  ptr_new()	, $
		_prof_binsize	:  0.			, $
		_stored_profile_fname  :  ""	, $
		_enc_ene		:  ptr_new()	, $
		_enc_ene_dist	:  ptr_new()	, $
		_enc_ene_dist_lD:  ptr_new()	, $
		_stored_enc_ene_fname	:  ""	, $
		_enc_ene_binsize : 0.       , $
        INHERITS AOframe     			, $
        INHERITS AOtime_series			  $
    }
end



