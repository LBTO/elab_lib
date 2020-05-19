
;+
; AOTV (CCD47) object initialization
;-

function AOTV::Init, root_obj, psf_fname, dark_fname
    ; maybe tv frames were not saved. Simply exit
    if psf_fname eq '' then return,0

	if not file_test(psf_fname) then  begin
        message, psf_fname + 'file not found', /info
        return,0
    endif
    fitsheader = headfits(psf_fname, /SILENT, errmsg=errmsg)
    if errmsg ne '' then message, psf_fname+ ': '+ errmsg, /info


    ; Binning
    binning = long(aoget_fits_keyword(fitsheader, 'ccd47.BINNING'))

    ; Pixelscale
    pixelscale = 15./1024*binning

    ; lambda:
    lambda = !VALUES.F_NAN
    if obj_valid( (root_obj->wfs_status())->filtw2() ) then begin
        lambda = ((root_obj->wfs_status())->filtw2())->cw() * 1d-9
    endif
    if not finite(lambda) then begin
        lambda = 800d-9
        message, 'Empty/Unknown filter on FW2. Setting central wavelength for TV to 800nm', /info
    endif

    ;Framerate
	framerate = float(aoget_fits_keyword(fitsheader, 'ccd47.FRAMERATE'))
	if framerate eq 0 then message, 'PSF acquisition frame rate not known', /info

    ; Exposure time
    self._exptime =   1./framerate


    ; ROI
    ;str = aoget_fits_keyword(self->header(), 'DETSEC')
    ;temp = strsplit( strmid(str,1,strlen(str)-2), ",", /ext)
    ;xra = strsplit( temp[0], ":", /ext)
    ;yra = strsplit( temp[1], ":", /ext)
	;roi[0] = xra[0]-1 ; xmin
	;roi[1] = xra[1]-1 ; xmax
	;oi[2] = yra[0]-1 ; ymin
	;roi[3] = yra[1]-1 ; ymax

    ; Dark fname
    if not arg_present(dark_fname) then begin
        dark_basename = string(aoget_fits_keyword(fitsheader, 'ccd47.DARK_FILENAME'))
        dark_subdir   =  ['wfs_calib_'+(root_obj->wfs_status())->wunit(), 'ccd47', 'backgrounds', 'bin'+strtrim(string(binning),2)]
        dark_fname = filepath(root=ao_datadir(), sub=dark_subdir, dark_basename)
    endif

    ;self._centroid_fname   = filepath(root=root_obj->elabdir(), 'tv_psfcentroid.sav')
    ;self._store_psd_fname  = filepath(root=root_obj->elabdir(), 'tv_psfcentroid_psd.sav')
    ;self._store_peaks_fname  = filepath(root=root_obj->elabdir(), 'tv_psfcentroid_peaks.sav')
	;self._psf_le_fname     = filepath(root=root_obj->elabdir(), 'tv_psf_le.sav')
	;self._psf_elab_fname   = filepath(root=root_obj->elabdir(), 'tv_psfcube_elab.sav')
	;self._sr_se_fname      = filepath(root=root_obj->elabdir(), 'tv_sr_se.sav')
	;self._profile_fname    = filepath(root=root_obj->elabdir(), 'tv_psf_profile.sav')
	;self._enc_ene_fname    = filepath(root=root_obj->elabdir(), 'tv_psf_enc_ene.sav')

    if not self->AOpsfAbstract::Init(psf_fname, dark_fname, pixelscale, lambda, framerate, $
            label=root_obj->tracknum(), store_radix= filepath(root=root_obj->elabdir(), 'tv'), $
            recompute=root_obj->recompute()) then return,0


    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOTV', 'TV CCD47 image') then return, 0
    self->addMethodHelp, "exptime()", "exposure time (s)"
    self->AOpsfAbstract::addHelp, self

    return, 1
end

function AOTV::exptime
	return, self._exptime
end

;
; Override AOframe::rawimage because CCD47 is saved with dark subtracted
;
function AOTV::rawImage, dark_correct=dark_correct, badpixel_correct=badpixel_correct, linenoise_correct=linenoise_correct
    psf = float(readfits(self->fname(), header, /SILENT))
    psf_le = (self->nframes() gt 1) ? total(psf, 3) / self->nframes() : psf
    psf_le += self->dark_image() ;; ADD DARK IMAGE TO GET REAL RAW IMAGE
    if keyword_set(dark_correct) then $
        if self._dark_fname ne "" then           psf_le -= self->dark_image()         ; subtract dark
    if keyword_set(badpixel_correct) then $
        if obj_valid(self._badpixelmap_obj) then psf_le = self->maneggiaFrame(psf_le) ; trigrid bad pixels
    if keyword_set(linenoise_correct) then $
		if self._object_size ne 0 then           psf_le = self->pulisceFrame(psf_le)  ; remove line noise
    return, psf_le
end


pro AOTV__define
    struct = { AOTV					, $
    	_exptime 			: 0.0		, $
        INHERITS    AOpsfAbstract,  $
        INHERITS    AOhelp  $
    }
end
