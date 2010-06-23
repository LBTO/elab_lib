
;+
; AOTV (CCD47) object initialization
;-

function AOTV::Init, root_obj, psf_fname, dark_fname

	if not file_test(psf_fname) then return,0
    fitsheader = headfits(psf_fname, /SILENT)

    ; Binning
    binning = long(aoget_fits_keyword(fitsheader, 'ccd47.BINNING'))

    ; Pixelscale
    pixelscale = 15./1024/binning

    ; lambda:
    if obj_valid( (root_obj->wfs_status())->filtw2() ) then $
    lambda = ((root_obj->wfs_status())->filtw2())->cw() * 1d-9 $ 
    else begin
        lambda = 800d-9
        message, 'Unknown filter on FW2. Setting central wavelength for TV to 800nm', /info
    endelse 
    
    ;Framerate
	framerate = float(aoget_fits_keyword(fitsheader, 'ccd47.FRAMERATE')) 
	if framerate eq 0 then message, 'PSF acquisition frame rate not known', /info
    
    ; Exposure time
    exptime =   1./framerate 
    
    ; ROI
    ;str = aoget_fits_keyword(self->header(), 'DETSEC')
    ;temp = strsplit( strmid(str,1,strlen(str)-2), ",", /ext)
    ;xra = strsplit( temp[0], ":", /ext)
    ;yra = strsplit( temp[1], ":", /ext)
	;roi[0] = xra[0]-1 ; xmin
	;roi[1] = xra[1]-1 ; xmax
	;oi[2] = yra[0]-1 ; ymin
	;roi[3] = yra[1]-1 ; ymax


    self._centroid_fname   = filepath(root=root_obj->elabdir(), 'tv_psfcentroid.sav')
    self._store_psd_fname  = filepath(root=root_obj->elabdir(), 'tv_psfcentroid_psd.sav')
	self._psf_le_fname     = filepath(root=root_obj->elabdir(), 'tv_psf_le.sav')
	self._psf_elab_fname   = filepath(root=root_obj->elabdir(), 'tv_psf_elab.sav')
	self._sr_se_fname      = filepath(root=root_obj->elabdir(), 'tv_sr_se.sav')
	self._profile_fname    = filepath(root=root_obj->elabdir(), 'tv_psf_profile.sav')
	self._enc_ene_fname    = filepath(root=root_obj->elabdir(), 'tv_psf_enc_ene.sav')

    if not self->AOpsf::Init(root_obj, psf_fname, dark_fname, pixelscale, lambda, exptime, framerate, binning=binning) then return,0

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOTV', 'TV CCD47 image') then return, 0
    self->AOpsf::addHelp, self

    return, 1
end


pro AOTV__define
    struct = { AOTV					, $
        INHERITS    AOpsf,  $
        INHERITS    AOhelp  $
    }
end
