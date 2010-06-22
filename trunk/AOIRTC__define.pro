
;+
; AOIRTC object initialization
;-

function AOIRTC::Init, root_obj, psf_fname, dark_fname ;, pixelscale=pixelscale

	if not file_test(psf_fname) then return,0
    fitsheader = headfits(psf_fname, /SILENT)

    ; Binning
    binning = 1

    ; Pixelscale
    if not keyword_set(pixelscale) then begin 
        apertnr = long(aoget_fits_keyword(fitsheader, 'APERTNR'))
        case apertnr of
            1: pixelscale = 0.010
            2: pixelscale = 0.020
            3: pixelscale = 0.100
        else: begin
                message, 'unknown pixelscale (apertnr is'+string(apertnr)+'). Force to 0.010', /info 
                pixelscale = 0.010
              end
        endcase
    endif

    ; Detect filter:
    filter_number = long(aoget_fits_keyword(fitsheader, 'FILTRNR'))
    lambda = irtc_filter_lambda(filter_number)
    
    ; Exposure time
	exptime = float(aoget_fits_keyword(fitsheader, 'EXPTIME'))*1e-6	;in seconds
	if exptime eq 0 then message, 'PSF image exposure time not known', /info
    
    ;Framerate
	framerate = float(aoget_fits_keyword(fitsheader, 'FR-RATE')) < 66.
	if framerate eq 0 then message, 'PSF acquisition frame rate not known', /info
    framerate =  framerate < 1./exptime 
    
    ; ROI
    str = aoget_fits_keyword(fitsheader, 'DETSEC')
    temp = strsplit( strmid(str,1,strlen(str)-2), ",", /ext)
    xra = strsplit( temp[0], ":", /ext)
    yra = strsplit( temp[1], ":", /ext)
    roi = fltarr(4)
	roi[0] = xra[0]-1 ; xmin
	roi[1] = xra[1]-1 ; xmax
	roi[2] = yra[0]-1 ; ymin
	roi[3] = yra[1]-1 ; ymax
    

    ; File names
    self._centroid_fname   = filepath(root=root_obj->elabdir(), 'psfcentroid.sav')
    self._store_psd_fname  = filepath(root=root_obj->elabdir(), 'psfcentroid_psd.sav')
	self._psf_le_fname     = filepath(root=root_obj->elabdir(), 'psf_le.sav')
	self._psf_elab_fname   = filepath(root=root_obj->elabdir(), 'psf_elab.sav')
	self._sr_se_fname      = filepath(root=root_obj->elabdir(), 'sr_se.sav')
	self._profile_fname    = filepath(root=root_obj->elabdir(), 'psf_profile.sav')
	self._enc_ene_fname    = filepath(root=root_obj->elabdir(), 'psf_enc_ene.sav')

    if not self->AOpsf::Init(root_obj, psf_fname, dark_fname, pixelscale, lambda, exptime, framerate, binning=binning, ROI=roi) then return,0

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOIRTC', 'IRTC image') then return, 0
    self->AOpsf::addHelp, self
    
    return, 1
end


pro AOIRTC__define
    struct = { AOIRTC					, $
        INHERITS    AOpsf,  $
        INHERITS    AOhelp  $
    }
end
