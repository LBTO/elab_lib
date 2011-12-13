
;+
; aoscientificimage object initialization
; Has several (1+) objects in the field
; Corresponds to a pointing in the sky
; Can be subframed
; Has a pixelscale, a lambda, an exptime mad a framerate
; Want to maintain the original behaviour/interface of aopsf class (irtc)
; So the brightest-star's properties are directly accessible (inherits aopsfabstract)
; Other detected stars object (aopsf) are obtained with psfs() method 
; 
;
; store_radix          (string) example =filepath(root=root_obj->elabdir(), 'irtc')    
;
;-

function aoscientificimage::Init, root_obj, psf_fname, dark_fname, pixelscale, lambda, exptime, framerate, $
            badpixelmap_fname=badpixelmap_fname, recompute=recompute, store_radix=store_radix


    if not file_test(psf_fname) then begin
        message, psf_fname + ' not found', /info
        return,0
    endif

	badpixelmap_obj = keyword_set(badpixelmap_fname) ? getbadpixelmap(badpixelmap_fname) : 0

    self._exptime    = exptime      ; s
    self._recompute  = keyword_set(recompute)

    ; File names
    self._store_radix = keyword_set(store_radix) ? store_radix : filepath(root=getenv('IDL_TMPDIR'), 'aoscientificimageabstract')
	store_le_fname      = self._store_radix+'_le.sav'
	store_cube_fname    = self._store_radix+'_cube.sav'
	
	; initialize PSF object
    if not self->AOpsfAbstract::Init(psf_fname, dark_fname, pixelscale, lambda, framerate, $
    	badpixelmap_obj=badpixelmap_obj, label=root_obj->tracknum(), $
        store_radix= self._store_radix,  recompute=recompute) then return,0

    ; this is useful since we don't know NOW where the brightest star is
    self._knowwherethestaris = 0B

	return,1
end

pro AOscientificimage::addHelp, obj
    self->AOpsfAbstract::addHelp, obj
    obj->addMethodHelp, "exptime()",		"exposure time [s]"
    obj->addMethodHelp, "psfs([idx])",		"reference to PSF objects (stars) detected in the frame [objarr]"
    obj->addMethodHelp, "nstars()",		    "number of PSF objects (stars) detected in the frame"
    obj->addMethodHelp, "star_offset_sky([idx])",		"coords of detected stars on sky (offset from frame center, arcsec) [offx,offy]"
    obj->addMethodHelp, "star_position_px([idx])",		"coords of detected stars in the frame [x,y]"
    obj->addMethodHelp, "star_fwhm([idx])",		"fwhm of detected stars"
    obj->addMethodHelp, "star_sr([idx])",		"SR of detected stars"
end 

pro aoscientificimage::summary
    if self._knowwherethestaris eq 0B then ima=self->longexposure()
    self->AOpsfAbstract::summary
    print, string(format='(%"%-30s %f")','exptime [s]', self->exptime() )
    print, string(format='(%"%-30s %d")','nstars', self->nstars() )
end

;;;;;;;;;;;;;;;;;;;; PRIVATE ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNCTIONS FOR PRIVATE USE IN THIS CLASS. INTERFACE AND IMPLEMENTATION CAN CHANGE !!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; On initialization need to determine where the brightest star is in the frame:
; search peak in the fullframe image and select a roi around it
function aoscientificimage::longexposure, _extra=ex
    if self._knowwherethestaris eq 0B then begin
        ;message, 'looking for brightest star ' ,/info
        ima = self->AOframe::longexposure(/fullframe)
        ; TODO use find of astrolib?
        pk=max(ima, idxmax)
        xc = idxmax mod n_elements(ima[*,0])
        yc = idxmax / n_elements(ima[*,0])

        ; TODO FQP convert with pixelscale to 2-3 arcsec of side
        roi = fltarr(4)
        roi[0] = (xc-60)>0                 ; xmin
        roi[1] = (xc+59) < (self->frame_w()-1) ; xmax
        roi[2] = (yc-60)>0                 ; ymin
        roi[3] = (yc+59) < (self->frame_h()-1) ; ymax
    
        self->setroi, roi
        self._knowwherethestaris = 1B
    endif
    return, self->AOframe::longexposure(_extra=ex)
end


pro aoscientificimage::findstars, hmin=hmin
    ; free self._psfs and other stuff before
    if ptr_valid(self._psfs) then begin
        for i=0, self->nstars()-1 do obj_destroy, self->psfs(i)
        ptr_free, self._psfs
    endif
    if ptr_valid(self._xx)   then ptr_free, self._xx
    if ptr_valid(self._yy)   then ptr_free, self._yy
    if ptr_valid(self._flux) then ptr_free, self._flux
    ; analyze fullframe image and find star
    ima = self->AOframe::longexposure(/fullframe)
    ima_w = n_elements(ima[*,0])
    ima_h = n_elements(ima[0,*])
    if not keyword_set(hmin) then begin 
        hmin = median(ima) + 6*stddev(ima);  TODO BOH? ultragrezzo
    endif
    fwhm = self->lambda() / ao_pupil_diameter() / 4.85e-6 / self->pixelscale()  ;  in pixels
    roundlim = [-1.0,1.0]
    sharplim = [0.2,1.0]
    find, ima, xx, yy, flux, sharpness, roundness, hmin, fwhm, roundlim, sharplim, /SILENT
    ord=reverse(sort(flux))
    flux=flux[ord] & xx=xx[ord] & yy=yy[ord] ; flux descending
    self._nsources = n_elements(ord)  
    self._xx   = ptr_new(xx)
    self._yy   = ptr_new(yy)
    self._flux = ptr_new(flux)
    ; for every detected object instantiate an aopsf object
    self._psfs = ptr_new(objarr(self._nsources))
    for i=0, self->nstars()-1 do begin
        roi = [ (xx[i]-60)>0, (xx[i]+59)<(ima_w-1), (yy[i]-60)>0, (yy[i]+59)<(ima_h-1)] ;TODO ROI SIZE parametrizzato
        objpsf = obj_new('aopsf', self->fname(), self->dark_fname(), self->pixelscale(), self->lambda(), $
             self->framerate(), ROI=roi, badpixelmap_obj=self->badpixelmap_obj(), recompute=self._recompute, $ 
             store_radix=self._store_radix+'_psf'+string(format='(%"%d")',i) ) 

        (*self._psfs)[i]=objpsf
    endfor 
end



;;;;;;;;;;;;;;;;;;;; API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; exposure time [s]
function aoscientificimage::exptime
    return, self._exptime
end

; field of view [arcsec]
function aoscientificimage::fov
    return, self._fov
end

; north direction in the image clockwise, 0 when bottom to top [degrees]
function aoscientificimage::north_direction
    return, 0
end

; return center-of-field coordinates [ra, dec] [arcsec]
function aoscientificimage::pointing
    return, self._pointing
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions to access infos on detected sources in the image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; aopsf obj of detected sources in the image
function aoscientificimage::psfs, idx
    if not ptr_valid(self._psfs) then self->findstars
    return, n_elements(idx) ne 0 ? (*(self._psfs))[idx] : self._psfs
end

; number of detected sources in the image
function aoscientificimage::nstars
    if not ptr_valid(self._psfs) then self->findstars
    return, self._nsources
end

; coordinates of stars in the image [ra, dec] [arcsec]
function aoscientificimage::star_sky_coord
    
    return, 0
end

; position of stars in the image wrt the center-of-field [offset_x, offset_y] [arcsec]
function aoscientificimage::star_offset_sky
    return, 0
end

; position of stars in the image [x,y] from bottomleft corner [px]
function aoscientificimage::star_position_px, idx
    if not ptr_valid(self._psfs) then self->findstars
    if n_elements(idx) eq 0 then idx=indgen(n_elements(*(self._xx)))
    return, (transpose([[ *(self._xx)],[*(self._yy)]]))[*,idx]
end

; FWHM of stars in the image [arcsec]
function aoscientificimage::star_fwhm
    fwhm=fltarr(self->nstars())
    for i=0, self->nstars()-1 do begin
        fwhm[i] = ((self->psfs(i))->gaussfit())->fwhm()
    endfor
    return, fwhm*self->pixelscale()
end

; SR of stars in the image [arcsec]
function aoscientificimage::star_sr
    sr=fltarr(self->nstars())
    for i=0, self->nstars()-1 do begin
        sr[i] = (self->psfs(i))->sr_se()
    endfor
    return, sr
end

; peak of stars in the image [counts]
function aoscientificimage::star_peak
    if not ptr_valid(self._psfs) then self->findstars
    return, *(self._flux)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro AOscientificimage::set_dark_image, dark_image
    self->free
    self._knowwherethestaris = 0
    self->AOpsfAbstract::set_dark_image, dark_image
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns the error messages
function aoscientificimage::isok, cause=cause
	isok=1B
    isok *= self->AOpsfAbstract::isok(cause=cause)
	if strtrim(self._scientificimage_err_msg,2) ne '' then begin
		isok*=0B
		cause += self._scientificimage_err_msg
	endif
	return, isok
end

pro aoscientificimage::free
    if ptr_valid(self._psfs) then begin
        for i=0, self->nstars()-1 do obj_destroy, self->psfs(i)
        ptr_free, self._psfs
    endif
    if ptr_valid(self._xx)   then ptr_free, self._xx
    if ptr_valid(self._yy)   then ptr_free, self._yy
    if ptr_valid(self._flux) then ptr_free, self._flux
    
    self->AOpsfAbstract::free
end

pro aoscientificimage::Cleanup
    if ptr_valid(self._psfs) then begin
        for i=0, self->nstars()-1 do obj_destroy, self->psfs(i)
        ptr_free, self._psfs
    endif
    if ptr_valid(self._xx)   then ptr_free, self._xx
    if ptr_valid(self._yy)   then ptr_free, self._yy
    if ptr_valid(self._flux) then ptr_free, self._flux
    self->AOpsfAbstract::Cleanup
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro aoscientificimage__define
    struct = { aoscientificimage		, $
        _exptime            : 0.0       , $  ; exptime [s]
        _fov                : 0.0       , $  ; fov [arcsec]
        _nsources           : 0L        , $  ; number of detected sources in the image
        _psfs               : ptr_new() , $  ; pointer to an array of psf objects detected in the frame
        _xx                 : ptr_new() , $  ; xx coords of objects detected in the frame [px]
        _yy                 : ptr_new() , $  ; yy coords of objects detected in the frame [px]
        _flux               : ptr_new() , $  ; flux of objects detected in the frame [counts]
        _recompute          : 0L        , $  ;
        _knowwherethestaris : 0B        , $  ;
        _store_radix        : ""        , $  ;
        _scientificimage_err_msg  : ""  , $  ;
        INHERITS    AOpsfAbstract         $  ;  
    }
end


