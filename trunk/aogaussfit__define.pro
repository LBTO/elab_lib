
;+
;
;-

function AOgaussfit::Init, frame, debug=debug
    self._frame = ptr_new(frame)


    self->fitta, debug=debug

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOGaussFit', 'Gaussian 2D fit of the PSF') then return, 0
    self->addMethodHelp, "fitted_frame()", "return psf frame cropped to fitted ROI"
    self->addMethodHelp, "psffit()", "return fit of psf cropped to fitted ROI. To be compared with fitted_frame()"
    self->addMethodHelp, "roi()", "return the ROI of the psf used for fitting [xmin,xmax,ymin,ymax] (px)"
    self->addMethodHelp, "fwhm()", "return psf FWHM (px)"
    self->addMethodHelp, "fwhm_max()", "return psf FWHM along ellipse major axis (px)"
    self->addMethodHelp, "fwhm_min()", "return psf FWHM along ellipse minor axis (px)"
    self->addMethodHelp, "center()",   "return psf gaussian center int[2] (px)"
    self->addMethodHelp, "ampl()", "return psf gaussian amplitude (cnts)"
    self->addMethodHelp, "background()", "return psf gaussian background (cnts)"
    self->addMethodHelp, "angle()", "return rotation of gaussian from x axis (rad, ccw)"
    self->addMethodHelp, "ecc()",  "return psf eccentricity"
    self->addMethodHelp, "coeff()",  "return gauss2dfit coefficients"

    return, 1
end

pro AOgaussfit::fitta, debug=debug
    if keyword_set(debug) then ddd=1 else ddd=0

	fr = reform(*self._frame)
    if test_type(fr, /real, dim_size=dim) then begin
        message, 'Wrong input type'
    endif
    if dim[0] ne 2 then message, 'Wrong input format must be 2D'

	;width (x) and height (y) of image
    w = dim[1]
    h = dim[2]

	; Discard borders 2 rows and 2 columns
	fr[[0,1,w-2,w-1],*] = 0
	fr[*,[0,1,h-2,h-1]] = 0
;   fr = fr[2:w-3, 2:h-3]
;	w1 = n_elements(fr[*,0])
;	h1 = n_elements(fr[0,*])

	; Find a first estimate of the PSF center: the position of the max...
	; If you fear hot-spots just smooth:
	toten = total(fr)
    psfmax = max(smooth(fr,5),idx)
    xmax = idx mod w
    ymax = long(idx) / w
    if (ddd) then begin
    	print, 'PSF max at ' , xmax, ymax
     	print, " total energy = ", toten
    endif

	; Get a subarray containing the PSF image
    ;teeprev = 0.0
    sz = max([w,h])
    teev = dblarr(sz)
    for i=0L,sz-1 do begin
        xint = [0 > (xmax-i), (xmax+i) < (w-1)]
        yint = [0 > (ymax-i), (ymax+i) < (h-1)]
        teev[i] = total(fr[xint[0]:xint[1], yint[0]:yint[1]])
        ;if (tee/teeprev lt 1.002) then break
        ;teeprev = tee
    endfor
    i=min(where(teev gt 0.95*max(teev)))
    xint = [0 > (xmax-i), (xmax+i) < (w-1)]
    yint = [0 > (ymax-i), (ymax+i) < (h-1)]
    tee = teev[i]
      
    if (ddd) then  print, 'fitting subframe ', xint[0], xint[1], yint[0], yint[1], tee/toten

	; Fit a 2D Gaussian
    rfr = fr[xint[0]:xint[1], yint[0]:yint[1]]
    rfrfit = gauss2dfit(rfr,coeff,/tilt)
    coeff[4:5] += [xint[0], yint[0]]

	; Save all Gaussian-fit parameters:
    self._center     = coeff[4:5]
    self._fwhm_max   = coeff[2] * 2*SQRT(2*ALOG(2)) ;* self->pixelscale()
    self._fwhm_min   = coeff[3] * 2*SQRT(2*ALOG(2)) ;* self->pixelscale()
    self._fwhm   	 = sqrt(self._fwhm_max * self._fwhm_min)
    self._ampl       = coeff[1]
    self._ecc        = sqrt(1d - ( min([coeff[3], coeff[2]]) / max([coeff[3], coeff[2]]) )^2 )
    self._angle  	 = coeff[6]
    self._background = coeff[0]
    self._coeff  	 = coeff
    self._roi    	 = [xint[0], xint[1], yint[0], yint[1]]
    self._psffit	 = ptr_new(rfrfit, /no_copy)
end

function AOgaussfit::center
    return, self._center
end

function AOgaussfit::cx
	return, (self->center())[0]
end

function AOgaussfit::cy
	return, (self->center())[1]
end

function AOgaussfit::fwhm
    return, self._fwhm
end

function AOgaussfit::fwhm_max
    return, self._fwhm_max
end

function AOgaussfit::fwhm_min
    return, self._fwhm_min
end

function AOgaussfit::ampl
    return, self._ampl
end

function AOgaussfit::ecc
    return, self._ecc
end

function AOgaussfit::angle
    return, self._angle
end

function AOgaussfit::background
    return, self._background
end

function AOgaussfit::coeff
    return, self._coeff
end

function AOgaussfit::roi
    return, self._roi
end

function AOgaussfit::frame
    return, (*self._frame)
end

function AOgaussfit::fitted_frame
    roi = self->roi()
    return, (*self._frame)[roi[0]:roi[1], roi[2]:roi[3] ]
end

function AOgaussfit::psffit
    return, *self._psffit
end

pro AOgaussfit::free
	;if ptr_valid(self._frame) then ptr_free, self._frame
end

pro AOgaussfit::Cleanup
	if ptr_valid(self._frame) then ptr_free, self._frame
    if ptr_valid(self._psffit) then ptr_free, self._psffit
    self->AOhelp::Cleanup
end

pro AOgaussfit__define
    struct = { AOgaussfit, $
        _frame         : ptr_new(), $
        _psffit        : ptr_new(), $
        _center        : fltarr(2), $
        _fwhm          : 0d, $				;arcsec
        _fwhm_max      : 0d, $				;arcsec
        _fwhm_min      : 0d, $				;arcsec
        _ampl          : 0d, $
        _ecc           : 0d, $
        _angle         : 0d, $
        _background    : 0d, $
        _coeff         : fltarr(7), $
        _roi           : lonarr(4), $
        INHERITS AOhelp $
    }
end
