
;+
;
;-

function AOpupils::Init, wfs_header, wunit

	self._header = wfs_header
	hdr = *(self._header)

	pp = aoget_fits_keyword(hdr, 'sc.PUPILS')
	if pp eq "" then return,0

	; Get pupil indexes
	;-----------------------------------------------------------------------------
	pups_subdir	= ['wfs_calib_'+wunit,'ccd39','LUTs']
	pups_path   = filepath(root=ao_datadir(), sub=pups_subdir,  '') + $
				  strmid(pp,1) + path_sep()

	self._pup_tracknum = (strsplit(pp, '/', /EXTRACT))[1]


    ; test pupil files: 'pup[1234].fits' and 'pupdata.txt'
    for i=1,4 do begin
        fname=filepath(root=pups_path, 'pup'+strtrim(string(i),2)+'.fits')
	    if not file_test(fname) then begin
            message, 'File not found:'+fname, /INFO
            return, 0
        endif
    endfor
    fname=filepath(root=pups_path, 'pupdata.txt')
	if not file_test(fname) then begin
        message, 'File not found:'+fname, /INFO
        return, 0
    endif


	; Read first pupil index
    indpup_file = pups_path+'pup1.fits'
	indpup = readfits(indpup_file, /SILENT)
	nsub = n_elements(indpup)

	; Number of valid subapertures
	self._nsub = nsub

	; Read three other pupil indexes
	for i=2,4 do begin
		indpup_file = pups_path+'pup'+strtrim(i,2)+'.fits'
		indpup = [[indpup],[readfits(indpup_file, /SILENT)]]
	endfor

	self._indpup = ptr_new(indpup, /no_copy)


	; Get pupil radii and center coordinates:
	;-----------------------------------------------
	res = read_ascii(pups_path+'pupdata.txt')
	self._radius = res.field1[0,*]
	self._cx     = res.field1[1,*]
	self._cy     = res.field1[2,*]

    ; REAL MEASURED position of pupils

	self._real_radius = [ float(aoget_fits_keyword(hdr, 'pup0.DIAMETER'))/2, $
                          float(aoget_fits_keyword(hdr, 'pup1.DIAMETER'))/2, $
                          float(aoget_fits_keyword(hdr, 'pup2.DIAMETER'))/2, $
                          float(aoget_fits_keyword(hdr, 'pup3.DIAMETER'))/2]
	self._real_cx = [ float(aoget_fits_keyword(hdr, 'pup0.CX')), $
                      float(aoget_fits_keyword(hdr, 'pup1.CX')), $
                      float(aoget_fits_keyword(hdr, 'pup2.CX')), $
                      float(aoget_fits_keyword(hdr, 'pup3.CX'))]
	self._real_cy = [ float(aoget_fits_keyword(hdr, 'pup0.CY')), $
                      float(aoget_fits_keyword(hdr, 'pup1.CY')), $
                      float(aoget_fits_keyword(hdr, 'pup2.CY')), $
                      float(aoget_fits_keyword(hdr, 'pup3.CY'))]
	self._real_side = [ float(aoget_fits_keyword(hdr, 'pup0.SIDE')), $
                        float(aoget_fits_keyword(hdr, 'pup1.SIDE')), $
                        float(aoget_fits_keyword(hdr, 'pup2.SIDE')), $
                        float(aoget_fits_keyword(hdr, 'pup3.SIDE'))]
	self._diffx = [ float(aoget_fits_keyword(hdr, 'pup0.DIFFX')), $
                    float(aoget_fits_keyword(hdr, 'pup1.DIFFX')), $
                    float(aoget_fits_keyword(hdr, 'pup2.DIFFX')), $
                    float(aoget_fits_keyword(hdr, 'pup3.DIFFX'))]
	self._diffy = [ float(aoget_fits_keyword(hdr, 'pup0.DIFFY')), $
                    float(aoget_fits_keyword(hdr, 'pup1.DIFFY')), $
                    float(aoget_fits_keyword(hdr, 'pup2.DIFFY')), $
                    float(aoget_fits_keyword(hdr, 'pup3.DIFFY'))]

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOpupils', 'Represent WFS pupils') then return, 0
    self->addMethodHelp, "indpup()", "pupil indexes  (lonarr)"
    self->addMethodHelp, "single_mask()", "returns the 2d mask of a single pupil"
    self->addMethodHelp, "complete_mask()", "returns the full 4-pupils mask"
    self->addMethodHelp, "nsub()", "number of valid subapertures (long)"
    self->addMethodHelp, "radius()", "nominal radius of pupils (long[4])"
    self->addMethodHelp, "cx()", "nominal x-coord of pupils centers (long[4])"
    self->addMethodHelp, "cy()", "nominal y-coord of pupils centers (long[4])"
    self->addMethodHelp, "real_radius()", "measured radius of pupils (long[4])"
    self->addMethodHelp, "real_cx()", "measured x-coord of pupils centers (long[4])"
    self->addMethodHelp, "real_cy()", "measured y-coord of pupils centers (long[4])"
    self->addMethodHelp, "real_side()", "measured sides between pupils centers (long[4])"
    self->addMethodHelp, "diffx()", "pupils centers error along x-axis (long[4])"
    self->addMethodHelp, "diffy()", "pupils centers error along y-axis (long[4])"
    self->addMethodHelp, "pup_tracknum()", "pupils tracking number (string)"
	return, 1
end

pro aopupils::summary, COMPREHENSIVE=COMPREHENSIVE
    print, string(format='(%"%-30s %d")','number of valid subapertures', self->nsub() )
    print, string(format='(%"%-30s %s")','pupils tracking number', self->pup_tracknum() )
    if keyword_set(COMPREHENSIVE) then begin
	    print, string(format='(%"%-30s [%f,%f,%f,%f]")','nominal radius of pupils [px]', self->radius() )
	    print, string(format='(%"%-30s [%f,%f,%f,%f]")','nominal x-coord of centers [px]', self->cx() )
	    print, string(format='(%"%-30s [%f,%f,%f,%f]")','nominal y-coord of centers [px]', self->cy() )
	    print, string(format='(%"%-30s [%f,%f,%f,%f]")','measured radius of pupils [px]', self->real_radius() )
	    print, string(format='(%"%-30s [%f,%f,%f,%f]")','measured x-coord of centers [px]', self->real_cx() )
	    print, string(format='(%"%-30s [%f,%f,%f,%f]")','measured y-coord of centers [px]', self->real_cy() )
	    print, string(format='(%"%-30s [%f,%f,%f,%f]")','measured sides between pupils centers [px]', self->real_side() )
	    print, string(format='(%"%-30s [%f,%f,%f,%f]")','pupils centers error along x-axis [px]', self->diffx() )
	    print, string(format='(%"%-30s [%f,%f,%f,%f]")','pupils centers error along y-axis [px]', self->diffy() )
	endif
end

function AOpupils::indpup
	return, *(self._indpup)
end

function AOpupils::single_mask
        hdr = *(self._header)
        bin = fix(aoget_fits_keyword(hdr, 'ccd39.BINNING'))
        npix = 80/bin
	frame = intarr(npix, npix)
        frame[ (self->indpup())[*,2]] =1
	return, frame[0:npix/2-1, 0:npix/2-1]
end

function AOpupils::complete_mask
        hdr = *(self._header)
        bin = fix(aoget_fits_keyword(hdr, 'ccd39.BINNING'))
        npix = 80/bin
	frame = intarr(npix, npix)
        frame[ self->indpup()] =1
	return, frame
end

function AOpupils::nsub
	return, self._nsub
end

function AOpupils::radius
	return, self._radius
end

function AOpupils::cx
	return, self._cx
end

function AOpupils::cy
	return, self._cy
end

function AOpupils::real_radius
	return, self._real_radius
end

function AOpupils::real_cx
	return, self._real_cx
end

function AOpupils::real_cy
	return, self._real_cy
end

function AOpupils::real_side
	return, self._real_side
end

function AOpupils::diffx
	return, self._diffx
end

function AOpupils::diffy
	return, self._diffy
end

function AOpupils::pup_tracknum
	return, self._pup_tracknum
end

pro AOpupils::test
    d=self->indpup()
    d=self->nsub()
    d=self->radius()
    d=self->cx()
    d=self->cy()
    d=self->real_radius()
    d=self->real_cx()
    d=self->real_cy()
    d=self->real_side()
    d=self->diffx()
    d=self->diffy()
    d=self->pup_tracknum()
end

function AOpupils::isok, cause=cause
    imok = 1B
    if max(abs([mean(self->diffx()), mean(self->diffy())])) gt 0.2 then begin
        imok*=0B
        cause += ' - Pupils not on target'
    endif
    return, imok
end

pro AOpupils::free
  ;if ptr_valid(self._header) then ptr_free, self._header
  ;if ptr_valid(self._indpup) then ptr_free, self._indpup
end

pro AOpupils::Cleanup
	ptr_free, self._header
	ptr_free, self._indpup
    self->AOhelp::Cleanup
end

pro AOpupils__define
    struct = { AOpupils, $
        _header        : ptr_new()	, $
        _pup_tracknum  : ""			, $
        _indpup		   : ptr_new()  , $
        _nsub		   : 0L			, $
        _radius		   : fltarr(4)	, $
        _cx			   : fltarr(4)	, $
        _cy			   : fltarr(4)	, $
        _real_radius   : fltarr(4)	, $
        _real_cx       : fltarr(4)	, $
        _real_cy       : fltarr(4)	, $
        _real_side     : fltarr(4)	, $
        _diffx         : fltarr(4)	, $
        _diffy         : fltarr(4)	, $
        INHERITS    AOhelp  $
    }
end
