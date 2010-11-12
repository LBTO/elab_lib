
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
            message, 'File not found:'+fname
            return, 0
        endif
    endfor
    fname=filepath(root=pups_path, 'pupdata.txt')
	if not file_test(fname) then begin 
        message, 'File not found:'+fname
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

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOpupils', 'Represent WFS pupils') then return, 0
    self->addMethodHelp, "indpup()", "pupil indexes  (lonarr)"
    self->addMethodHelp, "nsub()", "number of valid subapertures (long)"
    self->addMethodHelp, "radius()", "radius of pupils (long[4])"
    self->addMethodHelp, "cx()", "x-coord of pupils centers (long[4])"
    self->addMethodHelp, "cy()", "y-coord of pupils centers (long[4])"
    self->addMethodHelp, "pup_tracknum()", "pupils tracking number (string)"

	return, 1
end

function AOpupils::indpup
	return, *(self._indpup)
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

function AOpupils::pup_tracknum
	return, self._pup_tracknum
end

pro AOpupils::free
  if ptr_valid(self._header) then ptr_free, self._header  
  if ptr_valid(self._indpup) then ptr_free, self._indpup
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
        INHERITS    AOhelp  $
    }
end
