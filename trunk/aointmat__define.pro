
;+
;
;-

function AOintmat::Init, fname
    self._im_file  = fname
    full_fname = ao_datadir()+path_sep()+self->fname()
    header = headfits(full_fname ,/SILENT, errmsg=errmsg)
    if errmsg ne '' then message, full_fname+ ': '+ errmsg, /info

    self._basis = strtrim(aoget_fits_keyword(header, 'M2C'))
    self._nmodes  = -1
    self._nslopes = -1
    self._wfs_status = obj_new('AOwfs_status', self, full_fname)

	;modal disturbance sequence
    md_fn = strtrim(aoget_fits_keyword(header, 'M_DIST_F'))
    md_fn  = file_basename(md_fn)
    md_dir = file_dirname(self->fname())
    md_dir = strmid(md_dir, 0, strpos(md_dir,'RECs'))+'disturb'
    self._modal_dist_fname = md_dir+path_sep()+md_fn

    self._im_file_fitsheader = ptr_new(header, /no_copy)

	;retrieve data from header of modal disturbance sequence
    full_fname = ao_datadir()+path_sep()+self->modal_dist_fname()
    header = headfits(full_fname ,/SILENT, errmsg=errmsg)
    if errmsg ne '' then message, full_fname+ ': '+ errmsg, /info

	md_fn = strtrim(aoget_fits_keyword(header, 'PP_AMP_F'))
    md_fn  = file_basename(md_fn)
    md_dir = file_dirname(self->fname())
    md_dir = strmid(md_dir, 0, strpos(md_dir,'RECs'))+'modesAmp'
    self._modalamp_fname = md_dir+path_sep()+md_fn


    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('AOintmat', 'Represent an interaction matrix (IM)') then return, 0
    self->addMethodHelp, "fname()",   "fitsfile name (string)"
    self->addMethodHelp, "header()",     "header of fitsfile (strarr)"
    self->addMethodHelp, "im()", 	"interaction matrix"
    self->addMethodHelp, "im2d()", "interaction matrix (signals remapped in 2D)"
    self->addMethodHelp, "visu_im2d [,mode_num_idx ,ncol=ncol ,nrows=nrows ,ct=ct ,zoom=zoom]", "displays IM in 2D"
    self->addMethodHelp, "nmodes()", "number of non-null columns in IM"
    self->addMethodHelp, "modes_idx()", "index vector of non-null columns in IM"
    self->addMethodHelp, "nslopes()", "number of non-null rows in IM"
    self->addMethodHelp, "slopes_idx()", "index vector of non-null rows in IM"
    self->addMethodHelp, "basis()", "modal basis calibrated"
    self->addMethodHelp, "wfs_status()", "reference to wfs_status object"
    self->addMethodHelp, "modalamp_fname()", "fitsfile name of modal amplitudes (string)"
    self->addMethodHelp, "modalamp()", "modal amplitudes (fltarr)"
    self->addMethodHelp, "modal_dist_fname()", "fitsfile name of modal disturbance sequence (fltarr)"
    self->addMethodHelp, "modal_dist()", "modal disturbance sequence (fltarr)"
    return, 1
end

function AOintmat::fname
    return, self._im_file
end

function AOintmat::header
    if ptr_valid(self._im_file_fitsheader) then return, *(self._im_file_fitsheader) else return, ""
end

function AOintmat::im
    im = readfits(ao_datadir()+path_sep()+self->fname(), /SILENT)
    if not ptr_valid(self._modes_idx) then begin
    	self._modes_idx = ptr_new(where(total(im,2) ne 0, t_nmodes), /no_copy)
    	if t_nmodes eq 0 then message, 'Null im matrix '+self->fname()
    	self._nmodes = t_nmodes
    endif
    if not ptr_valid(self._slopes_idx) then begin
       	self._slopes_idx = ptr_new(where(total(im,1) ne 0, t_nslopes), /no_copy)
    	if t_nslopes eq 0 then message, 'Null im matrix '+self->fname()
    	self._nslopes = t_nslopes
    endif
    return, im
end

; returns Sx in im matrix
function AOintmat::sx, mode_num_idx
	if n_params() eq 0 then mode_num_idx = lindgen(self->nmodes())
	im = self->im()
	nsub = ((self->wfs_status())->pupils())->nsub()
	sx = im[mode_num_idx,*]
	sx = sx[*,0:nsub*2-1]
	sx = sx[*,0:*:2]
	return, sx
end

; returns Sy in im matrix
function AOintmat::sy, mode_num_idx
	if n_params() eq 0 then mode_num_idx = lindgen(self->nmodes())
	im = self->im()
	nsub = ((self->wfs_status())->pupils())->nsub()
	sy = im[mode_num_idx,*]
	sy = sy[*,0:nsub*2-1]
	sy = sy[*,1:*:2]
	return, sy
end


; number of non-null columns in im matrix
function AOintmat::nmodes
    if (self._nmodes eq -1) then r=self->im()
    return, self._nmodes
end

; indexes of non-null columns in im matrix (modes)
function AOintmat::modes_idx
    if not ptr_valid(self._modes_idx) then r=self->im()
    if (PTR_VALID(self._modes_idx)) THEN return, *(self._modes_idx) else return, 0d
end

; number of non-null rows in im matrix
function AOintmat::nslopes
    if (self._nslopes eq -1) then r=self->im()
    return, self._nslopes
end

; indexes of non-null rows in im matrix (slopes)
function AOintmat::slopes_idx
    if not ptr_valid(self._slopes_idx) then r=self->im()
    if (PTR_VALID(self._slopes_idx)) THEN return, *(self._slopes_idx) else return, 0d
end

; modal basis calibrated
function AOintmat::basis
	return, self._basis
end

; return wfs_status object
function AOintmat::wfs_status
    IF (OBJ_VALID(self._wfs_status)) THEN return, self._wfs_status else return, obj_new()
end

; return filename of modal disturbance sequence used for the acquistion.
function AOintmat::modal_dist_fname
	return, self._modal_dist_fname
end

; return modal disturbance sequence
function AOintmat::modal_dist
    md = readfits(ao_datadir()+path_sep()+self->modal_dist_fname(), /SILENT)
	return, md
end

; return filename of modal amplitudes
function AOintmat::modalamp_fname
	return, self._modalamp_fname
end

; return modal amplitudes
function AOintmat::modalamp
	ma = readfits(ao_datadir()+path_sep()+self->modalamp_fname(), /SILENT)
	return, ma
end

; return remapped IM in 2D for signal visualization
function AOintmat::im2d
	if not ptr_valid(self._im2d_cube) then begin
		mypup = 0	;use this pupil info to remap signals
		sx = self->sx()
		sy = self->sy()
		indpup = ((self->wfs_status())->pupils())->indpup()
		nsub   = ((self->wfs_status())->pupils())->nsub()
		fr_sz =80/((self->wfs_status())->ccd39())->binning()	;pixels

		cx  = (((self->wfs_status())->pupils())->cx())[mypup]
		cy  = (((self->wfs_status())->pupils())->cy())[mypup]
		rad = (((self->wfs_status())->pupils())->radius())[mypup]
		xr = [floor(cx-rad),ceil(cx+rad)]
		yr = [floor(cy-rad),ceil(cy+rad)]
		im2d_w = xr[1]-xr[0]+1
		im2d_h = yr[1]-yr[0]+1

		s2d = fltarr(fr_sz,fr_sz)
		im_2d = fltarr(im2d_w*2,im2d_h,self->nmodes())
		for ii=0, self->nmodes()-1 do begin
			s2d[indpup[*,mypup]] = sx[ii,*]
			s2d_tmpA = s2d[xr[0]:xr[1],yr[0]:yr[1]]
			s2d[indpup[*,mypup]] = sy[ii,*]
			s2d_tmpB = s2d[xr[0]:xr[1],yr[0]:yr[1]]
			im_2d[*,*,ii] = [s2d_tmpA,s2d_tmpB]
		endfor
		self._im2d_cube = ptr_new(im_2d, /no_copy)
	endif
	return, *(self._im2d_cube)
end

pro AOintmat::visu_im2d, mode_num_idx, ncol=ncol, nrows=nrows, ct=ct, zoom=zoom
	if n_params() eq 0 then mode_num_idx = lindgen(self->nmodes())
	if not keyword_set(ct) then ct=3
	if not keyword_set(zoom) then zoom=1

	nsig = n_elements(mode_num_idx)
	imcube = self->im2d()
	sz = size(imcube, /dim)

	if nsig eq 1 then begin
		ncol=1
		nrows=1
	endif
	if ( (not keyword_set(nrows)) AND (not keyword_set(ncol)) ) then ncol=nsig/3
	if not keyword_set(ncol)  then ncol  = ceil(nsig/float(nrows))
	if not keyword_set(nrows) then nrows = ceil(nsig/float(ncol))
	npag = ceil(nsig/(float(nrows)*float(ncol)))

	if (ct lt 0) or (ct gt 40) then ct=3
	loadct,ct
	window,/free,xsize=sz[0]*ncol*zoom,ysize=sz[1]*nrows*zoom
    erase,0
    for ii=0, nsig-1 do tvscl,rebin(imcube[*,*,mode_num_idx[ii]],sz[0]*zoom,sz[1]*zoom,/SAMPLE),ii
end

pro AOintmat::Cleanup
    ptr_free, self._modes_idx
    ptr_free, self._slopes_idx
    ptr_free, self._im_file_fitsheader
    ptr_free, self._im2d_cube
    obj_destroy, self._wfs_status
    self->AOhelp::Cleanup
end


pro AOintmat__define
    struct = { AOintmat, $
        _im_file                          : ""			, $
        _im_file_fitsheader               : ptr_new()	, $
        _basis							  : ""			, $
        _nmodes                           : -1L			, $
        _modes_idx                        : ptr_new()	, $
        _nslopes						  : -1L			, $
        _slopes_idx						  : ptr_new()	, $
        _wfs_status						  : obj_new()	, $
        _im2d_cube						  : ptr_new()	, $
        _modal_dist_fname				  : ''			, $
        _modalamp_fname					  : ''			, $
        INHERITS AOhelp $
    }
end