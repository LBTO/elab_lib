function AOintmat_meas::Init

    im_header = self->header()


	;DISTURBANCE DATA:
	;------------------------------------------------------
    self._avg_frames = long(aoget_fits_keyword(im_header, 'AVG_FRM'))

	;modal disturbance sequence
    md_fn = strtrim(aoget_fits_keyword(im_header, 'M_DIST_F'))
    md_fn  = file_basename(md_fn)
    md_dir = file_dirname(self->fname())
    md_dir = strmid(md_dir, 0, strpos(md_dir,'RECs'))+'disturb'
    self._modal_dist_fname = md_dir+path_sep()+md_fn

	;acquisition dir (find total number of parts in which acq is split)
	acq_dir = strtrim(aoget_fits_keyword(im_header, 'IM_ACQ_D'),2)
	acq_params_fn = file_search(filepath(root=ao_datadir(), sub=['adsec_calib',acq_dir], 'SwitchBCU*params.sav'), count=total_parts)
	if file_test(acq_params_fn[0]) then begin
		restore, acq_params_fn[0]
		if n_elements(params) ne 0 then	frames_len = params.frames_len else frames_len = 0
	endif else frames_len = 0

	;retrieve data from header of modal disturbance sequence
    full_fname = ao_datadir()+path_sep()+self->modal_dist_fname()
    header = headfits(full_fname ,/SILENT, errmsg=errmsg)
    if errmsg ne '' then message, full_fname+ ': '+ errmsg, /info
	self._dist_type = strtrim(aoget_fits_keyword(header, 'IM_TYPE'),2)

	;PUSH-PULL: compute number of PP cycles per mode:
	meas_cycles = long(aoget_fits_keyword(header, 'PP_CYCLE'))
	dist_size = long(aoget_fits_keyword(header, 'NAXIS1'))
	self._npp_per_mode = total_parts * meas_cycles / (dist_size/4000L) * (frames_len/4000L)

	md_fn = strtrim(aoget_fits_keyword(header, 'PP_AMP_F'),2)
    md_fn  = file_basename(md_fn)
    md_dir = file_dirname(self->fname())
    md_dir = strmid(md_dir, 0, strpos(md_dir,'RECs'))+'modesAmp'
    self._modalamp_fname = md_dir+path_sep()+md_fn

	;Amplitude envelope
	md_dir = file_dirname(self->fname())
	md_dir = strmid(md_dir, 0, strpos(md_dir,'RECs'))
	self._amp_envelope_fname = md_dir+'amp_envelope.fits'


	;ACQUISITION OF INTERACTION MATRIX IN CLOSED-LOOP ??
	;--------------------------------------------------------------------------

	rec_file  = ''
	gain_file = ''
	if file_test(acq_params_fn[0]) then begin
		restore, acq_params_fn[0]
		if n_elements(params) ne 0  then begin
			if tag_exist(params,'recfile') then rec_file  = params.recfile
			if tag_exist(params,'gainfile') then gain_file = params.gainfile
		endif
	endif

	if rec_file ne '' then begin
		self._clcalib = 1B
		extr = strsplit(rec_file, '/', /extr)
        idx = (where(stregex(extr, 'adsec*', /BOOLEAN),count))[0]
        if count gt 0 then rec_file  = 'adsec_calib/'+strjoin(extr[idx+1:*], '/')
       	pos = strpos(rec_file, 'Rec_', /REVERSE_SEARCH)
		intmat_file = strmid(rec_file, 0, pos) +'Intmat_'+strmid(rec_file, pos+4)
		self._clcalib_intmat = getintmat( intmat_file )
    endif

	if gain_file ne '' then begin
		extr = strsplit(gain_file, '/', /extr)
        idx = (where(stregex(extr, 'adsec*', /BOOLEAN),count))[0]
        if count gt 0 then self._clcalib_gain_fname  = 'adsec_calib/'+strjoin(extr[idx+1:*], '/')
    endif

	return,1
end

pro AOintmat_meas::addHelp, obj
    obj->addMethodHelp, "modalamp_fname()", "fitsfile name of modal amplitudes (string)"
    obj->addMethodHelp, "modalamp()", "modal amplitudes (fltarr)"
    obj->addMethodHelp, "modal_dist_fname()", "fitsfile name of modal disturbance sequence (fltarr)"
    obj->addMethodHelp, "modal_dist()", "modal disturbance sequence (fltarr)"
    obj->addMethodHelp, "dist_type()", "PUSH-PULL or SINUS (string)"
    obj->addMethodHelp, "npp()", "number of push-pull cycles per mode (long)"
    obj->addMethodHelp, "avg_frames()", "number of averaged frames in push movement (long)"
    obj->addMethodHelp, "amp_envelope_fname()", "fitsfile name of modal amplitude envelope (string)"
    obj->addMethodHelp, "amp_envelope()", "modal amplitude envelope (fltarr)"
    obj->addMethodHelp, "clcalib()", "Equals 1B if IM acquisition in closed-loop (boolean)"
    obj->addMethodHelp, "clcalib_gain_fname()", "gain file used during IM acquisition in closed-loop (string)"
    obj->addMethodHelp, "clcalib_intmat()", "reference to IM object used in closed-loop IM acq (AOintmat)"
end


; return filename of modal disturbance sequence used for the acquistion.
function AOintmat_meas::modal_dist_fname
	return, self._modal_dist_fname
end

; return modal disturbance sequence
function AOintmat_meas::modal_dist
    md = readfits(ao_datadir()+path_sep()+self->modal_dist_fname(), /SILENT)
	return, md
end

; return disturbance type (PUSH-PULL, OR SINUS)
function AOintmat_meas::dist_type
	return, self._dist_type
end

function AOintmat_meas::npp
	return, self._npp_per_mode
end

function AOintmat_meas::avg_frames
	return, self._avg_frames
end

; return filename of modal amplitudes
function AOintmat_meas::modalamp_fname
	return, self._modalamp_fname
end

; return modal amplitudes
function AOintmat_meas::modalamp
	ma = readfits(ao_datadir()+path_sep()+self->modalamp_fname(), /SILENT)
	return, ma
end

; return filename of amp envelope
function AOintmat_meas::amp_envelope_fname
	return, self._amp_envelope_fname
end

; return amp envelope
function AOintmat_meas::amp_envelope
	me = readfits(ao_datadir()+path_sep()+self->amp_envelope_fname(), /SILENT)
	return, me
end

function AOintmat_meas::clcalib
	return, self._clcalib
end

function AOintmat_meas::clcalib_gain_fname
	return, self._clcalib_gain_fname
end

function AOintmat_meas::clcalib_intmat
    IF (OBJ_VALID(self._clcalib_intmat)) THEN return, self._clcalib_intmat else return, obj_new()
end


pro AOintmat::calib_history
	imobj = self
	print, format='(A-8,A-8,A-8,A-8,A-8,A-30,A30)', 'OL/CL', 'basis', 'nmodes', 'ncycles', 'nups', 'IM file name', 'gain file'
	while obj_valid(imobj) do begin
		print, format='(A-8,A-8,I8,I8,I8,A30,A30)', imobj->clcalib() ? 'CL' : 'OL', imobj->basis(), imobj->nmodes(), $
			imobj->npp(), imobj->avg_frames(), $
			file_basename(imobj->fname()), file_basename(imobj->clcalib_gain_fname())
		imobj = imobj->clcalib_intmat()
	endwhile
end


pro AOintmat_meas__define
    struct = { AOintmat_meas							, $
        _modal_dist_fname				  : ''			, $
        _dist_type						  : ''			, $
        _npp_per_mode					  : ''			, $
        _avg_frames						  : ''			, $
        _modalamp_fname					  : ''			, $
        _amp_envelope_fname				  : ''			, $
        _clcalib				  		  : 0B			, $
        _clcalib_intmat					  : obj_new()	, $
        _clcalib_gain_fname				  : ''			  $
    }
end