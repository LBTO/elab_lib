
;+
; aoluci object initialization
;-

function aoluci::Init, root_obj, psf_fname, dark_fname
    if psf_fname eq '' then return,0

    if not file_test(psf_fname) then begin
        message, psf_fname + ' not found', /info
        return,0
    endif
    fitsheader = headfits(psf_fname, /SILENT, errmsg=errmsg)
    if errmsg ne ''  then message, psf_fname+ ': '+ errmsg, /info

	;Camera Temperature (K)
	self._luci_temp = float(aoget_fits_keyword(fitsheader, 'TEMPI0_S'))

    ;Camera
    self._camera_name= strtrim(aoget_fits_keyword(fitsheader, 'CAMERA'),2)

    ; Pixelscale:
    pixelscale = 0.0149

    ; Detect filter:
    ;self._filter_name = strtrim(aoget_fits_keyword(fitsheader, 'FILTERS'),2)
    self._filter_name = strtrim(aoget_fits_keyword(fitsheader, 'HIERARCH LBTO LUCI INS FILTERS NAMES'),2)
    valid_filt_number = 1B
    CASE strtrim(self._filter_name,2) OF
    	'clear J':        lambda = 1.25e-6
    	'clear H2':       lambda = 2.124e-6
    	'clear FeII':     lambda = 1.646e-6
    	'clear H':        lambda = 1.646e-6
    	'clear Pbet':     lambda = 1.283e-6
    	'clear OH1060':   lambda = 1.060e-6
        'clear Br_gam':   lambda = 2.124e-6
        'clear P_beta':   lambda = 1.28e-6
        'clear Ks':       lambda = 2.15e-6
    	'J clear':        lambda = 1.25e-6
    	'H2 clear':       lambda = 2.124e-6
    	'FeII clear':     lambda = 1.646e-6
    	'FeII H':         lambda = 1.646e-6
    	'HeI clear':      lambda = 1.088e-6
    	'Pbet clear':     lambda = 1.283e-6
    	'OH1060 clear':   lambda = 1.060e-6
        'Br_gam clear':   lambda = 2.124e-6
        'P_beta clear':   lambda = 1.28e-6
     	'ND2 J':        lambda = 1.25e-6
    	'ND2 H2':       lambda = 2.124e-6
    	'ND2 FeII':     lambda = 1.646e-6
    	'ND2 H':        lambda = 1.646e-6
    	'ND2 Pbet':     lambda = 1.283e-6
    	'ND2 OH1060':   lambda = 1.060e-6
        'ND2 Br_gam':   lambda = 2.124e-6
        'ND2 P_beta':   lambda = 1.28e-6
        'ND2 Ks':       lambda = 2.15e-6
    	'J ND2':        lambda = 1.25e-6
    	'H2 ND2':       lambda = 2.124e-6
    	'FeII ND2':     lambda = 1.646e-6
    	'FeII H':       lambda = 1.646e-6
    	'HeI ND2':      lambda = 1.088e-6
    	'Pbet ND2':     lambda = 1.283e-6
    	'OH1060 ND2':   lambda = 1.060e-6
        'Br_gam ND2':   lambda = 2.124e-6
        'P_beta ND2':   lambda = 1.28e-6
    else: begin
     		lambda = !VALUES.F_NAN
     		msg_temp = 'Unknown luci filter <'+self._filter_name+'>'
            message, msg_temp, /info
	        self._luci_err_msg += ' - ' + msg_temp
    		valid_filt_number = 0B
           end
    ENDCASE

    ; Exposure time:
	exptime = float(aoget_fits_keyword(fitsheader, 'EXPTIME'))	;in seconds
	valid_exptime = 1B
	if exptime eq 0 then begin
		msg_temp = 'Unknown luci exposure time'
		message, msg_temp, /info
        self._luci_err_msg += ' - ' + msg_temp
        valid_exptime = 0B
    endif

    ;Framerate:
	framerate =  1./exptime  ; TODO 

    ; Frame size
    frame_w  = long(aoget_fits_keyword(fitsheader, 'NAXIS1'))
    frame_h  = long(aoget_fits_keyword(fitsheader, 'NAXIS2'))

    ;ra  = (root_obj->tel())->ra()
    ;dec = (root_obj->tel())->dec()
    ;stages_x = ((root_obj->wfs_status())->stages())[0]
    ;stages_y = ((root_obj->wfs_status())->stages())[1]
    ;thisJulday = (root_obj->obj_tracknum())->JulDay()
    ;dark = self->find_dark_from_tn(thisJulday, dark_subdir, exptime, filter_tag, frame_w, frame_h, ra, dec, stages_x, stages_y, err_msg=dark_err_msg)
;stop
	;Dark Frame:
    if obj_valid(root_obj->wfs_status()) then begin
    dark_subdir = ['wfs_calib_'+(root_obj->wfs_status())->wunit(),'luci','backgrounds','bin1']
    if (n_elements(dark_fname) eq 0) then begin
    	if valid_exptime and valid_filt_number then begin
    		thisJulday = (root_obj->obj_tracknum())->JulDay()
    		full_dark_fname = self->find_dark(thisJulday, dark_subdir, exptime, self._filter_name, frame_w, frame_h, err_msg=dark_err_msg)
   			if strtrim(dark_err_msg,2) ne '' then self._luci_err_msg += dark_err_msg
   		endif else begin
			msg_temp = 'luci dark cannot be searched: ('
			if not valid_exptime then msg_temp += 'exptime '
			if not valid_filt_number then msg_temp += 'filter '
			msg_temp += 'unknown)'
			message, msg_temp, /info
        	self._luci_err_msg += ' - ' + msg_temp
        endelse
    endif else begin
		full_dark_fname = filepath(root=ao_datadir(), sub=dark_subdir,  dark_fname)
		if not file_test(full_dark_fname) then begin
			msg_temp = 'Overidden luci dark file does not exist'
			message, msg_temp, /info
        	self._luci_err_msg += ' - ' + msg_temp
        endif
	endelse


	;Badpixelmap filename:
	badpixelmap_fname = filepath(root=ao_datadir(), sub=dark_subdir, 'badpixelmap.sav')
    endif

    store_radix = filepath(root=root_obj->elabdir(), 'luci')

	; initialize PSF object
    if not self->AOscientificimage::Init(root_obj, psf_fname, full_dark_fname, pixelscale, lambda, exptime, framerate, $
    	            0B, badpixelmap_fname=badpixelmap_fname, store_radix=store_radix, recompute=root_obj->recompute()) then return,0

    ; Override diameter & obstruction + dependent parameters
    self._pupdiam = 7.8
    self._oc = 0.314
    self._pixelscale_lD = self._pixelscale / ((self->lambda()/self->pupdiam())/4.848d-6)
    nsup = 40.  ;maximum radius to ensure all star light is in.
    self._object_size = nsup * self->lambda() / self->pupdiam() / 4.85e-6 / self._pixelscale

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('aoluci', 'luci image') then return, 0
    self->addMethodHelp, "temp()", "luci temperature (K)"
    self->addMethodHelp, "filter_name()", "filter name ['empty J', ...]"
    self->addMethodHelp, "camera()", "camera name ['N30 Camera', ...]"
    self->AOscientificimage::addHelp, self

    return, 1
end

pro aoluci::summary
    self->AOscientificimage::summary
    print, string(format='(%"%-30s %s")','Camera name', self->camera() )
    print, string(format='(%"%-30s %s")','Filter name', self->filter_name() )
    print, string(format='(%"%-30s %f")','Temperature (K)', self->temp() )
end


;Searches a luci dark closest in time to the luci image with the same set of parameters
;------------------------------------------------------------------------------------------
function aoluci::find_dark, thisJulday, dark_subdir, exptime, filter_tag, frame_w, frame_h, err_msg=err_msg

	err_msg = ""

	; Verify that the dark and the psf image were taken with the same exposure time!
	all_darks_search = filepath(root=ao_datadir(), sub=dark_subdir,  '*_cube.fits')
	all_darks_fname  = file_search(all_darks_search, count=ndarks)

	if ndarks gt 0 then begin
		all_darks_julday = dblarr(ndarks)
		for ii=0, ndarks-1 do begin
			dark_tracknum = strmid(file_basename(all_darks_fname[ii]), 0, 15)
   			y  = fix(strmid(dark_tracknum,  0, 4))
   			m  = fix(strmid(dark_tracknum,  4, 2))
   			d  = fix(strmid(dark_tracknum,  6, 2))
   			hh = fix(strmid(dark_tracknum,  9, 2))
   			mm = fix(strmid(dark_tracknum, 11, 2))
   			ss = fix(strmid(dark_tracknum, 13, 2))
   			all_darks_julday[ii] = julday(m, d, y, hh, mm, ss)
		endfor
		idx_closest = sort(abs(all_darks_julday - thisjulday))

		dark_found = 0B & dd=0
		while (dark_found eq 0B) and (dd lt ndarks) do begin
			;Retrieve header info of averaged dark frame because the file with the cube didn't have any info saved in the header...
			closest_av_dark_fname = filepath(root=ao_datadir(), sub=dark_subdir, file_basename(all_darks_fname[idx_closest[dd]], '_cube.fits'))
			dark_header = headfits(closest_av_dark_fname)
			dark_exptime = float(aoget_fits_keyword(dark_header, 'EXPTIME'))	;in seconds
			dark_filter_tag = aoget_fits_keyword(dark_header, 'HIERARCH LBTO LUCI INS FILTERS NAMES')
			dark_frame_w = long(aoget_fits_keyword(dark_header, 'NAXIS1'))
			dark_frame_h = long(aoget_fits_keyword(dark_header, 'NAXIS2'))
			if (dark_exptime eq exptime) and (strtrim(filter_tag,2) eq strtrim(dark_filter_tag,2)) and  $
			   (dark_frame_w eq frame_w) and (dark_frame_h eq frame_h) then dark_found=1B else dd+=1
		endwhile
		if dark_found then begin
			dark_fname = all_darks_fname[idx_closest[dd]]
		 	time_elapsed = abs(all_darks_julday[idx_closest[dd]] - thisjulday)
		 	max_time_elapsed = julday(01, 01, 2010, 01, 00, 00) -  julday(01, 01, 2010, 00, 00, 00)
		 	if time_elapsed gt max_time_elapsed then begin
		 		msg_temp = 'Warning: Selected luci dark +'+strtrim(round(time_elapsed/max_time_elapsed),2)+'h old!'
		 		message, msg_temp, /info
		 		err_msg += ' - ' + msg_temp
		 	endif
		endif else begin
			msg_temp = 'No compatible luci dark found (i.e. same exposure time or filter or dimensions)'
			message, msg_temp, /info
			err_msg += ' - ' + msg_temp
			return, ""
		endelse
	endif else begin
		msg_temp = 'No luci darks found matching '+all_darks_search
		message, msg_temp, /info
 		err_msg += ' - ' + msg_temp
		return, ""
	endelse

	return, dark_fname
end

;function aoluci::find_dark_from_tn, thisJulday, dark_subdir, exptime, filter_tag, frame_w, frame_h, ra, dec, stages_x, stages_y, err_msg=err_msg
;     set    = obj_new('aodataset', from=thisJulday-1d/48, to=thisJulday+1d/48) ; set of tn close in time to this
;     setl   = set->where('meas_type', 'eq', 'LOOP') ; only LOOP, no AutoGain, SlopesNull etc
;     xxx = 1d/60
;     setra  = setl->where('tel.ra',   'between', [ra-xxx*15, ra+xxx*15])  ; similar telescope pointing
;     setdec = setra->where('tel.dec', 'between', [dec-xxx, dec+xxx])  ; similar telescope pointing
;
;    ; find
;
;    return, setdec
;end

function aoluci::camera
	return, self._camera_name
end


function aoluci::filter_name
	return, self._filter_name
end

function aoluci::temp
	return, self._luci_temp
end

function aoluci::linearize, frame

        return, frame + 4.218*1e-6*(frame^2)
end
         

;Returns the error messages
;-----------------------------------------------------
function aoluci::isok, cause=cause
	isok=1B
    isok *= self->AOscientificimage::isok(cause=cause)
	if strtrim(self._luci_err_msg,2) ne '' then begin
		isok*=0B
		cause += self._luci_err_msg
	endif
	return, isok
end

pro aoluci__define
    struct = { aoluci				, $
        _camera_name         : ""       , $
    	_luci_temp           : 0.0      , $
        _filter_name         : ""       , $
        _luci_err_msg        : ""       , $
        INHERITS  AOscientificimage     , $
        INHERITS  AOhelp                  $
    }
end
