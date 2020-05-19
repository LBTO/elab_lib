
;+
; aopisces object initialization
;-

function aopisces::Init, root_obj, psf_fname, dark_fname
    if psf_fname eq '' then return,0

    if not file_test(psf_fname) then begin
        message, psf_fname + ' not found', /info
        return,0
    endif
    fitsheader = headfits(psf_fname, /SILENT, errmsg=errmsg)
    if errmsg ne ''  then message, psf_fname+ ': '+ errmsg, /info

	;Camera Temperature (K)
	self._pisces_temp = float(aoget_fits_keyword(fitsheader, 'CAMTEM'))
    if self._pisces_temp gt 260. then begin
    	msg_temp = 'Warning: pisces temperature > 260K'
        message, msg_temp, /info
	    self._pisces_err_msg += ' - ' + msg_temp
	endif


    ; Pixelscale:
    pixelscale = 0.0182

    ; Detect filter:
    self._filter_name = strtrim(aoget_fits_keyword(fitsheader, 'FILTER'),2)
    valid_filt_number = 1B
    CASE strtrim(self._filter_name,2) OF
                                            ;0:2.14
        'H2 2.122 um':  lambda = 2.122e-6   ;1:H2 2.122 um
        'open':         lambda = 1e-6       ;2:OPEN
                                            ;3:dark
    	'J':            lambda = 1.25e-6	;4:J
        'Ks':           lambda = 2.1e-6     ;5:ks
    	'H':            lambda = 1.65e-6	;6:H
		'Br-g 2.166 um': lambda= 2.16e-6    ;7:BrGamma
    	'FeII 1.64 um': lambda = 1.64e-6	;8:FeII
                                            ;9:2.086
     else: begin
     		;lambda = 1.
     		lambda = !VALUES.F_NAN
     		msg_temp = 'Unknown pisces filter <'+self._filter_name+'>'
            message, msg_temp, /info
	        self._pisces_err_msg += ' - ' + msg_temp
    		valid_filt_number = 0B
           end
    ENDCASE


    ; Exposure time:
	exptime = float(aoget_fits_keyword(fitsheader, 'EXPTIME'))	;in seconds
	valid_exptime = 1B
	if exptime eq 0 then begin
		msg_temp = 'Unknown pisces exposure time'
		message, msg_temp, /info
        self._pisces_err_msg += ' - ' + msg_temp
        valid_exptime = 0B
    endif


    ;Framerate:
	framerate =  1./(exptime + 7.)  ; TODO at fullframe roughly 7s of overhead for each exposure !!!!

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
    dark_subdir = ['wfs_calib_'+(root_obj->wfs_status())->wunit(),'pisces','backgrounds','bin1']
    if (n_elements(dark_fname) eq 0) then begin
    	if valid_exptime and valid_filt_number then begin
    		thisJulday = (root_obj->obj_tracknum())->JulDay()
    		full_dark_fname = self->find_dark(thisJulday, dark_subdir, exptime, self._filter_name, frame_w, frame_h, err_msg=dark_err_msg)
   			if strtrim(dark_err_msg,2) ne '' then self._pisces_err_msg += dark_err_msg
   		endif else begin
			msg_temp = 'pisces dark cannot be searched: ('
			if not valid_exptime then msg_temp += 'exptime '
			if not valid_filt_number then msg_temp += 'filter '
			msg_temp += 'unknown)'
			message, msg_temp, /info
        	self._pisces_err_msg += ' - ' + msg_temp
        endelse
    endif else begin
		full_dark_fname = filepath(root=ao_datadir(), sub=dark_subdir,  dark_fname)
;		full_dark_fname =  dark_fname
		if not file_test(full_dark_fname) then begin
			msg_temp = 'Overidden pisces dark file does not exist'
			message, msg_temp, /info
        	self._pisces_err_msg += ' - ' + msg_temp
        endif
	endelse


	;Badpixelmap filename:
	badpixelmap_fname = filepath(root=ao_datadir(), sub=dark_subdir, 'badpixelmap.sav')
    endif

    store_radix = filepath(root=root_obj->elabdir(), 'pisces')

	; initialize PSF object
    if not self->AOscientificimage::Init(root_obj, psf_fname, full_dark_fname, pixelscale, lambda, exptime, framerate, $
    	            badpixelmap_fname=badpixelmap_fname, store_radix=store_radix, recompute=root_obj->recompute()) then return,0

    ; initialize help object and add methods and leafs
    if not self->AOhelp::Init('aopisces', 'pisces image') then return, 0
    self->addMethodHelp, "temp()", "pisces temperature (K)"
    self->addMethodHelp, "filter_name()", "filter name ['J', 'H', 'FeII 1.64um', 'Ks', 'H2 2.122 um']"
    self->AOscientificimage::addHelp, self

    return, 1
end

pro aopisces::summary
    self->AOscientificimage::summary
    print, string(format='(%"%-30s %s")','Filter name', self->filter_name() )
    print, string(format='(%"%-30s %f")','Temperature (K)', self->temp() )
end


;Searches a pisces dark closest in time to the pisces image with the same set of parameters
;------------------------------------------------------------------------------------------
function aopisces::find_dark, thisJulday, dark_subdir, exptime, filter_tag, frame_w, frame_h, err_msg=err_msg

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
			dark_filter_tag = aoget_fits_keyword(dark_header, 'FILTER')
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
		 		msg_temp = 'Warning: Selected pisces dark +'+strtrim(round(time_elapsed/max_time_elapsed),2)+'h old!'
		 		message, msg_temp, /info
		 		err_msg += ' - ' + msg_temp
		 	endif
		endif else begin
			msg_temp = 'No compatible pisces dark found (i.e. same exposure time or filter or dimensions)'
			message, msg_temp, /info
			err_msg += ' - ' + msg_temp
			return, ""
		endelse
	endif else begin
		msg_temp = 'No pisces darks found matching '+all_darks_search
		message, msg_temp, /info
 		err_msg += ' - ' + msg_temp
		return, ""
	endelse

	return, dark_fname
end

;function aopisces::find_dark_from_tn, thisJulday, dark_subdir, exptime, filter_tag, frame_w, frame_h, ra, dec, stages_x, stages_y, err_msg=err_msg
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


function aopisces::filter_name
	return, self._filter_name
end

function aopisces::temp
	return, self._pisces_temp
end

;Returns the error messages
;-----------------------------------------------------
function aopisces::isok, cause=cause
	isok=1B
    isok *= self->AOscientificimage::isok(cause=cause)
	if strtrim(self._pisces_err_msg,2) ne '' then begin
		isok*=0B
		cause += self._pisces_err_msg
	endif
	return, isok
end

pro aopisces__define
    struct = { aopisces				, $
    	_pisces_temp        : 0.0      , $
        _filter_name         : ""       , $
        _pisces_err_msg     : ""       , $
        INHERITS  AOscientificimage     , $
        INHERITS  AOhelp                  $
    }
end
